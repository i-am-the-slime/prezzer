module Main where

import Prelude

import AceComponent (AceOutput(..), AceQuery(..), aceComponent) as Ace
import AceComponent (AceOutput, AceQuery)
import Affjax (ResponseFormatError)
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (find)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Foldable (foldMap, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Data.Traversable (for_)
import Data.Variant (SProxy(..), Variant, case_, inj, match, on, default)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (renderForeignError)
import Halogen (PropName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Simple.JSON (class ReadForeign, readJSON)
import Types (CompilerError, CompilerWarning, CompileResult)
import Unsafe.Coerce (unsafeCoerce)

main ∷ Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body

-- | The application state, which in this case just stores the current text in
-- | the editor.
type State = { currentSlide ∷ SlideId
             , slides ∷ Array SlideWithId
             }

-- | The query algebra for the app.
data Query a
  = ClearText a
  | ClearError a
  | HandleAceUpdate String a
  | HandleAceInitialised a
  | Initialise a
  | HandleSlideChange SlideChangedEvent (H.SubscribeStatus -> a)
  | RunCode a

-- | The slot address type for the Ace component.
data AceSlot = AceSlot
derive instance aceSlotEq ∷ Eq AceSlot
derive instance aceSlotOrd ∷ Ord AceSlot

-- | The main UI component definition.
ui ∷ H.Component HH.HTML Query Unit Void Aff
ui =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Initialise)
    , finalizer: Nothing
    }
  where


  initialState =
    { currentSlide: SlideId firstSlideId
    , slides: baseSlides
    }

  render ∷ State -> H.ParentHTML Query AceQuery AceSlot Aff
  render state@{ currentSlide, slides } =
    HH.div
      [ HP.id_ "root", HP.class_ $ HH.ClassName "reveal" ]
      [ HH.div
        [ HP.class_ $ HH.ClassName "slides"]
        (map (f currentSlide) slides)
      ]

  f ∷ SlideId -> SlideWithId -> HTML
  f current { content, id: id@(SlideId rawId) } = g content
    where 
      g = case_ 
          # on _regular identity
          # on _code (renderCodeSlide rawId (current == id))

  eval ∷ Query ~> H.ParentDSL State Query AceQuery AceSlot Void Aff
  eval = case _ of
    ClearText next -> pure next

    ClearError next -> do
      modifyCurrentSlide \x -> x { errors = [], warnings = [], output = Nothing }
      pure next

    RunCode next -> do
      currentSlideId <- H.gets _.currentSlide
      slides <- H.gets _.slides
      let currentSlide = find (\x -> x.id == currentSlideId) slides
      whenJust (currentSlide <#> _.content) $ match 
        { regular: \_ -> pure unit
        , code: \ { snippet: CodeSnippet txt, errors, warnings } -> do
          let content = RequestBody.string txt
          res <- H.liftAff $ Ajax.post ResponseFormat.string "_compile" content
          let (cr@{ errors, warnings } :: CompileResult) = readOrThrow res.body
          void (H.query AceSlot $ H.action (Ace.ShowCompileResult (cr)))
          if errors /= []
            then modifyCurrentSlide \x -> x { errors = errors, warnings = warnings, output = Nothing }
            else do
              res2 <- H.liftAff $ Ajax.post ResponseFormat.string "_run" (RequestBody.string "")
              let decoded = decode res2.body
              modifyCurrentSlide \x -> x { output = Just decoded.stdout }
        }
      pure next

    Initialise next -> do
      H.subscribe $ H.eventSource (onSlideChanged) (Just <<< H.request <<< HandleSlideChange)
      pure next

    HandleSlideChange event reply -> do
      let newSlideId = SlideId event.currentSlide.id
      H.modify_ $ _ { currentSlide = newSlideId }
      currentSlide <- H.gets _.currentSlide
      slides <- H.gets _.slides
      _ <- for_ slides case _ of
            { id, content } | id == currentSlide -> 
              content # whenMatch _code \ { snippet: CodeSnippet txt } -> 
                void $ H.query AceSlot $ H.action (Ace.ChangeText txt)
            other -> pure unit
      pure $ reply H.Listening

    HandleAceInitialised next -> do
      currentSlide <- H.gets _.currentSlide
      slides <- H.gets _.slides
      _ <- for_ slides case _ of
            { id, content } | id == currentSlide -> 
              content # whenMatch _code \ { snippet: CodeSnippet txt } -> 
                void $ H.query AceSlot $ H.action (Ace.ChangeText txt)
            other -> pure unit
      pure next

    HandleAceUpdate editorContent next -> do
      currentSlide <- H.gets _.currentSlide
      let update = case _ of
            s@{ id, content } | id == currentSlide ->
              s { content = modifySpecific _code (\x -> x { snippet = CodeSnippet editorContent }) content }
            other -> other
      H.modify_ $ \ old -> old { slides = update <$> old.slides }
      pure next

    where
      modifyCurrentSlide (modFn :: CodeSlide -> CodeSlide) = do
          currentSlide <- H.gets _.currentSlide
          slides <- H.gets _.slides
          let mappedSlides = (slides <#> case _ of
                slide@{ id, content } | id == currentSlide -> 
                  (default slide
                    # on _code \ c -> slide { content = inj _code $ modFn c }
                  ) content
                slide -> slide
          )
          H.modify_ $ _ { slides = mappedSlides }

readOrThrow :: forall a. ReadForeign a => Either ResponseFormatError String -> a
readOrThrow bodyEither = either unsafeCoerce identity $ do
  lmap Ajax.printResponseFormatError bodyEither >>= (\body ->
  lmap (foldMap renderForeignError) (readJSON body))

decode :: Either ResponseFormatError String -> { stdout :: String , stderr :: String , code :: Maybe Int }
decode bodyEither = either (\stderr -> { stdout: "", stderr, code: Just (-1) }) identity $ do
  lmap Ajax.printResponseFormatError bodyEither >>= (\body ->
  lmap (foldMap renderForeignError) (readJSON body))

type RunResult = { code ∷ Maybe Int, stdout ∷ String, stderr ∷ String }

type SlideChangedEvent = { currentSlide ∷ { id ∷ String } }

foreign import onSlideChanged ∷ (SlideChangedEvent -> Effect Unit) -> Effect Unit

newtype CodeSnippet = CodeSnippet String

newtype SlideId = SlideId String
derive instance eqSlideId ∷ Eq SlideId

type SlideWithId =
  { content ∷ Slide
  , id ∷ SlideId
  }

type CodeSlide = 
  { snippet :: CodeSnippet
  , output ∷ Maybe String
  , errors ∷ Array CompilerError
  , warnings :: Array CompilerWarning 
  }

type Slide = Variant ( regular :: HTML, code :: CodeSlide )

_regular = SProxy :: SProxy "regular"

_code = SProxy :: SProxy "code"

regularSlide ∷ String -> Array HTML -> SlideWithId
regularSlide id content =
  { id: SlideId id
  , content: inj _regular $ HH.section [ HP.id_ id ] content
  }

renderCodeSlide 
  :: String 
  -> Boolean
  -> CodeSlide
  -> HTML
renderCodeSlide id active { output, errors, warnings } =
  HH.section
    [ HP.id_ id ]
    [ HH.a
        [ HE.onClick $ HE.input_ RunCode
        , HP.class_ $ HH.ClassName "compileButton"
        ]
        [ HH.span_ [ HH.text "Run it!" ]
        ]
        , HH.div_
          [ if active then editor else emptyEditor
          , HH.pre
            [ HP.class_ $ HH.ClassName "goodOutput"
            ]
            [ HH.text $ "> " <> fromMaybe "" output ]
          ]
        ]
        where hasErrors = errors /= []

codeSlide ∷ String -> CodeSnippet -> SlideWithId
codeSlide id snippet =
  { id: SlideId id
  , content: inj _code { snippet, output: Nothing, errors: [], warnings: [] }
  }

type HTML = H.HTML (H.ComponentSlot HH.HTML AceQuery Aff AceSlot (Query Unit)) Query

editor' :: Array HTML -> HTML
editor' = HH.div [ HP.id_ "ace" ]

editor :: HTML
editor = editor' [ HH.slot AceSlot Ace.aceComponent unit handleAceOutput ]

emptyEditor ∷ HTML
emptyEditor = editor' []

handleAceOutput ∷ AceOutput -> Maybe (Query Unit)
handleAceOutput (Ace.TextChanged text) = Just $ H.action $ HandleAceUpdate text
handleAceOutput Ace.Initialised = Just $ H.action $ HandleAceInitialised

firstSlideId :: String
firstSlideId = "welcome"

baseSlides :: Array SlideWithId
baseSlides = 
        [ regularSlide firstSlideId
            [ HH.h1_ [ HH.text "A presentation about PureScript" ]
            , HH.p
              [ HP.class_ $ HH.ClassName "fragment" ]
              [ HH.text "Of the interactive kind" ]
            ]
        , codeSlide "hello" $ codeSnippet
              """
              |module Main where
              |
              |import Prelude
              |import Effect.Console (log)
              |import Simple.JSON
              |
              |main = log "Hello World"
              """
        , codeSlide "basics" $ codeSnippet
              """
              |module Main where
              |
              |import Prelude
              |import Effect.Console (logShow)
              |
              |main = logShow (12 + 44.0)
              """
        , codeSlide "arrays" $ codeSnippet
              """
              |module Main where
              |
              |import Prelude
              |import Effect.Console (logShow)
              |
              |main = logShow [ 2, 55, "no"]
              """
        , codeSlide "functions" $ codeSnippet
              """
              |module Main where
              |
              |import Prelude
              |import Effect.Console (logShow)
              |
              |likesProduct :: String -> String -> Boolean
              |likesProduct pid mid =
              |  if pid == "good product" && mid == "mango"
              |    then true
              |    else false
              |
              |main = do
              |  logShow $ likesProduct "mango" "mediocre product"
              """
        , regularSlide "types-are-a-lie"
            [ HH.h1_ [ HH.text "Type safety" ]
            , HH.p
              [ HP.class_ $ HH.ClassName "fragment"]
              [ HH.blockquote 
                [ HP.prop (PropName "cite") "https://twitter.com/jusrin00" ]  
                [ HH.div_ 
                  [ HH.img
                    [ HP.src "https://pbs.twimg.com/media/DCV46-aXUAEOnqf.jpg:large"
                    , HP.height 350
                    ]
                  , HH.div_ 
                    [ HH.a [ HP.href "https://twitter.com/jusrin00" ] [HH.text "Justin Woo"] ]
                  ]
                ]
              ]
            ]
        , codeSlide "newtypes" $ codeSnippet
              """
              |module Main where
              |
              |import Prelude
              |import Effect.Console (logShow)
              |
              |newtype ProductId = ProductId String
              |newtype MerchantId = MerchantId String
              |
              |likesProduct :: ProductId -> MerchantId -> Boolean
              |likesProduct (ProductId pid) (MerchantId mid) =
              |  if pid == "good product" && mid == "mango"
              |    then true
              |    else false
              |
              |main = do
              |  logShow $ likesProduct "mango" "mediocre product"
              """
        , codeSlide "JSON-Errors" $ codeSnippet
              """
              |module Main where
              |
              |import Prelude
              |import Effect.Console (logShow)
              |import Simple.JSON
              |
              |type MyType = { foo :: String, bar :: Int }
              |
              |main = do
              |  let json = ""'{"foo": 3, "bar": "hello"}'"" -- careful
              |  let result = readJSON json :: _ MyType
              |  logShow result
              """
        , codeSlide "more-json-errors" $ codeSnippet
              """
              |module Main where
              |
              |import Prelude
              |import Effect.Console (logShow)
              |import Simple.JSON
              |
              |niceReadJSON :: forall a. ReadForeign a => String -> Either String a
              |niceReadJSON = lmap renderError <<< readJSON

              |renderError :: MultipleErrors -> String
              |renderError = show <<< intercalate ", " <<< map fun
              |  where
              |    fun (ForeignError s) = s
              |    fun (TypeMismatch a b) = a <> " should be " <> b
              |    fun (ErrorAtIndex i e) = "At index: " <> (show i) <> " " <> (fun e)
              |    fun (ErrorAtProperty p e) = "At Property " <> p <> " " <> (fun e)

              |type MyType = { foo :: String, bar :: Int }
              |
              |main = do
              |  let json = ""'{"foo": 3, "bar": "hello"}'"" -- careful
              |  let result = niceReadJSON json :: _ MyType
              |  logShow result
              """
        ]

codeSnippet :: String -> CodeSnippet
codeSnippet s = CodeSnippet $ removePrefix "\n" (intercalate "\n" prefixesRemoved)
  where
      lines = S.split (S.Pattern "\n") s
      removePrefix pat str = fromMaybe str (S.stripPrefix (S.Pattern  pat) str)
      removeLeading c = S.dropWhile (_ == (S.codePointFromChar c))
      prefixesRemoved = removePrefix "|" <<< removeLeading ' ' <$> lines

whenJust :: forall m a. Applicative m => Maybe a -> (a -> m Unit) -> m Unit
whenJust v f = maybe (pure unit) f v

whenMatch (proxy :: SProxy _) (f :: CodeSlide -> _ Unit) (v :: Slide) = (default (pure unit) # on proxy f) v

modifySpecific proxy f v = 
  (default v # on proxy (\x -> inj proxy (f x))) v