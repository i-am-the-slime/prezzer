module AceComponent (AceQuery(..), AceOutput(..), AceAction, aceComponent) where

import Prelude

import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Range as Range
import Ace.Types (Editor)
import Data.Array (catMaybes, filter, reverse)
import Data.Maybe (Maybe(..), isNothing)
import Data.String.CodeUnits as S
import Data.Traversable (for, for_)
import Debug.Trace (spy)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Types (CompileResult, unusedImportLink)

-- | The state for the ace component - we only need a reference to the editor,
-- | as Ace editor has its own internal state that we can query instead of
-- | replicating it within Halogen.
type AceState = { editor ∷ Maybe Editor, markers :: Array Int }

type AceAction =
  { action ∷ String, start ∷ { row ∷ Int, column ∷ Int }, end ∷ { row ∷ Int, column ∷ Int }, lines ∷ Array String }

-- | A basic query algebra for the Ace component.
data AceQuery a
  = Initialise a
  | Finalise a
  | ChangeText String a
  | GetText (String -> a)
  | ClearCompileResult a
  | ShowCompileResult CompileResult a
  | HandleChange AceAction (H.SubscribeStatus -> a)

data AceOutput = TextChanged String
               | Initialised

-- | The Ace component definition.
aceComponent ∷ H.Component HH.HTML AceQuery Unit AceOutput Aff
aceComponent =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialise)
    , finalizer: Just (H.action Finalise)
    , receiver: const Nothing
    }

  where

  initialState ∷ AceState
  initialState = { editor: Nothing, markers: [] }

  -- As we're embedding a 3rd party component we only need to create a
  -- placeholder div here and attach the ref property which will let us reference
  -- the element in eval.
  render ∷ AceState -> H.ComponentHTML AceQuery
  render = const $ HH.div [ HP.id_ "ace", HP.ref (H.RefLabel "ace") ] []

  -- The query algebra for the component handles the initialization of the Ace
  -- editor as well as responding to the `ChangeText` action that allows us to
  -- alter the editor's state.
  eval ∷ AceQuery ~> H.ComponentDSL AceState AceQuery AceOutput Aff
  eval = case _ of
    Initialise next -> do
      H.getHTMLElementRef (H.RefLabel "ace") >>= case _ of
        Nothing -> pure unit
        Just el' -> do
          editor <- H.liftEffect $ Ace.editNode el' Ace.ace
          H.liftEffect $ Editor.setKeyboardHandler "ace/keyboard/vim" editor
          H.liftEffect $ Editor.setTheme "ace/theme/dracula" editor
          H.liftEffect $ Editor.setFontSize "0.5em" editor
          H.liftEffect $ Editor.setStyle "font-family: 'PragmataPro Liga'" editor
          session <- H.liftEffect $ Editor.getSession editor
          H.liftEffect $ Session.setMode "ace/mode/haskell" session
          H.modify_ (_ { editor = Just editor })
          H.subscribe $ H.eventSource (Session.onChange session) (Just <<< H.request <<< HandleChange)
          H.raise $ Initialised
      pure next

    Finalise next -> do
      -- Release the reference to the editor and do any other cleanup that a
      -- real world component might need.
      H.modify_ (_ { editor = Nothing })
      pure next

    ChangeText text next -> do
      maybeEditor <- H.gets _.editor
      case maybeEditor of
        Nothing -> pure unit
        Just editor -> do
          current <- H.liftEffect $ Editor.getValue editor
          when (text /= current) do
            void $ H.liftEffect $ Editor.setValue text Nothing editor
      H.raise $ TextChanged text
      pure next

    ClearCompileResult next -> do
      maybeEditor <- H.gets _.editor
      case maybeEditor of 
        Nothing -> pure unit
        Just editor -> H.liftEffect do
          session <- Editor.getSession editor
          Session.clearAnnotations session
      pure next

    ShowCompileResult result' next -> do
      let result = result' 
                   { warnings = result'.warnings # filter \w -> 
                       w.errorLink /= unusedImportLink
                   }
      maybeEditor <- H.gets _.editor
      markers <- H.liftEffect $ case maybeEditor of 
        Nothing -> pure []
        Just editor -> do
          session <- Editor.getSession editor

          errors <- for result.errors $ \e -> do
            range <- Range.create 
                        (e.position.startLine - 1)
                        (e.position.startColumn - 1)
                        (e.position.endLine - 1)
                        (e.position.endColumn)
            Session.addMarker range "errorMarker" (spy "e" e).message false session

          let errorAnnotations = result.errors <#> \e ->
                { row: e.position.startLine - 1 , column: e.position.startColumn - 1 , text: e.message , type: "error"}

          warnings <- for result.warnings $ \w -> do
            range <- Range.create 
                        (w.position.startLine - 1)
                        (w.position.startColumn - 1)
                        (w.position.endLine - 1)
                        (w.position.endColumn)
            Session.addMarker (spy "Range" range) "warningMarker" w.message false session

          let warnAnnotations = filter (\w -> isNothing w.suggestion) result.warnings <#> \w ->
              { row: w.position.startLine - 1 , column: w.position.startColumn - 1 , text: w.message , type: "warning"}

          let suggestions = reverse <<< catMaybes $ result.warnings <#> _.suggestion
          
          when (result.errors == []) $ for_ suggestions \ { replacement, replaceRange } -> do
            let row = replaceRange.startLine - 1
            let col = replaceRange.startColumn - 1
            let rowEnd = replaceRange.endLine - 1
            let colEnd = replaceRange.endColumn - 1
            range <- Range.create row col rowEnd colEnd
            Session.remove range session
            Session.insert (Ace.Position { row: row, column: col }) (S.dropRight 1 replacement) session

          Session.setAnnotations (errorAnnotations <> warnAnnotations) session
          H.liftEffect $ Editor.resize (Just true) editor

          pure $ errors <> warnings

      H.modify_ $ _ { markers = markers }
      pure next

    GetText reply -> do
      maybeEditor <- H.gets _.editor
      text <- case maybeEditor of
        Nothing ->
          pure ""
        Just editor ->
          H.liftEffect $ Editor.getValue editor
      pure $ reply text

    HandleChange aceAction reply -> do
      maybeEditor <- H.gets _.editor
      case maybeEditor of
        Nothing -> pure unit
        Just editor -> do
          text <- H.liftEffect (Editor.getValue editor)
          markers <- H.gets _.markers
          session <- H.liftEffect $ Editor.getSession editor
          for_ markers \m -> H.liftEffect $ Session.removeMarker m session
          H.modify_ $ _ { markers = [] }
          H.liftEffect $ Session.clearAnnotations session
          H.liftEffect $ Editor.resize (Just true) editor
          H.raise $ TextChanged text
      pure (reply H.Listening)
