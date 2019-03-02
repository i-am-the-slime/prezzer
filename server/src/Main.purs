module Main where

import Prelude

import Data.Array (unsafeIndex)
import Data.Either (Either(..))
import Data.Foldable (elem, intercalate)
import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Data.String.Utils (lines)
import Data.String.Utils as S
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Foreign (unsafeToForeign)
import HTTPure as HTTPure
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess (ExecResult, defaultExecOptions, exec, kill)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class WriteForeign, read_, writeJSON)

main ∷ HTTPure.ServerM
main = HTTPure.serve 5555 router $ Console.log "Server up at: http://localhost:5555/"
  where
    router = case _ of

      { method: HTTPure.Post, path: ["_compile"], body } -> do
        result <- compileCode body
        HTTPure.ok' (HTTPure.header "Content-Type" "application/json") result

      { method: HTTPure.Post, path: ["_run"] } -> do
        result   <- runCode
        response <- toBody result
        okJson response

      { method: HTTPure.Get, path } | elem path [[], [""], ["/"]] -> do
        content <- readTextFile UTF8 "index.html"
        HTTPure.ok' (HTTPure.header "Content-Type" "text/html") content

      { method: HTTPure.Get, path } -> do
        let filePath = intercalate "/" path
        let mimeType = guessMimeType filePath
        content <- readTextFile UTF8 filePath
        HTTPure.ok' (HTTPure.header "Content-Type" mimeType) content

      _ -> HTTPure.notFound

okJson ∷ ∀ a . WriteForeign a => a -> HTTPure.ResponseM
okJson = writeJSON >>> HTTPure.ok

guessMimeType :: String -> String
guessMimeType = case _ of
  s | s # S.endsWith ".js"   -> "text/javascript"
  s | s # S.endsWith ".html" -> "text/html"
  s | s # S.endsWith ".css"  -> "text/css"
  s | s # S.endsWith ".png"  -> "image/png"
  s | s # S.endsWith ".ico"  -> "image/vnd.microsoft.icon"
  s | s # S.endsWith ".jpg"  -> "image/jpeg"
  s | s # S.endsWith ".jpeg" -> "image/jpeg"
  _ -> "text/plain"

toBody :: forall r m. MonadEffect m => { stdout :: Buffer , stderr :: Buffer | r } -> m { code :: Maybe Int, stdout :: String, stderr :: String }
toBody result = liftEffect $ ado
  stdout <- Buffer.toString UTF8 result.stdout
  stderr <- Buffer.toString UTF8 result.stderr
  let (code :: Maybe Int) = asErrorWithCode result >>= _.code
  in { code, stdout, stderr } :: { code :: Maybe Int, stdout :: String, stderr :: String }

type ErrorWithCode = { code :: Maybe Int }

asErrorWithCode :: forall a. a -> Maybe ErrorWithCode
asErrorWithCode = read_ <<< unsafeToForeign 

compileCode ∷ String -> Aff String
compileCode code = do
  saveMainFile code
  { stderr } <- execCommand "spago build -- --json-errors"
  errorString <- liftEffect $ Buffer.toString UTF8 stderr
  pure $ unsafePartial (unsafeIndex (lines errorString) 0)

runCode ∷ Aff ExecResult
runCode = execCommand "node run.js"

playground ∷ String
playground = "../playground"

saveMainFile ∷ String -> Aff Unit
saveMainFile code =
  writeTextFile UTF8 (playground <> "/src/Main.purs") code

execCommand ∷ String -> Aff ExecResult
execCommand command =
  makeAff \callback -> do
    let options = defaultExecOptions { cwd = Just playground }
    childProcess <- exec command options (callback <<< Right)
    pure $ effectCanceler (kill SIGKILL childProcess)
