module Types where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

type CompileResult = { errors :: Array CompilerError, warnings :: Array CompilerWarning }

type Position = { startLine :: Int, startColumn :: Int, endLine :: Int, endColumn :: Int }

type Span = { start :: Tuple Int Int, name :: String, end :: Tuple Int Int }

type Suggestion = 
  { replacement :: String
  , replaceRange :: Position
--  , allSpans :: Array Span
  }

type CompilerError = 
  { position :: Position
  , message :: String
  , errorCode :: String
  , errorLink :: String
  }

type CompilerWarning = 
  { position :: Position
  , message :: String
  , errorLink :: String
  , filename :: String
  , moduleName :: String
  , suggestion ::  Maybe Suggestion
  }

unusedImportLink :: String
unusedImportLink = "https://github.com/purescript/documentation/blob/master/errors/UnusedImport.md" 