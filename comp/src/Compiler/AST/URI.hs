{-# LANGUAGE TemplateHaskell #-}

-- Module      : Compiler.AST.URI
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.URI
    ( Segment
    , URI (..)
    , uriPath
    , uriQuery
    , segments
    , variables
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parse
import           Data.Jason
import           Data.Text            (Text)

data Segment
    = Tok Text
    | Var Text
      deriving (Eq, Show)

makePrisms ''Segment

data URI = URI
    { _uriPath  :: [Segment]
    , _uriQuery :: [Segment]
    } deriving (Eq, Show)

makeLenses ''URI

segments :: Traversal' URI Segment
segments f x = URI <$> traverse f (_uriPath x) <*> traverse f (_uriQuery x)

variables :: Traversal' URI Text
variables = segments . _Var

instance FromJSON URI where
    parseJSON = withText "uri" (either fail return . Parse.parseOnly parser)

parser :: Parser URI
parser = URI
    <$> some seg
    <*> Parse.option [] (Parse.char '?' *> some seg)
    <*  Parse.endOfInput
  where
    seg = Tok <$> Parse.takeWhile1 (end '{')
      <|> Var <$> var

    var = Parse.char '{'
       *> Parse.takeWhile1 (end '}')
       <* Parse.char '}'

    end x y | x == y = False
    end _ '?'        = False
    end _ _          = True
