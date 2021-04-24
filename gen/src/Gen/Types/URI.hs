{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.Types.URI
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.URI where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as Parse
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text
import GHC.Generics (Generic)
import Gen.TH
import Gen.Types.Id

data Segment
  = Tok Text
  | Var Id
  deriving (Eq, Show)

makePrisms ''Segment

data URI = URI'
  { _uriPath :: [Segment],
    _uriQuery :: [Segment]
  }
  deriving (Eq, Show)

makeClassy ''URI

segments :: Traversal' URI Segment
segments f x = URI' <$> traverse f (_uriPath x) <*> traverse f (_uriQuery x)

-- variables :: HasUrTraversal' URI Id
-- variables = segments . _Var

instance FromJSON URI where
  parseJSON = withText "uri" (either fail return . Parse.parseOnly uriParser)

uriParser :: Parser URI
uriParser =
  URI'
    <$> some seg
    <*> Parse.option [] (Parse.char '?' *> some seg)
    <* Parse.endOfInput
  where
    seg =
      Tok <$> Parse.takeWhile1 (end '{')
        <|> Var <$> var

    var =
      mkId . Text.filter rep
        <$> (Parse.char '{' *> Parse.takeWhile1 (end '}') <* Parse.char '}')

    end x y | x == y = False
    end _ '?' = False
    end _ _ = True

    rep '+' = False
    rep _ = True

data Method
  = GET
  | POST
  | HEAD
  | PUT
  | DELETE
  | PATCH
  deriving (Eq, Show, Generic)

instance FromJSON Method where
  parseJSON = gParseJSON' upper

instance ToJSON Method where
  toJSON = toJSON . methodToText

methodToText :: Method -> Text
methodToText = \case
  GET -> "get"
  POST -> "post"
  HEAD -> "head'"
  PUT -> "put"
  DELETE -> "delete"
  PATCH -> "patch"

data HTTP = HTTP
  { _method :: !Method,
    _requestURI :: !URI,
    _responseCode :: !Int
  }
  deriving (Show, Generic)

makeClassy ''HTTP

instance HasURI HTTP where
  uRI = requestURI

instance FromJSON HTTP where
  parseJSON = withObject "HTTP" $ \o ->
    HTTP
      <$> o .: "method"
      <*> o .: "requestUri"
      <*> ((o .: "responseCode" <&> parseStatusCode) <|> pure 200)

newtype StatusCodeParser = StatusCodeParser {parseStatusCode :: Int}

instance FromJSON StatusCodeParser where
  parseJSON (Number n) = StatusCodeParser <$> parseJSON (Number n)
  parseJSON (String s) =
    case Text.decimal s of
      Right (n, "") -> pure (StatusCodeParser n)
      v -> fail ("Failure parsing responseCode from: " ++ show v)
  parseJSON v =
    fail ("Failure parsing responseCode from: " ++ show v)
