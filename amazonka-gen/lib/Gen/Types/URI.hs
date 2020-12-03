{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Gen.Types.URI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.URI where

import Control.Applicative (some)
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Value (..), (.:))
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import qualified GHC.Generics as Generics
import Gen.Prelude
import qualified Gen.TH
import Gen.Types.Id

data Segment
  = Tok Text
  | Var Id
  deriving stock (Eq, Show)

$(Lens.makePrisms ''Segment)

data URI = URI'
  { _uriPath :: [Segment],
    _uriQuery :: [Segment]
  }
  deriving stock (Eq, Show)

$(Lens.makeClassy ''URI)

segments :: Traversal' URI Segment
segments f x = URI' <$> traverse f (_uriPath x) <*> traverse f (_uriQuery x)

-- variables :: HasUrTraversal' URI Id
-- variables = segments . _Var

instance FromJSON URI where
  parseJSON = Aeson.withText "uri" (either fail pure . Parsec.parseOnly uriParser)

uriParser :: Parsec.Parser URI
uriParser =
  URI'
    <$> some seg
    <*> Parsec.option [] (Parsec.char '?' *> some seg)
    <* Parsec.endOfInput
  where
    seg =
      Tok <$> Parsec.takeWhile1 (end '{')
        <|> Var <$> var

    var =
      mkId . Text.filter rep
        <$> (Parsec.char '{' *> Parsec.takeWhile1 (end '}') <* Parsec.char '}')

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
  deriving stock (Eq, Show, Generic)

instance FromJSON Method where
  parseJSON = Gen.TH.genericParseJSON Gen.TH.upper

instance ToJSON Method where
  toJSON = Aeson.toJSON . methodToText

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
  deriving stock (Show, Generic)

$(Lens.makeClassy ''HTTP)

instance HasURI HTTP where
  uRI = requestURI

instance FromJSON HTTP where
  parseJSON = Aeson.withObject "HTTP" $ \o ->
    HTTP
      <$> o .: "method"
      <*> o .: "requestUri"
      <*> ((o .: "responseCode" <&> parseStatusCode) <|> pure 200)

newtype StatusCodeParser = StatusCodeParser {parseStatusCode :: Int}

instance FromJSON StatusCodeParser where
  parseJSON (Number n) = StatusCodeParser <$> Aeson.parseJSON (Number n)
  parseJSON (String s) =
    case Text.Read.decimal s of
      Right (n, "") -> pure (StatusCodeParser n)
      v -> fail ("Failure parsing responseCode from: " ++ show v)
  parseJSON v =
    fail ("Failure parsing responseCode from: " ++ show v)
