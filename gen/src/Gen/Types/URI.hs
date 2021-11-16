{-# LANGUAGE TemplateHaskell #-}

module Gen.Types.URI where

import Control.Applicative (some)
import qualified Control.Lens as Lens
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Gen.Prelude
import Gen.TH
import Gen.Types.Id

data Segment
  = Tok Text
  | Var Id
  deriving (Eq, Show)

$(Lens.makePrisms ''Segment)

data URI = URI'
  { _uriPath :: [Segment],
    _uriQuery :: [Segment]
  }
  deriving (Eq, Show)

$(Lens.makeClassy ''URI)

segments :: Lens.Traversal' URI Segment
segments f x = URI' <$> traverse f (_uriPath x) <*> traverse f (_uriQuery x)

instance FromJSON URI where
  parseJSON = Aeson.withText "URI" (either fail return . Atto.parseOnly uriParser)

uriParser :: Atto.Parser URI
uriParser =
  URI'
    <$> some seg
    <*> Atto.option [] (Atto.char '?' *> some seg)
    <* Atto.endOfInput
  where
    seg =
      Tok <$> Atto.takeWhile1 (end '{')
        <|> Var <$> var

    var =
      mkId . Text.filter rep
        <$> (Atto.char '{' *> Atto.takeWhile1 (end '}') <* Atto.char '}')

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
  { _method :: Method,
    _requestURI :: URI,
    _responseCode :: Int
  }
  deriving (Show, Generic)

$(Lens.makeClassy ''HTTP)

instance HasURI HTTP where
  uRI = requestURI

instance FromJSON HTTP where
  parseJSON =
    Aeson.withObject "HTTP" $ \o ->
      HTTP
        <$> o .: "method"
        <*> o .: "requestUri"
        <*> ((o .: "responseCode" <&> parseStatusCode) <|> pure 200)

newtype StatusCodeParser = StatusCodeParser {parseStatusCode :: Int}

instance FromJSON StatusCodeParser where
  parseJSON = \case
    Aeson.Number n -> StatusCodeParser <$> Aeson.parseJSON (Aeson.Number n)
    Aeson.String s ->
      case Text.decimal s of
        Right (n, "") -> pure (StatusCodeParser n)
        other -> fail ("Failure parsing responseCode from: " ++ show other)
    value ->
      fail ("Failure parsing responseCode from: " ++ show value)
