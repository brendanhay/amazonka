{-# LANGUAGE TemplateHaskell #-}

module Gen.Types.Pager where

import qualified Control.Lens as Lens
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NonEmpty
import Gen.Prelude
import Gen.Types.Id
import Gen.Types.Notation

data Token a = Token
  { _tokenInput :: Notation a,
    _tokenOutput :: Notation a
  }
  deriving stock (Eq, Show, Functor, Foldable)

$(Lens.makeLenses ''Token)

instance FromJSON (Token Id) where
  parseJSON =
    Aeson.withObject "token" $ \o ->
      Token
        <$> o .: "input_token"
        <*> o .: "output_token"

data Pager a
  = Next (NonEmpty (Notation a)) (Token a)
  | Many (Notation a) (NonEmpty (Token a))
  | Only (Token a)
  deriving stock (Eq, Show, Functor, Foldable)

instance FromJSON (Pager Id) where
  parseJSON =
    Aeson.withObject "Pager" $ \o ->
      more o
        <|> next o
        <|> limit o
    where
      limit o =
        Only
          <$> Aeson.parseJSON (Aeson.Object o)

      next o =
        Next
          <$> oneOrMany o "result_key"
          <*> Aeson.parseJSON (Aeson.Object o)

      more o = do
        inp <- oneOrMany o "input_token"
        out <- oneOrMany o "output_token"

        unless (NonEmpty.length inp == NonEmpty.length out) $
          fail "input_token and output_token contain differing number of keys."

        Many <$> o .: "more_results"
          <*> pure (NonEmpty.zipWith Token inp out)

      oneOrMany o k = o .: k <|> ((:| []) <$> o .: k)
