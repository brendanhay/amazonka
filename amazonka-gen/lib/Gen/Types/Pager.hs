{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Gen.Types.Pager
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.Pager where

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Value (..), (.:))
import qualified Data.List as List
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
  parseJSON = Aeson.withObject "token" $ \o ->
    Token
      <$> o .: "input_token"
      <*> o .: "output_token"

data Pager a
  = Next (NonEmpty (Notation a)) (Token a)
  | Many (Notation a) (NonEmpty (Token a))
  | Only (Token a)
  deriving stock (Eq, Show, Functor, Foldable)

instance FromJSON (Pager Id) where
  parseJSON = Aeson.withObject "pager" $ \o -> more o <|> next o <|> limit o
    where
      limit o =
        Only
          <$> Aeson.parseJSON (Object o)

      next o =
        Next
          <$> oneOrMany o "result_key"
          <*> Aeson.parseJSON (Object o)

      more o = do
        inp <- oneOrMany o "input_token"
        out <- oneOrMany o "output_token"

        unless (NonEmpty.length inp == NonEmpty.length out) $
          fail "input_token and output_token contain differing number of keys."

        Many <$> o .: "more_results"
          <*> pure (NonEmpty.zipWith Token inp out)

      oneOrMany o k = o .: k <|> ((:| []) <$> o .: k)
