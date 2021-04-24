{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.Types.Pager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Pager where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Gen.Types.Id
import Gen.Types.Notation

data Token a = Token
  { _tokenInput :: Notation a,
    _tokenOutput :: Notation a
  }
  deriving (Eq, Show, Functor, Foldable)

makeLenses ''Token

instance FromJSON (Token Id) where
  parseJSON = withObject "token" $ \o ->
    Token
      <$> o .: "input_token"
      <*> o .: "output_token"

data Pager a
  = Next (NonEmpty (Notation a)) (Token a)
  | Many (Notation a) (NonEmpty (Token a))
  | Only (Token a)
  deriving (Eq, Show, Functor, Foldable)

instance FromJSON (Pager Id) where
  parseJSON = withObject "pager" $ \o -> more o <|> next o <|> limit o
    where
      limit o =
        Only
          <$> parseJSON (Object o)

      next o =
        Next
          <$> oneOrMany o "result_key"
          <*> parseJSON (Object o)

      more o = do
        inp <- oneOrMany o "input_token"
        out <- oneOrMany o "output_token"

        unless (NE.length inp == NE.length out) $
          fail "input_token and output_token contain differing number of keys."

        Many <$> o .: "more_results"
          <*> pure (NE.zipWith Token inp out)

      oneOrMany o k = o .: k <|> ((:| []) <$> o .: k)
