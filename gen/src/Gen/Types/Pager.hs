{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Gen.Types.Pager
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Pager where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NE
import           Data.Maybe
import           Gen.Types.Id
import           Gen.Types.Notation

data Token a = Token
    { _tokenInput  :: Notation a
    , _tokenOutput :: Notation a
    } deriving (Eq, Show, Functor, Foldable)

makeLenses ''Token

instance FromJSON (Token Id) where
    parseJSON = withObject "token" $ \o -> Token
        <$> o .: "input_token"
        <*> o .: "output_token"

data Pager a
    = Next (Notation a) (Token a)
    | Many (Notation a) (NonEmpty (Token a))
      deriving (Eq, Show, Functor, Foldable)

instance FromJSON (Pager Id) where
    parseJSON = withObject "pager" $ \o -> next o <|> more o
      where
        next o = Next
            <$> resultKey o
            <*> parseJSON (Object o)

        resultKey o = do
               o .: "result_key"
           <|> (o .: "result_key" >>=
                   maybe (fail "Empty result_key")
                         pure . listToMaybe)
        more o = do
            let f k = o .: k <|> ((:|[]) <$> o .: k)

            inp <- f "input_token"
            out <- f "output_token"

            unless (NE.length inp == NE.length out) $
                fail "input_token and output_token contain differing number of keys."

            Many <$> o .: "more_results"
                 <*> pure (NE.zipWith Token inp out)
