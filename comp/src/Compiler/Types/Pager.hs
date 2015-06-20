{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.Types.Pager
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Pager where

import           Compiler.TH
import           Compiler.Types.Id
import           Compiler.Types.Notation
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Text    (Parser, parseOnly)
import qualified Data.Attoparsec.Text    as A
import           Data.Foldable           (foldl')
import           Data.Ord
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           GHC.Generics

data Token a = Token
    { _tokenInput  :: Notation a
    , _tokenOutput :: Notation a
    } deriving (Eq, Show, Functor, Foldable, Traversable)

instance FromJSON (Token Id) where
    parseJSON = withObject "token" $ \o -> Token
        <$> o .: "input_token"
        <*> o .: "output_token"

data Pager a
    = More (Notation a) [Token a]
    | Next (Notation a) (Token a)
      deriving (Eq, Show, Functor, Foldable, Traversable)

instance FromJSON (Pager Id) where
    parseJSON = withObject "pager" $ \o -> more o <|> next o
      where
        next o = Next
            <$> o .: "result_key"
            <*> parseJSON (Object o)

        more o = do
            let f k = o .: k <|> (:[]) <$> o .: k

            inp <- f "input_token"
            out <- f "output_token"

            unless (length inp == length out) $
                fail "input_token and output_token contain differing number of keys."

            More <$> o .: "more_results"
                 <*> pure (zipWith Token inp out)

-- EC2
-- "input_token": "NextToken",
-- "output_token": "NextToken",
-- "limit_key": "MaxResults",
-- "result_key": "ReservedInstancesOfferings"

-- S3
-- "limit_key": "MaxUploads",
-- "more_results": "IsTruncated",
-- "output_token": [
-- "NextKeyMarker",
-- "NextUploadIdMarker"
-- ],
-- "input_token": [
-- "KeyMarker",
-- "UploadIdMarker"
-- ],
-- "result_key": [
-- "Uploads",
-- "CommonPrefixes"
-- ]

-- RDS
-- "input_token": "Marker",
-- "output_token": "Marker",
-- "limit_key": "MaxRecords",
-- "result_key": "ReservedDBInstancesOfferings"

-- Kinesis
-- "input_token": "ExclusiveStartShardId",
--   "limit_key": "Limit",
--   "more_results": "StreamDescription.HasMoreShards",
--   "output_token": "StreamDescription.Shards[-1].ShardId",
--   "result_key": "StreamDescription.Shards",
--   "non_aggregate_keys": [
--     "StreamDescription.StreamARN",
--     "StreamDescription.StreamName",
--     "StreamDescription.StreamStatus"
--   ]



-- pagerKeys :: Traversal' (Pager a) Key
-- pagerKeys f = \case
--     More k ts -> More <$> f k <*> traverse (tokenKeys f) ts
--     Next k t  -> Next <$> f k <*> tokenKeys f t

-- instance FromJSON (Pager ()) where
--     parseJSON = withObject "pager" $ \o -> more o <|> next o
--       where
--         more o = do
--             xs <- f "input_token"
--             ys <- f "output_token"

--             unless (length xs == length ys) $
--                 fail "input_token and output_token don't contain same number of keys."

--             More <$> o .: "more_results"
--                  <*> pure (zipWith (Token () ()) xs ys)
--           where
--             f k = o .: k <|> (:[]) <$> o .: k

--         next o = Next
--             <$> o .: "result_key"
--             <*> (Token () () <$> o .: "input_token" <*> o .: "output_token")
