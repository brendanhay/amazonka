{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.DescribeStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified stream. The information about the stream includes
-- its current status, its Amazon Resource Name (ARN), and an array of shard
-- objects. For each shard object, there is information about the hash key and
-- sequence number ranges that the shard spans, and the IDs of any earlier
-- shards that played in a role in creating the shard. A sequence number is
-- the identifier associated with every record ingested in the Amazon Kinesis
-- stream. The sequence number is assigned when a record is put into the
-- stream. You can limit the number of returned shards using the @Limit@
-- parameter. The number of shards in a stream may be too large to return from
-- a single call to @DescribeStream@. You can detect this by using the
-- @HasMoreShards@ flag in the returned output. @HasMoreShards@ is set to
-- @true@ when there is more data available. @DescribeStream@ is a paginated
-- operation. If there are more shards available, you can request them using
-- the shard ID of the last shard returned. Specify this ID in the
-- @ExclusiveStartShardId@ parameter in a subsequent request to
-- @DescribeStream@. @DescribeStream@ has a limit of 10 transactions per
-- second per account.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_DescribeStream.html>
module Network.AWS.Kinesis.DescribeStream
    (
    -- * Request
      DescribeStream
    -- ** Request constructor
    , describeStream
    -- ** Request lenses
    , ds1ExclusiveStartShardId
    , ds1Limit
    , ds1StreamName

    -- * Response
    , DescribeStreamResponse
    -- ** Response constructor
    , describeStreamResponse
    -- ** Response lenses
    , dsrStreamDescription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Kinesis.Types
import qualified GHC.Exts

data DescribeStream = DescribeStream
    { _ds1ExclusiveStartShardId :: Maybe Text
    , _ds1Limit                 :: Maybe Nat
    , _ds1StreamName            :: Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeStream' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds1ExclusiveStartShardId' @::@ 'Maybe' 'Text'
--
-- * 'ds1Limit' @::@ 'Maybe' 'Natural'
--
-- * 'ds1StreamName' @::@ 'Text'
--
describeStream :: Text -- ^ 'ds1StreamName'
               -> DescribeStream
describeStream p1 = DescribeStream
    { _ds1StreamName            = p1
    , _ds1Limit                 = Nothing
    , _ds1ExclusiveStartShardId = Nothing
    }

-- | The shard ID of the shard to start with.
ds1ExclusiveStartShardId :: Lens' DescribeStream (Maybe Text)
ds1ExclusiveStartShardId =
    lens _ds1ExclusiveStartShardId
        (\s a -> s { _ds1ExclusiveStartShardId = a })

-- | The maximum number of shards to return.
ds1Limit :: Lens' DescribeStream (Maybe Natural)
ds1Limit = lens _ds1Limit (\s a -> s { _ds1Limit = a }) . mapping _Nat

-- | The name of the stream to describe.
ds1StreamName :: Lens' DescribeStream Text
ds1StreamName = lens _ds1StreamName (\s a -> s { _ds1StreamName = a })

newtype DescribeStreamResponse = DescribeStreamResponse
    { _dsrStreamDescription :: StreamDescription
    } deriving (Eq, Show)

-- | 'DescribeStreamResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrStreamDescription' @::@ 'StreamDescription'
--
describeStreamResponse :: StreamDescription -- ^ 'dsrStreamDescription'
                       -> DescribeStreamResponse
describeStreamResponse p1 = DescribeStreamResponse
    { _dsrStreamDescription = p1
    }

-- | The current status of the stream, the stream ARN, an array of shard
-- objects that comprise the stream, and states whether there are more
-- shards available.
dsrStreamDescription :: Lens' DescribeStreamResponse StreamDescription
dsrStreamDescription =
    lens _dsrStreamDescription (\s a -> s { _dsrStreamDescription = a })

instance ToPath DescribeStream where
    toPath = const "/"

instance ToQuery DescribeStream where
    toQuery = const mempty

instance ToHeaders DescribeStream

instance ToJSON DescribeStream where
    toJSON DescribeStream{..} = object
        [ "StreamName"            .= _ds1StreamName
        , "Limit"                 .= _ds1Limit
        , "ExclusiveStartShardId" .= _ds1ExclusiveStartShardId
        ]

instance AWSRequest DescribeStream where
    type Sv DescribeStream = Kinesis
    type Rs DescribeStream = DescribeStreamResponse

    request  = post "DescribeStream"
    response = jsonResponse

instance FromJSON DescribeStreamResponse where
    parseJSON = withObject "DescribeStreamResponse" $ \o -> DescribeStreamResponse
        <$> o .:  "StreamDescription"

instance AWSPager DescribeStream where
    page rq rs
        | stop (rs ^. dsrStreamDescription . sdHasMoreShards) = Nothing
        | otherwise = Just $ rq
            & ds1ExclusiveStartShardId .~ rs ^. index (dsrStreamDescription . sdShards) sShardId
