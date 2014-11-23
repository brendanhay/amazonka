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

-- Module      : Network.AWS.Kinesis.GetShardIterator
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets a shard iterator. A shard iterator expires five minutes after it is
-- returned to the requester. A shard iterator specifies the position in the
-- shard from which to start reading data records sequentially. A shard
-- iterator specifies this position using the sequence number of a data record
-- in a shard. A sequence number is the identifier associated with every
-- record ingested in the Amazon Kinesis stream. The sequence number is
-- assigned when a record is put into the stream. You must specify the shard
-- iterator type. For example, you can set the ShardIteratorType parameter to
-- read exactly from the position denoted by a specific sequence number by
-- using the AT_SEQUENCE_NUMBER shard iterator type, or right after the
-- sequence number by using the AFTER_SEQUENCE_NUMBER shard iterator type,
-- using sequence numbers returned by earlier calls to PutRecord, GetRecords,
-- or DescribeStream. You can specify the shard iterator type TRIM_HORIZON in
-- the request to cause ShardIterator to point to the last untrimmed record in
-- the shard in the system, which is the oldest data record in the shard. Or
-- you can point to just after the most recent record in the shard, by using
-- the shard iterator type LATEST, so that you always read the most recent
-- data in the shard. When you repeatedly read from an Amazon Kinesis stream
-- use a GetShardIterator request to get the first shard iterator to to use in
-- your first GetRecords request and then use the shard iterator returned by
-- the GetRecords request in NextShardIterator for subsequent reads. A new
-- shard iterator is returned by every GetRecords request in
-- NextShardIterator, which you use in the ShardIterator parameter of the next
-- GetRecords request. If a GetShardIterator request is made too often, you
-- receive a ProvisionedThroughputExceededException. For more information
-- about throughput limits, see GetRecords. If the shard is closed, the
-- iterator can't return more data, and GetShardIterator returns null for its
-- ShardIterator. A shard can be closed using SplitShard or MergeShards.
-- GetShardIterator has a limit of 5 transactions per second per account per
-- open shard.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html>
module Network.AWS.Kinesis.GetShardIterator
    (
    -- * Request
      GetShardIterator
    -- ** Request constructor
    , getShardIterator
    -- ** Request lenses
    , gsiShardId
    , gsiShardIteratorType
    , gsiStartingSequenceNumber
    , gsiStreamName

    -- * Response
    , GetShardIteratorResponse
    -- ** Response constructor
    , getShardIteratorResponse
    -- ** Response lenses
    , gsirShardIterator
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Kinesis.Types
import qualified GHC.Exts

data GetShardIterator = GetShardIterator
    { _gsiShardId                :: Text
    , _gsiShardIteratorType      :: ShardIteratorType
    , _gsiStartingSequenceNumber :: Maybe Text
    , _gsiStreamName             :: Text
    } deriving (Eq, Show)

-- | 'GetShardIterator' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsiShardId' @::@ 'Text'
--
-- * 'gsiShardIteratorType' @::@ 'ShardIteratorType'
--
-- * 'gsiStartingSequenceNumber' @::@ 'Maybe' 'Text'
--
-- * 'gsiStreamName' @::@ 'Text'
--
getShardIterator :: Text -- ^ 'gsiStreamName'
                 -> Text -- ^ 'gsiShardId'
                 -> ShardIteratorType -- ^ 'gsiShardIteratorType'
                 -> GetShardIterator
getShardIterator p1 p2 p3 = GetShardIterator
    { _gsiStreamName             = p1
    , _gsiShardId                = p2
    , _gsiShardIteratorType      = p3
    , _gsiStartingSequenceNumber = Nothing
    }

-- | The shard ID of the shard to get the iterator for.
gsiShardId :: Lens' GetShardIterator Text
gsiShardId = lens _gsiShardId (\s a -> s { _gsiShardId = a })

-- | Determines how the shard iterator is used to start reading data records
-- from the shard. The following are the valid shard iterator types:
-- AT_SEQUENCE_NUMBER - Start reading exactly from the position denoted by a
-- specific sequence number. AFTER_SEQUENCE_NUMBER - Start reading right
-- after the position denoted by a specific sequence number. TRIM_HORIZON -
-- Start reading at the last untrimmed record in the shard in the system,
-- which is the oldest data record in the shard. LATEST - Start reading just
-- after the most recent record in the shard, so that you always read the
-- most recent data in the shard.
gsiShardIteratorType :: Lens' GetShardIterator ShardIteratorType
gsiShardIteratorType =
    lens _gsiShardIteratorType (\s a -> s { _gsiShardIteratorType = a })

-- | The sequence number of the data record in the shard from which to start
-- reading from.
gsiStartingSequenceNumber :: Lens' GetShardIterator (Maybe Text)
gsiStartingSequenceNumber =
    lens _gsiStartingSequenceNumber
        (\s a -> s { _gsiStartingSequenceNumber = a })

-- | The name of the stream.
gsiStreamName :: Lens' GetShardIterator Text
gsiStreamName = lens _gsiStreamName (\s a -> s { _gsiStreamName = a })

newtype GetShardIteratorResponse = GetShardIteratorResponse
    { _gsirShardIterator :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'GetShardIteratorResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsirShardIterator' @::@ 'Maybe' 'Text'
--
getShardIteratorResponse :: GetShardIteratorResponse
getShardIteratorResponse = GetShardIteratorResponse
    { _gsirShardIterator = Nothing
    }

-- | The position in the shard from which to start reading data records
-- sequentially. A shard iterator specifies this position using the sequence
-- number of a data record in a shard.
gsirShardIterator :: Lens' GetShardIteratorResponse (Maybe Text)
gsirShardIterator =
    lens _gsirShardIterator (\s a -> s { _gsirShardIterator = a })

instance ToPath GetShardIterator where
    toPath = const "/"

instance ToQuery GetShardIterator where
    toQuery = const mempty

instance ToHeaders GetShardIterator

instance ToJSON GetShardIterator where
    toJSON GetShardIterator{..} = object
        [ "StreamName"             .= _gsiStreamName
        , "ShardId"                .= _gsiShardId
        , "ShardIteratorType"      .= _gsiShardIteratorType
        , "StartingSequenceNumber" .= _gsiStartingSequenceNumber
        ]

instance AWSRequest GetShardIterator where
    type Sv GetShardIterator = Kinesis
    type Rs GetShardIterator = GetShardIteratorResponse

    request  = post "GetShardIterator"
    response = jsonResponse

instance FromJSON GetShardIteratorResponse where
    parseJSON = withObject "GetShardIteratorResponse" $ \o -> GetShardIteratorResponse
        <$> o .:? "ShardIterator"
