{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.V2013_12_02.DescribeStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns the following information about the stream: the
-- current status of the stream, the stream Amazon Resource Name (ARN), and an
-- array of shard objects that comprise the stream. For each shard object
-- there is information about the hash key and sequence number ranges that the
-- shard spans, and the IDs of any earlier shards that played in a role in a
-- MergeShards or SplitShard operation that created the shard. A sequence
-- number is the identifier associated with every record ingested in the
-- Amazon Kinesis stream. The sequence number is assigned by the Amazon
-- Kinesis service when a record is put into the stream. You can limit the
-- number of returned shards using the Limit parameter. The number of shards
-- in a stream may be too large to return from a single call to
-- DescribeStream. You can detect this by using the HasMoreShards flag in the
-- returned output. HasMoreShards is set to true when there is more data
-- available. If there are more shards available, you can request more shards
-- by using the shard ID of the last shard returned by the DescribeStream
-- request, in the ExclusiveStartShardId parameter in a subsequent request to
-- DescribeStream. DescribeStream is a paginated operation. DescribeStream has
-- a limit of 10 transactions per second per account. Obtain Information About
-- a Stream The following is an example of an Amazon Kinesis DescribeStream
-- request and response. POST / HTTP/1.1 Host: kinesis.. x-amz-Date:
-- Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.DescribeStream { "StreamName":"exampleStreamName" }
-- HTTP/1.1 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]> { "StreamDescription": { "HasMoreShards": false,
-- "Shards": [ { "HashKeyRange": { "EndingHashKey":
-- "113427455640312821154458202477256070484", "StartingHashKey": "0" },
-- "SequenceNumberRange": { "EndingSequenceNumber":
-- "21269319989741826081360214168359141376", "StartingSequenceNumber":
-- "21267647932558653966460912964485513216" }, "ShardId":
-- "shardId-000000000000" }, { "HashKeyRange": { "EndingHashKey":
-- "226854911280625642308916404954512140969", "StartingHashKey":
-- "113427455640312821154458202477256070485" }, "SequenceNumberRange": {
-- "StartingSequenceNumber": "21267647932558653966460912964485513217" },
-- "ShardId": "shardId-000000000001" }, { "HashKeyRange": { "EndingHashKey":
-- "340282366920938463463374607431768211455", "StartingHashKey":
-- "226854911280625642308916404954512140970" }, "SequenceNumberRange": {
-- "StartingSequenceNumber": "21267647932558653966460912964485513218" },
-- "ShardId": "shardId-000000000002" } ], "StreamARN":
-- "arn:aws:kinesis:us-east-1:052958737983:exampleStreamName", "StreamName":
-- "exampleStreamName", "StreamStatus": "ACTIVE" } }.
module Network.AWS.Kinesis.V2013_12_02.DescribeStream
    (
    -- * Request
      DescribeStream
    -- ** Request constructor
    , mkDescribeStream
    -- ** Request lenses
    , ds1StreamName
    , ds1Limit
    , ds1ExclusiveStartShardId

    -- * Response
    , DescribeStreamResponse
    -- ** Response lenses
    , dsrStreamDescription
    ) where

import Network.AWS.Kinesis.V2013_12_02.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a DescribeStream operation.
data DescribeStream = DescribeStream
    { _ds1StreamName :: Text
    , _ds1Limit :: Maybe Integer
    , _ds1ExclusiveStartShardId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStream' request.
mkDescribeStream :: Text -- ^ 'ds1StreamName'
                 -> DescribeStream
mkDescribeStream p1 = DescribeStream
    { _ds1StreamName = p1
    , _ds1Limit = Nothing
    , _ds1ExclusiveStartShardId = Nothing
    }

-- | The name of the stream to describe.
ds1StreamName :: Lens' DescribeStream Text
ds1StreamName = lens _ds1StreamName (\s a -> s { _ds1StreamName = a })

-- | The maximum number of shards to return.
ds1Limit :: Lens' DescribeStream (Maybe Integer)
ds1Limit = lens _ds1Limit (\s a -> s { _ds1Limit = a })

-- | The shard ID of the shard to start with for the stream description.
ds1ExclusiveStartShardId :: Lens' DescribeStream (Maybe Text)
ds1ExclusiveStartShardId =
    lens _ds1ExclusiveStartShardId
         (\s a -> s { _ds1ExclusiveStartShardId = a })

instance ToPath DescribeStream

instance ToQuery DescribeStream

instance ToHeaders DescribeStream

instance ToJSON DescribeStream

-- | Represents the output of a DescribeStream operation.
newtype DescribeStreamResponse = DescribeStreamResponse
    { _dsrStreamDescription :: StreamDescription
    } deriving (Show, Generic)

-- | Contains the current status of the stream, the stream ARN, an array of
-- shard objects that comprise the stream, and states whether there are more
-- shards available.
dsrStreamDescription :: Lens' DescribeStreamResponse StreamDescription
dsrStreamDescription =
    lens _dsrStreamDescription (\s a -> s { _dsrStreamDescription = a })

instance FromJSON DescribeStreamResponse

instance AWSRequest DescribeStream where
    type Sv DescribeStream = Kinesis
    type Rs DescribeStream = DescribeStreamResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeStream where
    next rq rs
        | not (rs ^. dsrStreamDescription . sdHasMoreShards) = Nothing
        | otherwise = Just $
            rq & ds1ExclusiveStartShardId .~ index (dsrStreamDescription . sdShards) sShardId rs
