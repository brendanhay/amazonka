{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Kinesis.V2013_12_02.GetShardIterator
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns a shard iterator in ShardIterator. The shard
-- iterator specifies the position in the shard from which you want to start
-- reading data records sequentially. A shard iterator specifies this position
-- using the sequence number of a data record in a shard. A sequence number is
-- the identifier associated with every record ingested in the Amazon Kinesis
-- stream. The sequence number is assigned by the Amazon Kinesis service when
-- a record is put into the stream. You must specify the shard iterator type
-- in the GetShardIterator request. For example, you can set the
-- ShardIteratorType parameter to read exactly from the position denoted by a
-- specific sequence number by using the AT_SEQUENCE_NUMBER shard iterator
-- type, or right after the sequence number by using the AFTER_SEQUENCE_NUMBER
-- shard iterator type, using sequence numbers returned by earlier PutRecord,
-- GetRecords or DescribeStream requests. You can specify the shard iterator
-- type TRIM_HORIZON in the request to cause ShardIterator to point to the
-- last untrimmed record in the shard in the system, which is the oldest data
-- record in the shard. Or you can point to just after the most recent record
-- in the shard, by using the shard iterator type LATEST, so that you always
-- read the most recent data in the shard. Note: Each shard iterator expires
-- five minutes after it is returned to the requester. When you repeatedly
-- read from an Amazon Kinesis stream use a GetShardIterator request to get
-- the first shard iterator to to use in your first GetRecords request and
-- then use the shard iterator returned by the GetRecords request in
-- NextShardIterator for subsequent reads. A new shard iterator is returned by
-- every GetRecords request in NextShardIterator, which you use in the
-- ShardIterator parameter of the next GetRecords request. If a
-- GetShardIterator request is made too often, you will receive a
-- ProvisionedThroughputExceededException. For more information about
-- throughput limits, see the Amazon Kinesis Developer Guide. GetShardIterator
-- can return null for its ShardIterator to indicate that the shard has been
-- closed and that the requested iterator will return no more data. A shard
-- can be closed by a SplitShard or MergeShards operation. GetShardIterator
-- has a limit of 5 transactions per second per account per open shard. Get a
-- Shard Iterator The following is an example of an Amazon Kinesis
-- GetShardIterator request and response. POST / HTTP/1.1 Host: kinesis..
-- x-amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.GetShardIterator { "StreamName": "exampleStreamName",
-- "ShardId": "shardId-000000000001", "ShardIteratorType": "LATEST" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]> { "ShardIterator":
-- "AAAAAAAAAAETYyAYzd665+8e0X7JTsASDM/Hr2rSwc0X2qz93iuA3udrjTH+ikQvpQk/1ZcMMLzRdAesqwBGPnsthzU0/CBlM/U8/8oEqGwX3pKw0XyeDNRAAZyXBo3MqkQtCpXhr942BRTjvWKhFz7OmCb2Ncfr8Tl2cBktooi6kJhr+djN5WYkB38Rr3akRgCl9qaU4dY="
-- }.
module Network.AWS.Kinesis.V2013_12_02.GetShardIterator where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.Kinesis.V2013_12_02.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetShardIterator' request.
getShardIterator :: Text -- ^ '_gsiiShardId'
                 -> ShardIteratorType -- ^ '_gsiiShardIteratorType'
                 -> Text -- ^ '_gsiiStreamName'
                 -> GetShardIterator
getShardIterator p1 p2 p3 = GetShardIterator
    { _gsiiShardId = p1
    , _gsiiShardIteratorType = p2
    , _gsiiStreamName = p3
    , _gsiiStartingSequenceNumber = Nothing
    }

data GetShardIterator = GetShardIterator
    { _gsiiShardId :: Text
      -- ^ The shard ID of the shard to get the iterator for.
    , _gsiiShardIteratorType :: ShardIteratorType
      -- ^ Determines how the shard iterator is used to start reading data
      -- records from the shard. The following are the valid shard
      -- iterator types: AT_SEQUENCE_NUMBER - Start reading exactly from
      -- the position denoted by a specific sequence number.
      -- AFTER_SEQUENCE_NUMBER - Start reading right after the position
      -- denoted by a specific sequence number. TRIM_HORIZON - Start
      -- reading at the last untrimmed record in the shard in the system,
      -- which is the oldest data record in the shard. LATEST - Start
      -- reading just after the most recent record in the shard, so that
      -- you always read the most recent data in the shard.
    , _gsiiStreamName :: Text
      -- ^ The name of the stream.
    , _gsiiStartingSequenceNumber :: Maybe Text
      -- ^ The sequence number of the data record in the shard from which to
      -- start reading from.
    } deriving (Generic)

makeLenses ''GetShardIterator

instance ToPath GetShardIterator

instance ToQuery GetShardIterator

instance ToHeaders GetShardIterator

instance ToJSON GetShardIterator

data GetShardIteratorResponse = GetShardIteratorResponse
    { _gsioShardIterator :: Maybe Text
      -- ^ The position in the shard from which to start reading data
      -- records sequentially. A shard iterator specifies this position
      -- using the sequence number of a data record in a shard.
    } deriving (Generic)

makeLenses ''GetShardIteratorResponse

instance FromJSON GetShardIteratorResponse

instance AWSRequest GetShardIterator where
    type Sv GetShardIterator = Kinesis
    type Rs GetShardIterator = GetShardIteratorResponse

    request = get
    response _ = jsonResponse
