{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.V2013_12_02.CreateStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation adds a new Amazon Kinesis stream to your AWS account. A
-- stream captures and transports data records that are continuously emitted
-- from different data sources or producers. Scale-out within an Amazon
-- Kinesis stream is explicitly supported by means of shards, which are
-- uniquely identified groups of data records in an Amazon Kinesis stream. You
-- specify and control the number of shards that a stream is composed of. Each
-- open shard can support up to 5 read transactions per second, up to a
-- maximum total of 2 MB of data read per second. Each shard can support up to
-- 1000 write transactions per second, up to a maximum total of 1 MB data
-- written per second. You can add shards to a stream if the amount of data
-- input increases and you can remove shards if the amount of data input
-- decreases. The stream name identifies the stream. The name is scoped to the
-- AWS account used by the application. It is also scoped by region. That is,
-- two streams in two different accounts can have the same name, and two
-- streams in the same account, but in two different regions, can have the
-- same name. CreateStream is an asynchronous operation. Upon receiving a
-- CreateStream request, Amazon Kinesis immediately returns and sets the
-- stream status to CREATING. After the stream is created, Amazon Kinesis sets
-- the stream status to ACTIVE. You should perform read and write operations
-- only on an ACTIVE stream. You receive a LimitExceededException when making
-- a CreateStream request if you try to do one of the following: Have more
-- than five streams in the CREATING state at any point in time. Create more
-- shards than are authorized for your account. Note: The default limit for an
-- AWS account is 10 shards per stream. If you need to create a stream with
-- more than 10 shards, contact AWS Support to increase the limit on your
-- account. You can use the DescribeStream operation to check the stream
-- status, which is returned in StreamStatus. CreateStream has a limit of 5
-- transactions per second per account. Create a Stream The following is an
-- example of an Amazon Kinesis CreateStream request and response. POST /
-- HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.CreateStream {
-- "StreamName":"exampleStreamName","ShardCount":3 } HTTP/1.1 200 OK
-- x-amzn-RequestId: Content-Type: application/x-amz-json-1.1 Content-Length:
-- Date: ]]>.
module Network.AWS.Kinesis.V2013_12_02.CreateStream
    (
    -- * Request
      CreateStream
    -- ** Request constructor
    , mkCreateStreamInput
    -- ** Request lenses
    , csiStreamName
    , csiShardCount

    -- * Response
    , CreateStreamResponse
    ) where

import           Network.AWS.Kinesis.V2013_12_02.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStream' request.
mkCreateStreamInput :: Text -- ^ 'csiStreamName'
                    -> Integer -- ^ 'csiShardCount'
                    -> CreateStream
mkCreateStreamInput p1 p2 = CreateStream
    { _csiStreamName = p1
    , _csiShardCount = p2
    }
{-# INLINE mkCreateStreamInput #-}

data CreateStream = CreateStream
    { _csiStreamName :: Text
      -- ^ A name to identify the stream. The stream name is scoped to the
      -- AWS account used by the application that creates the stream. It
      -- is also scoped by region. That is, two streams in two different
      -- AWS accounts can have the same name, and two streams in the same
      -- AWS account, but in two different regions, can have the same
      -- name.
    , _csiShardCount :: Integer
      -- ^ The number of shards that the stream will use. The throughput of
      -- the stream is a function of the number of shards; more shards are
      -- required for greater provisioned throughput. Note: The default
      -- limit for an AWS account is 10 shards per stream. If you need to
      -- create a stream with more than 10 shards, contact AWS Support to
      -- increase the limit on your account.
    } deriving (Show, Generic)

-- | A name to identify the stream. The stream name is scoped to the AWS account
-- used by the application that creates the stream. It is also scoped by
-- region. That is, two streams in two different AWS accounts can have the
-- same name, and two streams in the same AWS account, but in two different
-- regions, can have the same name.
csiStreamName :: Lens' CreateStream (Text)
csiStreamName = lens _csiStreamName (\s a -> s { _csiStreamName = a })
{-# INLINE csiStreamName #-}

-- | The number of shards that the stream will use. The throughput of the stream
-- is a function of the number of shards; more shards are required for greater
-- provisioned throughput. Note: The default limit for an AWS account is 10
-- shards per stream. If you need to create a stream with more than 10 shards,
-- contact AWS Support to increase the limit on your account.
csiShardCount :: Lens' CreateStream (Integer)
csiShardCount = lens _csiShardCount (\s a -> s { _csiShardCount = a })
{-# INLINE csiShardCount #-}

instance ToPath CreateStream

instance ToQuery CreateStream

instance ToHeaders CreateStream

instance ToJSON CreateStream

data CreateStreamResponse = CreateStreamResponse
    deriving (Eq, Show, Generic)

instance AWSRequest CreateStream where
    type Sv CreateStream = Kinesis
    type Rs CreateStream = CreateStreamResponse

    request = get
    response _ = nullaryResponse CreateStreamResponse
