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

-- Module      : Network.AWS.Kinesis.CreateStream
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a Amazon Kinesis stream. A stream captures and transports data
-- records that are continuously emitted from different data sources or /producers/
-- . Scale-out within an Amazon Kinesis stream is explicitly supported by means
-- of shards, which are uniquely identified groups of data records in an Amazon
-- Kinesis stream.
--
-- You specify and control the number of shards that a stream is composed of.
-- Each open shard can support up to 5 read transactions per second, up to a
-- maximum total of 2 MB of data read per second. Each shard can support up to
-- 1000 records written per second, up to a maximum total of 1 MB data written
-- per second. You can add shards to a stream if the amount of data input
-- increases and you can remove shards if the amount of data input decreases.
--
-- The stream name identifies the stream. The name is scoped to the AWS account
-- used by the application. It is also scoped by region. That is, two streams in
-- two different accounts can have the same name, and two streams in the same
-- account, but in two different regions, can have the same name.
--
-- 'CreateStream' is an asynchronous operation. Upon receiving a 'CreateStream'
-- request, Amazon Kinesis immediately returns and sets the stream status to 'CREATING'. After the stream is created, Amazon Kinesis sets the stream status to 'ACTIVE'
-- . You should perform read and write operations only on an 'ACTIVE' stream.
--
-- You receive a 'LimitExceededException' when making a 'CreateStream' request if
-- you try to do one of the following:
--
-- Have more than five streams in the 'CREATING' state at any point in time. Create more shards than are authorized for your account.
-- The default limit for an AWS account is 10 shards per stream. If you need
-- to create a stream with more than 10 shards, <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html contact AWS Support> to increase
-- the limit on your account.
--
-- You can use 'DescribeStream' to check the stream status, which is returned in 'StreamStatus'.
--
-- 'CreateStream' has a limit of 5 transactions per second per account.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_CreateStream.html>
module Network.AWS.Kinesis.CreateStream
    (
    -- * Request
      CreateStream
    -- ** Request constructor
    , createStream
    -- ** Request lenses
    , csShardCount
    , csStreamName

    -- * Response
    , CreateStreamResponse
    -- ** Response constructor
    , createStreamResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Kinesis.Types
import qualified GHC.Exts

data CreateStream = CreateStream
    { _csShardCount :: Nat
    , _csStreamName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateStream' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csShardCount' @::@ 'Natural'
--
-- * 'csStreamName' @::@ 'Text'
--
createStream :: Text -- ^ 'csStreamName'
             -> Natural -- ^ 'csShardCount'
             -> CreateStream
createStream p1 p2 = CreateStream
    { _csStreamName = p1
    , _csShardCount = withIso _Nat (const id) p2
    }

-- | The number of shards that the stream will use. The throughput of the stream
-- is a function of the number of shards; more shards are required for greater
-- provisioned throughput.
--
-- Note: The default limit for an AWS account is 10 shards per stream. If you
-- need to create a stream with more than 10 shards, <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html contact AWS Support> to
-- increase the limit on your account.
csShardCount :: Lens' CreateStream Natural
csShardCount = lens _csShardCount (\s a -> s { _csShardCount = a }) . _Nat

-- | A name to identify the stream. The stream name is scoped to the AWS account
-- used by the application that creates the stream. It is also scoped by region.
-- That is, two streams in two different AWS accounts can have the same name,
-- and two streams in the same AWS account, but in two different regions, can
-- have the same name.
csStreamName :: Lens' CreateStream Text
csStreamName = lens _csStreamName (\s a -> s { _csStreamName = a })

data CreateStreamResponse = CreateStreamResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'CreateStreamResponse' constructor.
createStreamResponse :: CreateStreamResponse
createStreamResponse = CreateStreamResponse

instance ToPath CreateStream where
    toPath = const "/"

instance ToQuery CreateStream where
    toQuery = const mempty

instance ToHeaders CreateStream

instance ToJSON CreateStream where
    toJSON CreateStream{..} = object
        [ "StreamName" .= _csStreamName
        , "ShardCount" .= _csShardCount
        ]

instance AWSRequest CreateStream where
    type Sv CreateStream = Kinesis
    type Rs CreateStream = CreateStreamResponse

    request  = post "CreateStream"
    response = nullResponse CreateStreamResponse
