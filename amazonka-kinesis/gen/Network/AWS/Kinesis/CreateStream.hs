{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.CreateStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Kinesis data stream. A stream captures and transports data records that are continuously emitted from different data sources or /producers/ . Scale-out within a stream is explicitly supported by means of shards, which are uniquely identified groups of data records in a stream.
--
--
-- You specify and control the number of shards that a stream is composed of. Each shard can support reads up to five transactions per second, up to a maximum data read total of 2 MB per second. Each shard can support writes up to 1,000 records per second, up to a maximum data write total of 1 MB per second. If the amount of data input increases or decreases, you can add or remove shards.
--
-- The stream name identifies the stream. The name is scoped to the AWS account used by the application. It is also scoped by AWS Region. That is, two streams in two different accounts can have the same name, and two streams in the same account, but in two different Regions, can have the same name.
--
-- @CreateStream@ is an asynchronous operation. Upon receiving a @CreateStream@ request, Kinesis Data Streams immediately returns and sets the stream status to @CREATING@ . After the stream is created, Kinesis Data Streams sets the stream status to @ACTIVE@ . You should perform read and write operations only on an @ACTIVE@ stream.
--
-- You receive a @LimitExceededException@ when making a @CreateStream@ request when you try to do one of the following:
--
--     * Have more than five streams in the @CREATING@ state at any point in time.
--
--     * Create more shards than are authorized for your account.
--
--
--
-- For the default shard limit for an AWS account, see <http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Amazon Kinesis Data Streams Limits> in the /Amazon Kinesis Data Streams Developer Guide/ . To increase this limit, <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html contact AWS Support> .
--
-- You can use @DescribeStream@ to check the stream status, which is returned in @StreamStatus@ .
--
-- 'CreateStream' has a limit of five transactions per second per account.
--
module Network.AWS.Kinesis.CreateStream
    (
    -- * Creating a Request
      createStream
    , CreateStream
    -- * Request Lenses
    , csStreamName
    , csShardCount

    -- * Destructuring the Response
    , createStreamResponse
    , CreateStreamResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for @CreateStream@ .
--
--
--
-- /See:/ 'createStream' smart constructor.
data CreateStream = CreateStream'
  { _csStreamName :: !Text
  , _csShardCount :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csStreamName' - A name to identify the stream. The stream name is scoped to the AWS account used by the application that creates the stream. It is also scoped by AWS Region. That is, two streams in two different AWS accounts can have the same name. Two streams in the same AWS account but in two different Regions can also have the same name.
--
-- * 'csShardCount' - The number of shards that the stream will use. The throughput of the stream is a function of the number of shards; more shards are required for greater provisioned throughput. DefaultShardLimit;
createStream
    :: Text -- ^ 'csStreamName'
    -> Natural -- ^ 'csShardCount'
    -> CreateStream
createStream pStreamName_ pShardCount_ =
  CreateStream'
    {_csStreamName = pStreamName_, _csShardCount = _Nat # pShardCount_}


-- | A name to identify the stream. The stream name is scoped to the AWS account used by the application that creates the stream. It is also scoped by AWS Region. That is, two streams in two different AWS accounts can have the same name. Two streams in the same AWS account but in two different Regions can also have the same name.
csStreamName :: Lens' CreateStream Text
csStreamName = lens _csStreamName (\ s a -> s{_csStreamName = a})

-- | The number of shards that the stream will use. The throughput of the stream is a function of the number of shards; more shards are required for greater provisioned throughput. DefaultShardLimit;
csShardCount :: Lens' CreateStream Natural
csShardCount = lens _csShardCount (\ s a -> s{_csShardCount = a}) . _Nat

instance AWSRequest CreateStream where
        type Rs CreateStream = CreateStreamResponse
        request = postJSON kinesis
        response = receiveNull CreateStreamResponse'

instance Hashable CreateStream where

instance NFData CreateStream where

instance ToHeaders CreateStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.CreateStream" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateStream where
        toJSON CreateStream'{..}
          = object
              (catMaybes
                 [Just ("StreamName" .= _csStreamName),
                  Just ("ShardCount" .= _csShardCount)])

instance ToPath CreateStream where
        toPath = const "/"

instance ToQuery CreateStream where
        toQuery = const mempty

-- | /See:/ 'createStreamResponse' smart constructor.
data CreateStreamResponse =
  CreateStreamResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStreamResponse' with the minimum fields required to make a request.
--
createStreamResponse
    :: CreateStreamResponse
createStreamResponse = CreateStreamResponse'


instance NFData CreateStreamResponse where
