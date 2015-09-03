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
-- Module      : Network.AWS.Kinesis.DescribeStream
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified stream.
--
-- The information about the stream includes its current status, its Amazon
-- Resource Name (ARN), and an array of shard objects. For each shard
-- object, there is information about the hash key and sequence number
-- ranges that the shard spans, and the IDs of any earlier shards that
-- played in a role in creating the shard. A sequence number is the
-- identifier associated with every record ingested in the Amazon Kinesis
-- stream. The sequence number is assigned when a record is put into the
-- stream.
--
-- You can limit the number of returned shards using the 'Limit' parameter.
-- The number of shards in a stream may be too large to return from a
-- single call to 'DescribeStream'. You can detect this by using the
-- 'HasMoreShards' flag in the returned output. 'HasMoreShards' is set to
-- 'true' when there is more data available.
--
-- 'DescribeStream' is a paginated operation. If there are more shards
-- available, you can request them using the shard ID of the last shard
-- returned. Specify this ID in the 'ExclusiveStartShardId' parameter in a
-- subsequent request to 'DescribeStream'.
--
-- DescribeStream has a limit of 10 transactions per second per account.
--
-- /See:/ <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_DescribeStream.html AWS API Reference> for DescribeStream.
--
-- This operation returns paginated results.
module Network.AWS.Kinesis.DescribeStream
    (
    -- * Creating a Request
      describeStream
    , DescribeStream
    -- * Request Lenses
    , dExclusiveStartShardId
    , dLimit
    , dStreamName

    -- * Destructuring the Response
    , describeStreamResponse
    , DescribeStreamResponse
    -- * Response Lenses
    , dsrsResponseStatus
    , dsrsStreamDescription
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Kinesis.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for 'DescribeStream'.
--
-- /See:/ 'describeStream' smart constructor.
data DescribeStream = DescribeStream'
    { _dExclusiveStartShardId :: !(Maybe Text)
    , _dLimit                 :: !(Maybe Nat)
    , _dStreamName            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dExclusiveStartShardId'
--
-- * 'dLimit'
--
-- * 'dStreamName'
describeStream
    :: Text -- ^ 'dStreamName'
    -> DescribeStream
describeStream pStreamName_ =
    DescribeStream'
    { _dExclusiveStartShardId = Nothing
    , _dLimit = Nothing
    , _dStreamName = pStreamName_
    }

-- | The shard ID of the shard to start with.
dExclusiveStartShardId :: Lens' DescribeStream (Maybe Text)
dExclusiveStartShardId = lens _dExclusiveStartShardId (\ s a -> s{_dExclusiveStartShardId = a});

-- | The maximum number of shards to return.
dLimit :: Lens' DescribeStream (Maybe Natural)
dLimit = lens _dLimit (\ s a -> s{_dLimit = a}) . mapping _Nat;

-- | The name of the stream to describe.
dStreamName :: Lens' DescribeStream Text
dStreamName = lens _dStreamName (\ s a -> s{_dStreamName = a});

instance AWSPager DescribeStream where
        page rq rs
          | stop
              (rs ^?
                 dsrsStreamDescription . sdShards . _last . sShardId)
            = Nothing
          | stop (rs ^. dsrsStreamDescription . sdShards) =
            Nothing
          | otherwise =
            Just $ rq &
              dExclusiveStartShardId .~
                rs ^?
                  dsrsStreamDescription . sdShards . _last . sShardId

instance AWSRequest DescribeStream where
        type Rs DescribeStream = DescribeStreamResponse
        request = postJSON kinesis
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStreamResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "StreamDescription"))

instance ToHeaders DescribeStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.DescribeStream" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeStream where
        toJSON DescribeStream'{..}
          = object
              (catMaybes
                 [("ExclusiveStartShardId" .=) <$>
                    _dExclusiveStartShardId,
                  ("Limit" .=) <$> _dLimit,
                  Just ("StreamName" .= _dStreamName)])

instance ToPath DescribeStream where
        toPath = const "/"

instance ToQuery DescribeStream where
        toQuery = const mempty

-- | Represents the output for 'DescribeStream'.
--
-- /See:/ 'describeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
    { _dsrsResponseStatus    :: !Int
    , _dsrsStreamDescription :: !StreamDescription
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsResponseStatus'
--
-- * 'dsrsStreamDescription'
describeStreamResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> StreamDescription -- ^ 'dsrsStreamDescription'
    -> DescribeStreamResponse
describeStreamResponse pResponseStatus_ pStreamDescription_ =
    DescribeStreamResponse'
    { _dsrsResponseStatus = pResponseStatus_
    , _dsrsStreamDescription = pStreamDescription_
    }

-- | The response status code.
dsrsResponseStatus :: Lens' DescribeStreamResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a});

-- | The current status of the stream, the stream ARN, an array of shard
-- objects that comprise the stream, and states whether there are more
-- shards available.
dsrsStreamDescription :: Lens' DescribeStreamResponse StreamDescription
dsrsStreamDescription = lens _dsrsStreamDescription (\ s a -> s{_dsrsStreamDescription = a});
