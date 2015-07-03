{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Kinesis.DescribeStream
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the specified stream.
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
-- You can limit the number of returned shards using the @Limit@ parameter.
-- The number of shards in a stream may be too large to return from a
-- single call to @DescribeStream@. You can detect this by using the
-- @HasMoreShards@ flag in the returned output. @HasMoreShards@ is set to
-- @true@ when there is more data available.
--
-- @DescribeStream@ is a paginated operation. If there are more shards
-- available, you can request them using the shard ID of the last shard
-- returned. Specify this ID in the @ExclusiveStartShardId@ parameter in a
-- subsequent request to @DescribeStream@.
--
-- DescribeStream has a limit of 10 transactions per second per account.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_DescribeStream.html>
module Network.AWS.Kinesis.DescribeStream
    (
    -- * Request
      DescribeStream
    -- ** Request constructor
    , describeStream
    -- ** Request lenses
    , desExclusiveStartShardId
    , desLimit
    , desStreamName

    -- * Response
    , DescribeStreamResponse
    -- ** Response constructor
    , describeStreamResponse
    -- ** Response lenses
    , dsrStatus
    , dsrStreamDescription
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for @DescribeStream@.
--
-- /See:/ 'describeStream' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desExclusiveStartShardId'
--
-- * 'desLimit'
--
-- * 'desStreamName'
data DescribeStream = DescribeStream'
    { _desExclusiveStartShardId :: !(Maybe Text)
    , _desLimit                 :: !(Maybe Nat)
    , _desStreamName            :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeStream' smart constructor.
describeStream :: Text -> DescribeStream
describeStream pStreamName =
    DescribeStream'
    { _desExclusiveStartShardId = Nothing
    , _desLimit = Nothing
    , _desStreamName = pStreamName
    }

-- | The shard ID of the shard to start with.
desExclusiveStartShardId :: Lens' DescribeStream (Maybe Text)
desExclusiveStartShardId = lens _desExclusiveStartShardId (\ s a -> s{_desExclusiveStartShardId = a});

-- | The maximum number of shards to return.
desLimit :: Lens' DescribeStream (Maybe Natural)
desLimit = lens _desLimit (\ s a -> s{_desLimit = a}) . mapping _Nat;

-- | The name of the stream to describe.
desStreamName :: Lens' DescribeStream Text
desStreamName = lens _desStreamName (\ s a -> s{_desStreamName = a});

instance AWSPager DescribeStream where
        page rq rs
          | stop (rs ^. dsrStreamDescription . sdHasMoreShards)
            = Nothing
          | isNothing
              (rs ^?
                 dsrStreamDescription . sdShards . _last . shaShardId)
            = Nothing
          | otherwise =
            Just $ rq &
              desExclusiveStartShardId .~
                rs ^?
                  dsrStreamDescription . sdShards . _last . shaShardId

instance AWSRequest DescribeStream where
        type Sv DescribeStream = Kinesis
        type Rs DescribeStream = DescribeStreamResponse
        request = postJSON
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
              ["ExclusiveStartShardId" .=
                 _desExclusiveStartShardId,
               "Limit" .= _desLimit, "StreamName" .= _desStreamName]

instance ToPath DescribeStream where
        toPath = const "/"

instance ToQuery DescribeStream where
        toQuery = const mempty

-- | Represents the output for @DescribeStream@.
--
-- /See:/ 'describeStreamResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrStatus'
--
-- * 'dsrStreamDescription'
data DescribeStreamResponse = DescribeStreamResponse'
    { _dsrStatus            :: !Int
    , _dsrStreamDescription :: !StreamDescription
    } deriving (Eq,Read,Show)

-- | 'DescribeStreamResponse' smart constructor.
describeStreamResponse :: Int -> StreamDescription -> DescribeStreamResponse
describeStreamResponse pStatus pStreamDescription =
    DescribeStreamResponse'
    { _dsrStatus = pStatus
    , _dsrStreamDescription = pStreamDescription
    }

-- | FIXME: Undocumented member.
dsrStatus :: Lens' DescribeStreamResponse Int
dsrStatus = lens _dsrStatus (\ s a -> s{_dsrStatus = a});

-- | The current status of the stream, the stream ARN, an array of shard
-- objects that comprise the stream, and states whether there are more
-- shards available.
dsrStreamDescription :: Lens' DescribeStreamResponse StreamDescription
dsrStreamDescription = lens _dsrStreamDescription (\ s a -> s{_dsrStreamDescription = a});
