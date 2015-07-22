{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DescribeStream
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
    , drqExclusiveStartShardId
    , drqLimit
    , drqStreamName

    -- * Response
    , DescribeStreamResponse
    -- ** Response constructor
    , describeStreamResponse
    -- ** Response lenses
    , drsStatus
    , drsStreamDescription
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
-- * 'drqExclusiveStartShardId'
--
-- * 'drqLimit'
--
-- * 'drqStreamName'
data DescribeStream = DescribeStream'
    { _drqExclusiveStartShardId :: !(Maybe Text)
    , _drqLimit                 :: !(Maybe Nat)
    , _drqStreamName            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStream' smart constructor.
describeStream :: Text -> DescribeStream
describeStream pStreamName =
    DescribeStream'
    { _drqExclusiveStartShardId = Nothing
    , _drqLimit = Nothing
    , _drqStreamName = pStreamName
    }

-- | The shard ID of the shard to start with.
drqExclusiveStartShardId :: Lens' DescribeStream (Maybe Text)
drqExclusiveStartShardId = lens _drqExclusiveStartShardId (\ s a -> s{_drqExclusiveStartShardId = a});

-- | The maximum number of shards to return.
drqLimit :: Lens' DescribeStream (Maybe Natural)
drqLimit = lens _drqLimit (\ s a -> s{_drqLimit = a}) . mapping _Nat;

-- | The name of the stream to describe.
drqStreamName :: Lens' DescribeStream Text
drqStreamName = lens _drqStreamName (\ s a -> s{_drqStreamName = a});

instance AWSPager DescribeStream where
        page rq rs
          | stop (rs ^. drsStreamDescription . sdHasMoreShards)
            = Nothing
          | isNothing
              (rs ^?
                 drsStreamDescription . sdShards . _last . sShardId)
            = Nothing
          | otherwise =
            Just $ rq &
              drqExclusiveStartShardId .~
                rs ^?
                  drsStreamDescription . sdShards . _last . sShardId

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
                 _drqExclusiveStartShardId,
               "Limit" .= _drqLimit, "StreamName" .= _drqStreamName]

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
-- * 'drsStatus'
--
-- * 'drsStreamDescription'
data DescribeStreamResponse = DescribeStreamResponse'
    { _drsStatus            :: !Int
    , _drsStreamDescription :: !StreamDescription
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStreamResponse' smart constructor.
describeStreamResponse :: Int -> StreamDescription -> DescribeStreamResponse
describeStreamResponse pStatus pStreamDescription =
    DescribeStreamResponse'
    { _drsStatus = pStatus
    , _drsStreamDescription = pStreamDescription
    }

-- | FIXME: Undocumented member.
drsStatus :: Lens' DescribeStreamResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});

-- | The current status of the stream, the stream ARN, an array of shard
-- objects that comprise the stream, and states whether there are more
-- shards available.
drsStreamDescription :: Lens' DescribeStreamResponse StreamDescription
drsStreamDescription = lens _drsStreamDescription (\ s a -> s{_drsStreamDescription = a});
