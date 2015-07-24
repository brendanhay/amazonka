{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.GetRecords
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets data records from a shard.
--
-- Specify a shard iterator using the @ShardIterator@ parameter. The shard
-- iterator specifies the position in the shard from which you want to
-- start reading data records sequentially. If there are no records
-- available in the portion of the shard that the iterator points to,
-- GetRecords returns an empty list. Note that it might take multiple calls
-- to get to a portion of the shard that contains records.
--
-- You can scale by provisioning multiple shards. Your application should
-- have one thread per shard, each reading continuously from its stream. To
-- read from a stream continually, call GetRecords in a loop. Use
-- GetShardIterator to get the shard iterator to specify in the first
-- GetRecords call. GetRecords returns a new shard iterator in
-- @NextShardIterator@. Specify the shard iterator returned in
-- @NextShardIterator@ in subsequent calls to GetRecords. Note that if the
-- shard has been closed, the shard iterator can\'t return more data and
-- GetRecords returns @null@ in @NextShardIterator@. You can terminate the
-- loop when the shard is closed, or when the shard iterator reaches the
-- record with the sequence number or other attribute that marks it as the
-- last record to process.
--
-- Each data record can be up to 50 KB in size, and each shard can read up
-- to 2 MB per second. You can ensure that your calls don\'t exceed the
-- maximum supported size or throughput by using the @Limit@ parameter to
-- specify the maximum number of records that GetRecords can return.
-- Consider your average record size when determining this limit. For
-- example, if your average record size is 40 KB, you can limit the data
-- returned to about 1 MB per call by specifying 25 as the limit.
--
-- The size of the data returned by GetRecords will vary depending on the
-- utilization of the shard. The maximum size of data that GetRecords can
-- return is 10 MB. If a call returns this amount of data, subsequent calls
-- made within the next 5 seconds throw
-- @ProvisionedThroughputExceededException@. If there is insufficient
-- provisioned throughput on the shard, subsequent calls made within the
-- next 1 second throw @ProvisionedThroughputExceededException@. Note that
-- GetRecords won\'t return any data when it throws an exception. For this
-- reason, we recommend that you wait one second between calls to
-- GetRecords; however, it\'s possible that the application will get
-- exceptions for longer than 1 second.
--
-- To detect whether the application is falling behind in processing, you
-- can use the @MillisBehindLatest@ response attribute. You can also
-- monitor the amount of data in a stream using the CloudWatch metrics. For
-- more information, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/monitoring_with_cloudwatch.html Monitoring Amazon Kinesis with Amazon CloudWatch>
-- in the /Amazon Kinesis Developer Guide/.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html>
module Network.AWS.Kinesis.GetRecords
    (
    -- * Request
      GetRecords
    -- ** Request constructor
    , getRecords
    -- ** Request lenses
    , grLimit
    , grShardIterator

    -- * Response
    , GetRecordsResponse
    -- ** Response constructor
    , getRecordsResponse
    -- ** Response lenses
    , grrsMillisBehindLatest
    , grrsNextShardIterator
    , grrsStatus
    , grrsRecords
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for GetRecords.
--
-- /See:/ 'getRecords' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grLimit'
--
-- * 'grShardIterator'
data GetRecords = GetRecords'
    { _grLimit         :: !(Maybe Nat)
    , _grShardIterator :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRecords' smart constructor.
getRecords :: Text -> GetRecords
getRecords pShardIterator_ =
    GetRecords'
    { _grLimit = Nothing
    , _grShardIterator = pShardIterator_
    }

-- | The maximum number of records to return. Specify a value of up to
-- 10,000. If you specify a value that is greater than 10,000, GetRecords
-- throws @InvalidArgumentException@.
grLimit :: Lens' GetRecords (Maybe Natural)
grLimit = lens _grLimit (\ s a -> s{_grLimit = a}) . mapping _Nat;

-- | The position in the shard from which you want to start sequentially
-- reading data records. A shard iterator specifies this position using the
-- sequence number of a data record in the shard.
grShardIterator :: Lens' GetRecords Text
grShardIterator = lens _grShardIterator (\ s a -> s{_grShardIterator = a});

instance AWSRequest GetRecords where
        type Sv GetRecords = Kinesis
        type Rs GetRecords = GetRecordsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetRecordsResponse' <$>
                   (x .?> "MillisBehindLatest") <*>
                     (x .?> "NextShardIterator")
                     <*> (pure (fromEnum s))
                     <*> (x .?> "Records" .!@ mempty))

instance ToHeaders GetRecords where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.GetRecords" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRecords where
        toJSON GetRecords'{..}
          = object
              ["Limit" .= _grLimit,
               "ShardIterator" .= _grShardIterator]

instance ToPath GetRecords where
        toPath = const "/"

instance ToQuery GetRecords where
        toQuery = const mempty

-- | Represents the output for GetRecords.
--
-- /See:/ 'getRecordsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrsMillisBehindLatest'
--
-- * 'grrsNextShardIterator'
--
-- * 'grrsStatus'
--
-- * 'grrsRecords'
data GetRecordsResponse = GetRecordsResponse'
    { _grrsMillisBehindLatest :: !(Maybe Nat)
    , _grrsNextShardIterator  :: !(Maybe Text)
    , _grrsStatus             :: !Int
    , _grrsRecords            :: ![Record]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRecordsResponse' smart constructor.
getRecordsResponse :: Int -> GetRecordsResponse
getRecordsResponse pStatus_ =
    GetRecordsResponse'
    { _grrsMillisBehindLatest = Nothing
    , _grrsNextShardIterator = Nothing
    , _grrsStatus = pStatus_
    , _grrsRecords = mempty
    }

-- | The number of milliseconds the GetRecords response is from the tip of
-- the stream, indicating how far behind current time the consumer is. A
-- value of zero indicates record processing is caught up, and there are no
-- new records to process at this moment.
grrsMillisBehindLatest :: Lens' GetRecordsResponse (Maybe Natural)
grrsMillisBehindLatest = lens _grrsMillisBehindLatest (\ s a -> s{_grrsMillisBehindLatest = a}) . mapping _Nat;

-- | The next position in the shard from which to start sequentially reading
-- data records. If set to @null@, the shard has been closed and the
-- requested iterator will not return any more data.
grrsNextShardIterator :: Lens' GetRecordsResponse (Maybe Text)
grrsNextShardIterator = lens _grrsNextShardIterator (\ s a -> s{_grrsNextShardIterator = a});

-- | FIXME: Undocumented member.
grrsStatus :: Lens' GetRecordsResponse Int
grrsStatus = lens _grrsStatus (\ s a -> s{_grrsStatus = a});

-- | The data records retrieved from the shard.
grrsRecords :: Lens' GetRecordsResponse [Record]
grrsRecords = lens _grrsRecords (\ s a -> s{_grrsRecords = a}) . _Coerce;
