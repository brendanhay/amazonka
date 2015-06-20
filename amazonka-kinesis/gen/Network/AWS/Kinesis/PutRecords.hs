{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Kinesis.PutRecords
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

-- | Puts (writes) multiple data records from a producer into an Amazon
-- Kinesis stream in a single call (also referred to as a @PutRecords@
-- request). Use this operation to send data from a data producer into the
-- Amazon Kinesis stream for real-time ingestion and processing. Each shard
-- can support up to 1000 records written per second, up to a maximum total
-- of 1 MB data written per second.
--
-- You must specify the name of the stream that captures, stores, and
-- transports the data; and an array of request @Records@, with each record
-- in the array requiring a partition key and data blob.
--
-- The data blob can be any type of data; for example, a segment from a log
-- file, geographic\/location data, website clickstream data, and so on.
--
-- The partition key is used by Amazon Kinesis as input to a hash function
-- that maps the partition key and associated data to a specific shard. An
-- MD5 hash function is used to map partition keys to 128-bit integer
-- values and to map associated data records to shards. As a result of this
-- hashing mechanism, all data records with the same partition key map to
-- the same shard within the stream. For more information, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html Adding Data to a Stream>
-- in the /Amazon Kinesis Developer Guide/.
--
-- Each record in the @Records@ array may include an optional parameter,
-- @ExplicitHashKey@, which overrides the partition key to shard mapping.
-- This parameter allows a data producer to determine explicitly the shard
-- where the record is stored. For more information, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html#kinesis-using-sdk-java-putrecords Adding Multiple Records with PutRecords>
-- in the /Amazon Kinesis Developer Guide/.
--
-- The @PutRecords@ response includes an array of response @Records@. Each
-- record in the response array directly correlates with a record in the
-- request array using natural ordering, from the top to the bottom of the
-- request and response. The response @Records@ array always includes the
-- same number of records as the request array.
--
-- The response @Records@ array includes both successfully and
-- unsuccessfully processed records. Amazon Kinesis attempts to process all
-- records in each @PutRecords@ request. A single record failure does not
-- stop the processing of subsequent records.
--
-- A successfully-processed record includes @ShardId@ and @SequenceNumber@
-- values. The @ShardId@ parameter identifies the shard in the stream where
-- the record is stored. The @SequenceNumber@ parameter is an identifier
-- assigned to the put record, unique to all records in the stream.
--
-- An unsuccessfully-processed record includes @ErrorCode@ and
-- @ErrorMessage@ values. @ErrorCode@ reflects the type of error and can be
-- one of the following values: @ProvisionedThroughputExceededException@ or
-- @InternalFailure@. @ErrorMessage@ provides more detailed information
-- about the @ProvisionedThroughputExceededException@ exception including
-- the account ID, stream name, and shard ID of the record that was
-- throttled. For more information about partially successful responses,
-- see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html#kinesis-using-sdk-java-putrecords Adding Multiple Records with PutRecords>
-- in the /Amazon Kinesis Developer Guide/.
--
-- Data records are accessible for only 24 hours from the time that they
-- are added to an Amazon Kinesis stream.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_PutRecords.html>
module Network.AWS.Kinesis.PutRecords
    (
    -- * Request
      PutRecords
    -- ** Request constructor
    , putRecords
    -- ** Request lenses
    , pRecordEntries
    , pStreamName

    -- * Response
    , PutRecordsResponse
    -- ** Response constructor
    , putRecordsResponse
    -- ** Response lenses
    , prrFailedRecordCount
    , prrRecords
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putRecords' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pRecordEntries'
--
-- * 'pStreamName'
data PutRecords = PutRecords'{_pRecordEntries :: List1 PutRecordsRequestEntry, _pStreamName :: Text} deriving (Eq, Read, Show)

-- | 'PutRecords' smart constructor.
putRecords :: NonEmpty PutRecordsRequestEntry -> Text -> PutRecords
putRecords pRecordEntries pStreamName = PutRecords'{_pRecordEntries = _List1 # pRecordEntries, _pStreamName = pStreamName};

-- | The records associated with the request.
pRecordEntries :: Lens' PutRecords (NonEmpty PutRecordsRequestEntry)
pRecordEntries = lens _pRecordEntries (\ s a -> s{_pRecordEntries = a}) . _List1;

-- | The stream name associated with the request.
pStreamName :: Lens' PutRecords Text
pStreamName = lens _pStreamName (\ s a -> s{_pStreamName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest PutRecords where
        type Sv PutRecords = Kinesis
        type Rs PutRecords = PutRecordsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 PutRecordsResponse' <$>
                   (x .?> "FailedRecordCount") <*> (x .:> "Records"))

instance ToHeaders PutRecords where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.PutRecords" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRecords where
        toJSON PutRecords'{..}
          = object
              ["Records" .= _pRecordEntries,
               "StreamName" .= _pStreamName]

instance ToPath PutRecords where
        toPath = const "/"

instance ToQuery PutRecords where
        toQuery = const mempty

-- | /See:/ 'putRecordsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prrFailedRecordCount'
--
-- * 'prrRecords'
data PutRecordsResponse = PutRecordsResponse'{_prrFailedRecordCount :: Maybe Nat, _prrRecords :: List1 PutRecordsResultEntry} deriving (Eq, Read, Show)

-- | 'PutRecordsResponse' smart constructor.
putRecordsResponse :: NonEmpty PutRecordsResultEntry -> PutRecordsResponse
putRecordsResponse pRecords = PutRecordsResponse'{_prrFailedRecordCount = Nothing, _prrRecords = _List1 # pRecords};

-- | The number of unsuccessfully processed records in a @PutRecords@
-- request.
prrFailedRecordCount :: Lens' PutRecordsResponse (Maybe Natural)
prrFailedRecordCount = lens _prrFailedRecordCount (\ s a -> s{_prrFailedRecordCount = a}) . mapping _Nat;

-- | An array of successfully and unsuccessfully processed record results,
-- correlated with the request by natural ordering. A record that is
-- successfully added to your Amazon Kinesis stream includes
-- @SequenceNumber@ and @ShardId@ in the result. A record that fails to be
-- added to your Amazon Kinesis stream includes @ErrorCode@ and
-- @ErrorMessage@ in the result.
prrRecords :: Lens' PutRecordsResponse (NonEmpty PutRecordsResultEntry)
prrRecords = lens _prrRecords (\ s a -> s{_prrRecords = a}) . _List1;
