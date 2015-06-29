{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Kinesis.PutRecord
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

-- | Puts (writes) a single data record from a producer into an Amazon
-- Kinesis stream. Call @PutRecord@ to send data from the producer into the
-- Amazon Kinesis stream for real-time ingestion and subsequent processing,
-- one record at a time. Each shard can support up to 1000 records written
-- per second, up to a maximum total of 1 MB data written per second.
--
-- You must specify the name of the stream that captures, stores, and
-- transports the data; a partition key; and the data blob itself.
--
-- The data blob can be any type of data; for example, a segment from a log
-- file, geographic\/location data, website clickstream data, and so on.
--
-- The partition key is used by Amazon Kinesis to distribute data across
-- shards. Amazon Kinesis segregates the data records that belong to a data
-- stream into multiple shards, using the partition key associated with
-- each data record to determine which shard a given data record belongs
-- to.
--
-- Partition keys are Unicode strings, with a maximum length limit of 256
-- characters for each key. An MD5 hash function is used to map partition
-- keys to 128-bit integer values and to map associated data records to
-- shards using the hash key ranges of the shards. You can override hashing
-- the partition key to determine the shard by explicitly specifying a hash
-- value using the @ExplicitHashKey@ parameter. For more information, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html Adding Data to a Stream>
-- in the /Amazon Kinesis Developer Guide/.
--
-- @PutRecord@ returns the shard ID of where the data record was placed and
-- the sequence number that was assigned to the data record.
--
-- Sequence numbers generally increase over time. To guarantee strictly
-- increasing ordering, use the @SequenceNumberForOrdering@ parameter. For
-- more information, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html Adding Data to a Stream>
-- in the /Amazon Kinesis Developer Guide/.
--
-- If a @PutRecord@ request cannot be processed because of insufficient
-- provisioned throughput on the shard involved in the request, @PutRecord@
-- throws @ProvisionedThroughputExceededException@.
--
-- Data records are accessible for only 24 hours from the time that they
-- are added to an Amazon Kinesis stream.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_PutRecord.html>
module Network.AWS.Kinesis.PutRecord
    (
    -- * Request
      PutRecord
    -- ** Request constructor
    , putRecord
    -- ** Request lenses
    , prExplicitHashKey
    , prSequenceNumberForOrdering
    , prStreamName
    , prData
    , prPartitionKey

    -- * Response
    , PutRecordResponse
    -- ** Response constructor
    , putRecordResponse
    -- ** Response lenses
    , prrStatus
    , prrShardId
    , prrSequenceNumber
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for @PutRecord@.
--
-- /See:/ 'putRecord' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prExplicitHashKey'
--
-- * 'prSequenceNumberForOrdering'
--
-- * 'prStreamName'
--
-- * 'prData'
--
-- * 'prPartitionKey'
data PutRecord = PutRecord'
    { _prExplicitHashKey           :: !(Maybe Text)
    , _prSequenceNumberForOrdering :: !(Maybe Text)
    , _prStreamName                :: !Text
    , _prData                      :: !Base64
    , _prPartitionKey              :: !Text
    } deriving (Eq,Read,Show)

-- | 'PutRecord' smart constructor.
putRecord :: Text -> Base64 -> Text -> PutRecord
putRecord pStreamName pData pPartitionKey =
    PutRecord'
    { _prExplicitHashKey = Nothing
    , _prSequenceNumberForOrdering = Nothing
    , _prStreamName = pStreamName
    , _prData = pData
    , _prPartitionKey = pPartitionKey
    }

-- | The hash value used to explicitly determine the shard the data record is
-- assigned to by overriding the partition key hash.
prExplicitHashKey :: Lens' PutRecord (Maybe Text)
prExplicitHashKey = lens _prExplicitHashKey (\ s a -> s{_prExplicitHashKey = a});

-- | Guarantees strictly increasing sequence numbers, for puts from the same
-- client and to the same partition key. Usage: set the
-- @SequenceNumberForOrdering@ of record /n/ to the sequence number of
-- record /n-1/ (as returned in the result when putting record /n-1/). If
-- this parameter is not set, records will be coarsely ordered based on
-- arrival time.
prSequenceNumberForOrdering :: Lens' PutRecord (Maybe Text)
prSequenceNumberForOrdering = lens _prSequenceNumberForOrdering (\ s a -> s{_prSequenceNumberForOrdering = a});

-- | The name of the stream to put the data record into.
prStreamName :: Lens' PutRecord Text
prStreamName = lens _prStreamName (\ s a -> s{_prStreamName = a});

-- | The data blob to put into the record, which is base64-encoded when the
-- blob is serialized. The maximum size of the data blob (the payload
-- before base64-encoding) is 50 kilobytes (KB)
prData :: Lens' PutRecord Base64
prData = lens _prData (\ s a -> s{_prData = a});

-- | Determines which shard in the stream the data record is assigned to.
-- Partition keys are Unicode strings with a maximum length limit of 256
-- characters for each key. Amazon Kinesis uses the partition key as input
-- to a hash function that maps the partition key and associated data to a
-- specific shard. Specifically, an MD5 hash function is used to map
-- partition keys to 128-bit integer values and to map associated data
-- records to shards. As a result of this hashing mechanism, all data
-- records with the same partition key will map to the same shard within
-- the stream.
prPartitionKey :: Lens' PutRecord Text
prPartitionKey = lens _prPartitionKey (\ s a -> s{_prPartitionKey = a});

instance AWSRequest PutRecord where
        type Sv PutRecord = Kinesis
        type Rs PutRecord = PutRecordResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 PutRecordResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "ShardId") <*>
                     (x .:> "SequenceNumber"))

instance ToHeaders PutRecord where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.PutRecord" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRecord where
        toJSON PutRecord'{..}
          = object
              ["ExplicitHashKey" .= _prExplicitHashKey,
               "SequenceNumberForOrdering" .=
                 _prSequenceNumberForOrdering,
               "StreamName" .= _prStreamName, "Data" .= _prData,
               "PartitionKey" .= _prPartitionKey]

instance ToPath PutRecord where
        toPath = const "/"

instance ToQuery PutRecord where
        toQuery = const mempty

-- | Represents the output for @PutRecord@.
--
-- /See:/ 'putRecordResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prrStatus'
--
-- * 'prrShardId'
--
-- * 'prrSequenceNumber'
data PutRecordResponse = PutRecordResponse'
    { _prrStatus         :: !Int
    , _prrShardId        :: !Text
    , _prrSequenceNumber :: !Text
    } deriving (Eq,Read,Show)

-- | 'PutRecordResponse' smart constructor.
putRecordResponse :: Int -> Text -> Text -> PutRecordResponse
putRecordResponse pStatus pShardId pSequenceNumber =
    PutRecordResponse'
    { _prrStatus = pStatus
    , _prrShardId = pShardId
    , _prrSequenceNumber = pSequenceNumber
    }

-- | FIXME: Undocumented member.
prrStatus :: Lens' PutRecordResponse Int
prrStatus = lens _prrStatus (\ s a -> s{_prrStatus = a});

-- | The shard ID of the shard where the data record was placed.
prrShardId :: Lens' PutRecordResponse Text
prrShardId = lens _prrShardId (\ s a -> s{_prrShardId = a});

-- | The sequence number identifier that was assigned to the put data record.
-- The sequence number for the record is unique across all records in the
-- stream. A sequence number is the identifier associated with every record
-- put into the stream.
prrSequenceNumber :: Lens' PutRecordResponse Text
prrSequenceNumber = lens _prrSequenceNumber (\ s a -> s{_prrSequenceNumber = a});
