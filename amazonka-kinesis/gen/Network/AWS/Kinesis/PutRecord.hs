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
-- Module      : Network.AWS.Kinesis.PutRecord
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts (writes) a single data record from a producer into an Amazon
-- Kinesis stream. Call 'PutRecord' to send data from the producer into the
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
-- value using the 'ExplicitHashKey' parameter. For more information, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html Adding Data to a Stream>
-- in the /Amazon Kinesis Developer Guide/.
--
-- 'PutRecord' returns the shard ID of where the data record was placed and
-- the sequence number that was assigned to the data record.
--
-- Sequence numbers generally increase over time. To guarantee strictly
-- increasing ordering, use the 'SequenceNumberForOrdering' parameter. For
-- more information, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html Adding Data to a Stream>
-- in the /Amazon Kinesis Developer Guide/.
--
-- If a 'PutRecord' request cannot be processed because of insufficient
-- provisioned throughput on the shard involved in the request, 'PutRecord'
-- throws 'ProvisionedThroughputExceededException'.
--
-- Data records are accessible for only 24 hours from the time that they
-- are added to an Amazon Kinesis stream.
--
-- /See:/ <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_PutRecord.html AWS API Reference> for PutRecord.
module Network.AWS.Kinesis.PutRecord
    (
    -- * Creating a Request
      putRecord
    , PutRecord
    -- * Request Lenses
    , prExplicitHashKey
    , prSequenceNumberForOrdering
    , prStreamName
    , prData
    , prPartitionKey

    -- * Destructuring the Response
    , putRecordResponse
    , PutRecordResponse
    -- * Response Lenses
    , prrsStatus
    , prrsShardId
    , prrsSequenceNumber
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Kinesis.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for 'PutRecord'.
--
-- /See:/ 'putRecord' smart constructor.
data PutRecord = PutRecord'
    { _prExplicitHashKey           :: !(Maybe Text)
    , _prSequenceNumberForOrdering :: !(Maybe Text)
    , _prStreamName                :: !Text
    , _prData                      :: !Base64
    , _prPartitionKey              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
putRecord
    :: Text -- ^ 'prStreamName'
    -> ByteString -- ^ 'prData'
    -> Text -- ^ 'prPartitionKey'
    -> PutRecord
putRecord pStreamName_ pData_ pPartitionKey_ =
    PutRecord'
    { _prExplicitHashKey = Nothing
    , _prSequenceNumberForOrdering = Nothing
    , _prStreamName = pStreamName_
    , _prData = _Base64 # pData_
    , _prPartitionKey = pPartitionKey_
    }

-- | The hash value used to explicitly determine the shard the data record is
-- assigned to by overriding the partition key hash.
prExplicitHashKey :: Lens' PutRecord (Maybe Text)
prExplicitHashKey = lens _prExplicitHashKey (\ s a -> s{_prExplicitHashKey = a});

-- | Guarantees strictly increasing sequence numbers, for puts from the same
-- client and to the same partition key. Usage: set the
-- 'SequenceNumberForOrdering' of record /n/ to the sequence number of
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
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
prData :: Lens' PutRecord ByteString
prData = lens _prData (\ s a -> s{_prData = a}) . _Base64;

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
        type Rs PutRecord = PutRecordResponse
        request = postJSON kinesis
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
              (catMaybes
                 [("ExplicitHashKey" .=) <$> _prExplicitHashKey,
                  ("SequenceNumberForOrdering" .=) <$>
                    _prSequenceNumberForOrdering,
                  Just ("StreamName" .= _prStreamName),
                  Just ("Data" .= _prData),
                  Just ("PartitionKey" .= _prPartitionKey)])

instance ToPath PutRecord where
        toPath = const "/"

instance ToQuery PutRecord where
        toQuery = const mempty

-- | Represents the output for 'PutRecord'.
--
-- /See:/ 'putRecordResponse' smart constructor.
data PutRecordResponse = PutRecordResponse'
    { _prrsStatus         :: !Int
    , _prrsShardId        :: !Text
    , _prrsSequenceNumber :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRecordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prrsStatus'
--
-- * 'prrsShardId'
--
-- * 'prrsSequenceNumber'
putRecordResponse
    :: Int -- ^ 'prrsStatus'
    -> Text -- ^ 'prrsShardId'
    -> Text -- ^ 'prrsSequenceNumber'
    -> PutRecordResponse
putRecordResponse pStatus_ pShardId_ pSequenceNumber_ =
    PutRecordResponse'
    { _prrsStatus = pStatus_
    , _prrsShardId = pShardId_
    , _prrsSequenceNumber = pSequenceNumber_
    }

-- | The response status code.
prrsStatus :: Lens' PutRecordResponse Int
prrsStatus = lens _prrsStatus (\ s a -> s{_prrsStatus = a});

-- | The shard ID of the shard where the data record was placed.
prrsShardId :: Lens' PutRecordResponse Text
prrsShardId = lens _prrsShardId (\ s a -> s{_prrsShardId = a});

-- | The sequence number identifier that was assigned to the put data record.
-- The sequence number for the record is unique across all records in the
-- stream. A sequence number is the identifier associated with every record
-- put into the stream.
prrsSequenceNumber :: Lens' PutRecordResponse Text
prrsSequenceNumber = lens _prrsSequenceNumber (\ s a -> s{_prrsSequenceNumber = a});
