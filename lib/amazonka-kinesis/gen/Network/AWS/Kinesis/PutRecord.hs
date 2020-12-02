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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes a single data record into an Amazon Kinesis data stream. Call @PutRecord@ to send data into the stream for real-time ingestion and subsequent processing, one record at a time. Each shard can support writes up to 1,000 records per second, up to a maximum data write total of 1 MB per second.
--
--
-- You must specify the name of the stream that captures, stores, and transports the data; a partition key; and the data blob itself.
--
-- The data blob can be any type of data; for example, a segment from a log file, geographic/location data, website clickstream data, and so on.
--
-- The partition key is used by Kinesis Data Streams to distribute data across shards. Kinesis Data Streams segregates the data records that belong to a stream into multiple shards, using the partition key associated with each data record to determine the shard to which a given data record belongs.
--
-- Partition keys are Unicode strings, with a maximum length limit of 256 characters for each key. An MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards using the hash key ranges of the shards. You can override hashing the partition key to determine the shard by explicitly specifying a hash value using the @ExplicitHashKey@ parameter. For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream Adding Data to a Stream> in the /Amazon Kinesis Data Streams Developer Guide/ .
--
-- @PutRecord@ returns the shard ID of where the data record was placed and the sequence number that was assigned to the data record.
--
-- Sequence numbers increase over time and are specific to a shard within a stream, not across all shards within a stream. To guarantee strictly increasing ordering, write serially to a shard and use the @SequenceNumberForOrdering@ parameter. For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream Adding Data to a Stream> in the /Amazon Kinesis Data Streams Developer Guide/ .
--
-- If a @PutRecord@ request cannot be processed because of insufficient provisioned throughput on the shard involved in the request, @PutRecord@ throws @ProvisionedThroughputExceededException@ .
--
-- By default, data records are accessible for 24 hours from the time that they are added to a stream. You can use 'IncreaseStreamRetentionPeriod' or 'DecreaseStreamRetentionPeriod' to modify this retention period.
--
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
    , prrsEncryptionType
    , prrsResponseStatus
    , prrsShardId
    , prrsSequenceNumber
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for @PutRecord@ .
--
--
--
-- /See:/ 'putRecord' smart constructor.
data PutRecord = PutRecord'
  { _prExplicitHashKey           :: !(Maybe Text)
  , _prSequenceNumberForOrdering :: !(Maybe Text)
  , _prStreamName                :: !Text
  , _prData                      :: !Base64
  , _prPartitionKey              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prExplicitHashKey' - The hash value used to explicitly determine the shard the data record is assigned to by overriding the partition key hash.
--
-- * 'prSequenceNumberForOrdering' - Guarantees strictly increasing sequence numbers, for puts from the same client and to the same partition key. Usage: set the @SequenceNumberForOrdering@ of record /n/ to the sequence number of record /n-1/ (as returned in the result when putting record /n-1/ ). If this parameter is not set, records are coarsely ordered based on arrival time.
--
-- * 'prStreamName' - The name of the stream to put the data record into.
--
-- * 'prData' - The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MB).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'prPartitionKey' - Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
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


-- | The hash value used to explicitly determine the shard the data record is assigned to by overriding the partition key hash.
prExplicitHashKey :: Lens' PutRecord (Maybe Text)
prExplicitHashKey = lens _prExplicitHashKey (\ s a -> s{_prExplicitHashKey = a})

-- | Guarantees strictly increasing sequence numbers, for puts from the same client and to the same partition key. Usage: set the @SequenceNumberForOrdering@ of record /n/ to the sequence number of record /n-1/ (as returned in the result when putting record /n-1/ ). If this parameter is not set, records are coarsely ordered based on arrival time.
prSequenceNumberForOrdering :: Lens' PutRecord (Maybe Text)
prSequenceNumberForOrdering = lens _prSequenceNumberForOrdering (\ s a -> s{_prSequenceNumberForOrdering = a})

-- | The name of the stream to put the data record into.
prStreamName :: Lens' PutRecord Text
prStreamName = lens _prStreamName (\ s a -> s{_prStreamName = a})

-- | The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MB).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
prData :: Lens' PutRecord ByteString
prData = lens _prData (\ s a -> s{_prData = a}) . _Base64

-- | Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
prPartitionKey :: Lens' PutRecord Text
prPartitionKey = lens _prPartitionKey (\ s a -> s{_prPartitionKey = a})

instance AWSRequest PutRecord where
        type Rs PutRecord = PutRecordResponse
        request = postJSON kinesis
        response
          = receiveJSON
              (\ s h x ->
                 PutRecordResponse' <$>
                   (x .?> "EncryptionType") <*> (pure (fromEnum s)) <*>
                     (x .:> "ShardId")
                     <*> (x .:> "SequenceNumber"))

instance Hashable PutRecord where

instance NFData PutRecord where

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

-- | Represents the output for @PutRecord@ .
--
--
--
-- /See:/ 'putRecordResponse' smart constructor.
data PutRecordResponse = PutRecordResponse'
  { _prrsEncryptionType :: !(Maybe EncryptionType)
  , _prrsResponseStatus :: !Int
  , _prrsShardId        :: !Text
  , _prrsSequenceNumber :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRecordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prrsEncryptionType' - The encryption type to use on the record. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records in the stream.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
--
-- * 'prrsResponseStatus' - -- | The response status code.
--
-- * 'prrsShardId' - The shard ID of the shard where the data record was placed.
--
-- * 'prrsSequenceNumber' - The sequence number identifier that was assigned to the put data record. The sequence number for the record is unique across all records in the stream. A sequence number is the identifier associated with every record put into the stream.
putRecordResponse
    :: Int -- ^ 'prrsResponseStatus'
    -> Text -- ^ 'prrsShardId'
    -> Text -- ^ 'prrsSequenceNumber'
    -> PutRecordResponse
putRecordResponse pResponseStatus_ pShardId_ pSequenceNumber_ =
  PutRecordResponse'
    { _prrsEncryptionType = Nothing
    , _prrsResponseStatus = pResponseStatus_
    , _prrsShardId = pShardId_
    , _prrsSequenceNumber = pSequenceNumber_
    }


-- | The encryption type to use on the record. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records in the stream.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
prrsEncryptionType :: Lens' PutRecordResponse (Maybe EncryptionType)
prrsEncryptionType = lens _prrsEncryptionType (\ s a -> s{_prrsEncryptionType = a})

-- | -- | The response status code.
prrsResponseStatus :: Lens' PutRecordResponse Int
prrsResponseStatus = lens _prrsResponseStatus (\ s a -> s{_prrsResponseStatus = a})

-- | The shard ID of the shard where the data record was placed.
prrsShardId :: Lens' PutRecordResponse Text
prrsShardId = lens _prrsShardId (\ s a -> s{_prrsShardId = a})

-- | The sequence number identifier that was assigned to the put data record. The sequence number for the record is unique across all records in the stream. A sequence number is the identifier associated with every record put into the stream.
prrsSequenceNumber :: Lens' PutRecordResponse Text
prrsSequenceNumber = lens _prrsSequenceNumber (\ s a -> s{_prrsSequenceNumber = a})

instance NFData PutRecordResponse where
