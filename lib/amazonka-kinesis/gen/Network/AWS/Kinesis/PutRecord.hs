{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.PutRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes a single data record into an Amazon Kinesis data stream. Call @PutRecord@ to send data into the stream for real-time ingestion and subsequent processing, one record at a time. Each shard can support writes up to 1,000 records per second, up to a maximum data write total of 1 MiB per second.
--
-- You must specify the name of the stream that captures, stores, and transports the data; a partition key; and the data blob itself.
-- The data blob can be any type of data; for example, a segment from a log file, geographic/location data, website clickstream data, and so on.
-- The partition key is used by Kinesis Data Streams to distribute data across shards. Kinesis Data Streams segregates the data records that belong to a stream into multiple shards, using the partition key associated with each data record to determine the shard to which a given data record belongs.
-- Partition keys are Unicode strings, with a maximum length limit of 256 characters for each key. An MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards using the hash key ranges of the shards. You can override hashing the partition key to determine the shard by explicitly specifying a hash value using the @ExplicitHashKey@ parameter. For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream Adding Data to a Stream> in the /Amazon Kinesis Data Streams Developer Guide/ .
-- @PutRecord@ returns the shard ID of where the data record was placed and the sequence number that was assigned to the data record.
-- Sequence numbers increase over time and are specific to a shard within a stream, not across all shards within a stream. To guarantee strictly increasing ordering, write serially to a shard and use the @SequenceNumberForOrdering@ parameter. For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream Adding Data to a Stream> in the /Amazon Kinesis Data Streams Developer Guide/ .
-- /Important:/ After you write a record to a stream, you cannot modify that record or its order within the stream.
-- If a @PutRecord@ request cannot be processed because of insufficient provisioned throughput on the shard involved in the request, @PutRecord@ throws @ProvisionedThroughputExceededException@ .
-- By default, data records are accessible for 24 hours from the time that they are added to a stream. You can use 'IncreaseStreamRetentionPeriod' or 'DecreaseStreamRetentionPeriod' to modify this retention period.
module Network.AWS.Kinesis.PutRecord
  ( -- * Creating a request
    PutRecord (..),
    mkPutRecord,

    -- ** Request lenses
    prExplicitHashKey,
    prSequenceNumberForOrdering,
    prStreamName,
    prData,
    prPartitionKey,

    -- * Destructuring the response
    PutRecordResponse (..),
    mkPutRecordResponse,

    -- ** Response lenses
    prrsEncryptionType,
    prrsResponseStatus,
    prrsShardId,
    prrsSequenceNumber,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for @PutRecord@ .
--
-- /See:/ 'mkPutRecord' smart constructor.
data PutRecord = PutRecord'
  { explicitHashKey ::
      Lude.Maybe Lude.Text,
    sequenceNumberForOrdering :: Lude.Maybe Lude.Text,
    streamName :: Lude.Text,
    data' :: Lude.Base64,
    partitionKey :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecord' with the minimum fields required to make a request.
--
-- * 'data'' - The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'explicitHashKey' - The hash value used to explicitly determine the shard the data record is assigned to by overriding the partition key hash.
-- * 'partitionKey' - Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
-- * 'sequenceNumberForOrdering' - Guarantees strictly increasing sequence numbers, for puts from the same client and to the same partition key. Usage: set the @SequenceNumberForOrdering@ of record /n/ to the sequence number of record /n-1/ (as returned in the result when putting record /n-1/ ). If this parameter is not set, records are coarsely ordered based on arrival time.
-- * 'streamName' - The name of the stream to put the data record into.
mkPutRecord ::
  -- | 'streamName'
  Lude.Text ->
  -- | 'data''
  Lude.Base64 ->
  -- | 'partitionKey'
  Lude.Text ->
  PutRecord
mkPutRecord pStreamName_ pData_ pPartitionKey_ =
  PutRecord'
    { explicitHashKey = Lude.Nothing,
      sequenceNumberForOrdering = Lude.Nothing,
      streamName = pStreamName_,
      data' = pData_,
      partitionKey = pPartitionKey_
    }

-- | The hash value used to explicitly determine the shard the data record is assigned to by overriding the partition key hash.
--
-- /Note:/ Consider using 'explicitHashKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prExplicitHashKey :: Lens.Lens' PutRecord (Lude.Maybe Lude.Text)
prExplicitHashKey = Lens.lens (explicitHashKey :: PutRecord -> Lude.Maybe Lude.Text) (\s a -> s {explicitHashKey = a} :: PutRecord)
{-# DEPRECATED prExplicitHashKey "Use generic-lens or generic-optics with 'explicitHashKey' instead." #-}

-- | Guarantees strictly increasing sequence numbers, for puts from the same client and to the same partition key. Usage: set the @SequenceNumberForOrdering@ of record /n/ to the sequence number of record /n-1/ (as returned in the result when putting record /n-1/ ). If this parameter is not set, records are coarsely ordered based on arrival time.
--
-- /Note:/ Consider using 'sequenceNumberForOrdering' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prSequenceNumberForOrdering :: Lens.Lens' PutRecord (Lude.Maybe Lude.Text)
prSequenceNumberForOrdering = Lens.lens (sequenceNumberForOrdering :: PutRecord -> Lude.Maybe Lude.Text) (\s a -> s {sequenceNumberForOrdering = a} :: PutRecord)
{-# DEPRECATED prSequenceNumberForOrdering "Use generic-lens or generic-optics with 'sequenceNumberForOrdering' instead." #-}

-- | The name of the stream to put the data record into.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prStreamName :: Lens.Lens' PutRecord Lude.Text
prStreamName = Lens.lens (streamName :: PutRecord -> Lude.Text) (\s a -> s {streamName = a} :: PutRecord)
{-# DEPRECATED prStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prData :: Lens.Lens' PutRecord Lude.Base64
prData = Lens.lens (data' :: PutRecord -> Lude.Base64) (\s a -> s {data' = a} :: PutRecord)
{-# DEPRECATED prData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
--
-- /Note:/ Consider using 'partitionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPartitionKey :: Lens.Lens' PutRecord Lude.Text
prPartitionKey = Lens.lens (partitionKey :: PutRecord -> Lude.Text) (\s a -> s {partitionKey = a} :: PutRecord)
{-# DEPRECATED prPartitionKey "Use generic-lens or generic-optics with 'partitionKey' instead." #-}

instance Lude.AWSRequest PutRecord where
  type Rs PutRecord = PutRecordResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutRecordResponse'
            Lude.<$> (x Lude..?> "EncryptionType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ShardId")
            Lude.<*> (x Lude..:> "SequenceNumber")
      )

instance Lude.ToHeaders PutRecord where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.PutRecord" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRecord where
  toJSON PutRecord' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExplicitHashKey" Lude..=) Lude.<$> explicitHashKey,
            ("SequenceNumberForOrdering" Lude..=)
              Lude.<$> sequenceNumberForOrdering,
            Lude.Just ("StreamName" Lude..= streamName),
            Lude.Just ("Data" Lude..= data'),
            Lude.Just ("PartitionKey" Lude..= partitionKey)
          ]
      )

instance Lude.ToPath PutRecord where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRecord where
  toQuery = Lude.const Lude.mempty

-- | Represents the output for @PutRecord@ .
--
-- /See:/ 'mkPutRecordResponse' smart constructor.
data PutRecordResponse = PutRecordResponse'
  { encryptionType ::
      Lude.Maybe EncryptionType,
    responseStatus :: Lude.Int,
    shardId :: Lude.Text,
    sequenceNumber :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecordResponse' with the minimum fields required to make a request.
--
-- * 'encryptionType' - The encryption type to use on the record. This parameter can be one of the following values:
--
--
--     * @NONE@ : Do not encrypt the records in the stream.
--
--
--     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
--
--
-- * 'responseStatus' - The response status code.
-- * 'sequenceNumber' - The sequence number identifier that was assigned to the put data record. The sequence number for the record is unique across all records in the stream. A sequence number is the identifier associated with every record put into the stream.
-- * 'shardId' - The shard ID of the shard where the data record was placed.
mkPutRecordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'shardId'
  Lude.Text ->
  -- | 'sequenceNumber'
  Lude.Text ->
  PutRecordResponse
mkPutRecordResponse pResponseStatus_ pShardId_ pSequenceNumber_ =
  PutRecordResponse'
    { encryptionType = Lude.Nothing,
      responseStatus = pResponseStatus_,
      shardId = pShardId_,
      sequenceNumber = pSequenceNumber_
    }

-- | The encryption type to use on the record. This parameter can be one of the following values:
--
--
--     * @NONE@ : Do not encrypt the records in the stream.
--
--
--     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
--
--
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrsEncryptionType :: Lens.Lens' PutRecordResponse (Lude.Maybe EncryptionType)
prrsEncryptionType = Lens.lens (encryptionType :: PutRecordResponse -> Lude.Maybe EncryptionType) (\s a -> s {encryptionType = a} :: PutRecordResponse)
{-# DEPRECATED prrsEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrsResponseStatus :: Lens.Lens' PutRecordResponse Lude.Int
prrsResponseStatus = Lens.lens (responseStatus :: PutRecordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutRecordResponse)
{-# DEPRECATED prrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The shard ID of the shard where the data record was placed.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrsShardId :: Lens.Lens' PutRecordResponse Lude.Text
prrsShardId = Lens.lens (shardId :: PutRecordResponse -> Lude.Text) (\s a -> s {shardId = a} :: PutRecordResponse)
{-# DEPRECATED prrsShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

-- | The sequence number identifier that was assigned to the put data record. The sequence number for the record is unique across all records in the stream. A sequence number is the identifier associated with every record put into the stream.
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrsSequenceNumber :: Lens.Lens' PutRecordResponse Lude.Text
prrsSequenceNumber = Lens.lens (sequenceNumber :: PutRecordResponse -> Lude.Text) (\s a -> s {sequenceNumber = a} :: PutRecordResponse)
{-# DEPRECATED prrsSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}
