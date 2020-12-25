{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    prStreamName,
    prData,
    prPartitionKey,
    prExplicitHashKey,
    prSequenceNumberForOrdering,

    -- * Destructuring the response
    PutRecordResponse (..),
    mkPutRecordResponse,

    -- ** Response lenses
    prrrsShardId,
    prrrsSequenceNumber,
    prrrsEncryptionType,
    prrrsResponseStatus,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @PutRecord@ .
--
-- /See:/ 'mkPutRecord' smart constructor.
data PutRecord = PutRecord'
  { -- | The name of the stream to put the data record into.
    streamName :: Types.StreamName,
    -- | The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).
    data' :: Core.Base64,
    -- | Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
    partitionKey :: Types.PartitionKey,
    -- | The hash value used to explicitly determine the shard the data record is assigned to by overriding the partition key hash.
    explicitHashKey :: Core.Maybe Types.ExplicitHashKey,
    -- | Guarantees strictly increasing sequence numbers, for puts from the same client and to the same partition key. Usage: set the @SequenceNumberForOrdering@ of record /n/ to the sequence number of record /n-1/ (as returned in the result when putting record /n-1/ ). If this parameter is not set, records are coarsely ordered based on arrival time.
    sequenceNumberForOrdering :: Core.Maybe Types.SequenceNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecord' value with any optional fields omitted.
mkPutRecord ::
  -- | 'streamName'
  Types.StreamName ->
  -- | 'data\''
  Core.Base64 ->
  -- | 'partitionKey'
  Types.PartitionKey ->
  PutRecord
mkPutRecord streamName data' partitionKey =
  PutRecord'
    { streamName,
      data',
      partitionKey,
      explicitHashKey = Core.Nothing,
      sequenceNumberForOrdering = Core.Nothing
    }

-- | The name of the stream to put the data record into.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prStreamName :: Lens.Lens' PutRecord Types.StreamName
prStreamName = Lens.field @"streamName"
{-# DEPRECATED prStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prData :: Lens.Lens' PutRecord Core.Base64
prData = Lens.field @"data'"
{-# DEPRECATED prData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
--
-- /Note:/ Consider using 'partitionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPartitionKey :: Lens.Lens' PutRecord Types.PartitionKey
prPartitionKey = Lens.field @"partitionKey"
{-# DEPRECATED prPartitionKey "Use generic-lens or generic-optics with 'partitionKey' instead." #-}

-- | The hash value used to explicitly determine the shard the data record is assigned to by overriding the partition key hash.
--
-- /Note:/ Consider using 'explicitHashKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prExplicitHashKey :: Lens.Lens' PutRecord (Core.Maybe Types.ExplicitHashKey)
prExplicitHashKey = Lens.field @"explicitHashKey"
{-# DEPRECATED prExplicitHashKey "Use generic-lens or generic-optics with 'explicitHashKey' instead." #-}

-- | Guarantees strictly increasing sequence numbers, for puts from the same client and to the same partition key. Usage: set the @SequenceNumberForOrdering@ of record /n/ to the sequence number of record /n-1/ (as returned in the result when putting record /n-1/ ). If this parameter is not set, records are coarsely ordered based on arrival time.
--
-- /Note:/ Consider using 'sequenceNumberForOrdering' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prSequenceNumberForOrdering :: Lens.Lens' PutRecord (Core.Maybe Types.SequenceNumber)
prSequenceNumberForOrdering = Lens.field @"sequenceNumberForOrdering"
{-# DEPRECATED prSequenceNumberForOrdering "Use generic-lens or generic-optics with 'sequenceNumberForOrdering' instead." #-}

instance Core.FromJSON PutRecord where
  toJSON PutRecord {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just ("Data" Core..= data'),
            Core.Just ("PartitionKey" Core..= partitionKey),
            ("ExplicitHashKey" Core..=) Core.<$> explicitHashKey,
            ("SequenceNumberForOrdering" Core..=)
              Core.<$> sequenceNumberForOrdering
          ]
      )

instance Core.AWSRequest PutRecord where
  type Rs PutRecord = PutRecordResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Kinesis_20131202.PutRecord")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRecordResponse'
            Core.<$> (x Core..: "ShardId")
            Core.<*> (x Core..: "SequenceNumber")
            Core.<*> (x Core..:? "EncryptionType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output for @PutRecord@ .
--
-- /See:/ 'mkPutRecordResponse' smart constructor.
data PutRecordResponse = PutRecordResponse'
  { -- | The shard ID of the shard where the data record was placed.
    shardId :: Types.ShardId,
    -- | The sequence number identifier that was assigned to the put data record. The sequence number for the record is unique across all records in the stream. A sequence number is the identifier associated with every record put into the stream.
    sequenceNumber :: Types.SequenceNumber,
    -- | The encryption type to use on the record. This parameter can be one of the following values:
    --
    --
    --     * @NONE@ : Do not encrypt the records in the stream.
    --
    --
    --     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
    encryptionType :: Core.Maybe Types.EncryptionType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecordResponse' value with any optional fields omitted.
mkPutRecordResponse ::
  -- | 'shardId'
  Types.ShardId ->
  -- | 'sequenceNumber'
  Types.SequenceNumber ->
  -- | 'responseStatus'
  Core.Int ->
  PutRecordResponse
mkPutRecordResponse shardId sequenceNumber responseStatus =
  PutRecordResponse'
    { shardId,
      sequenceNumber,
      encryptionType = Core.Nothing,
      responseStatus
    }

-- | The shard ID of the shard where the data record was placed.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrsShardId :: Lens.Lens' PutRecordResponse Types.ShardId
prrrsShardId = Lens.field @"shardId"
{-# DEPRECATED prrrsShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

-- | The sequence number identifier that was assigned to the put data record. The sequence number for the record is unique across all records in the stream. A sequence number is the identifier associated with every record put into the stream.
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrsSequenceNumber :: Lens.Lens' PutRecordResponse Types.SequenceNumber
prrrsSequenceNumber = Lens.field @"sequenceNumber"
{-# DEPRECATED prrrsSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

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
prrrsEncryptionType :: Lens.Lens' PutRecordResponse (Core.Maybe Types.EncryptionType)
prrrsEncryptionType = Lens.field @"encryptionType"
{-# DEPRECATED prrrsEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrsResponseStatus :: Lens.Lens' PutRecordResponse Core.Int
prrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
