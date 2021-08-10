{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.PutRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes a single data record into an Amazon Kinesis data stream. Call
-- @PutRecord@ to send data into the stream for real-time ingestion and
-- subsequent processing, one record at a time. Each shard can support
-- writes up to 1,000 records per second, up to a maximum data write total
-- of 1 MiB per second.
--
-- You must specify the name of the stream that captures, stores, and
-- transports the data; a partition key; and the data blob itself.
--
-- The data blob can be any type of data; for example, a segment from a log
-- file, geographic\/location data, website clickstream data, and so on.
--
-- The partition key is used by Kinesis Data Streams to distribute data
-- across shards. Kinesis Data Streams segregates the data records that
-- belong to a stream into multiple shards, using the partition key
-- associated with each data record to determine the shard to which a given
-- data record belongs.
--
-- Partition keys are Unicode strings, with a maximum length limit of 256
-- characters for each key. An MD5 hash function is used to map partition
-- keys to 128-bit integer values and to map associated data records to
-- shards using the hash key ranges of the shards. You can override hashing
-- the partition key to determine the shard by explicitly specifying a hash
-- value using the @ExplicitHashKey@ parameter. For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream Adding Data to a Stream>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- @PutRecord@ returns the shard ID of where the data record was placed and
-- the sequence number that was assigned to the data record.
--
-- Sequence numbers increase over time and are specific to a shard within a
-- stream, not across all shards within a stream. To guarantee strictly
-- increasing ordering, write serially to a shard and use the
-- @SequenceNumberForOrdering@ parameter. For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream Adding Data to a Stream>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- After you write a record to a stream, you cannot modify that record or
-- its order within the stream.
--
-- If a @PutRecord@ request cannot be processed because of insufficient
-- provisioned throughput on the shard involved in the request, @PutRecord@
-- throws @ProvisionedThroughputExceededException@.
--
-- By default, data records are accessible for 24 hours from the time that
-- they are added to a stream. You can use IncreaseStreamRetentionPeriod or
-- DecreaseStreamRetentionPeriod to modify this retention period.
module Network.AWS.Kinesis.PutRecord
  ( -- * Creating a Request
    PutRecord (..),
    newPutRecord,

    -- * Request Lenses
    putRecord_sequenceNumberForOrdering,
    putRecord_explicitHashKey,
    putRecord_streamName,
    putRecord_data,
    putRecord_partitionKey,

    -- * Destructuring the Response
    PutRecordResponse (..),
    newPutRecordResponse,

    -- * Response Lenses
    putRecordResponse_encryptionType,
    putRecordResponse_httpStatus,
    putRecordResponse_shardId,
    putRecordResponse_sequenceNumber,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @PutRecord@.
--
-- /See:/ 'newPutRecord' smart constructor.
data PutRecord = PutRecord'
  { -- | Guarantees strictly increasing sequence numbers, for puts from the same
    -- client and to the same partition key. Usage: set the
    -- @SequenceNumberForOrdering@ of record /n/ to the sequence number of
    -- record /n-1/ (as returned in the result when putting record /n-1/). If
    -- this parameter is not set, records are coarsely ordered based on arrival
    -- time.
    sequenceNumberForOrdering :: Prelude.Maybe Prelude.Text,
    -- | The hash value used to explicitly determine the shard the data record is
    -- assigned to by overriding the partition key hash.
    explicitHashKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream to put the data record into.
    streamName :: Prelude.Text,
    -- | The data blob to put into the record, which is base64-encoded when the
    -- blob is serialized. When the data blob (the payload before
    -- base64-encoding) is added to the partition key size, the total size must
    -- not exceed the maximum record size (1 MiB).
    data' :: Core.Base64,
    -- | Determines which shard in the stream the data record is assigned to.
    -- Partition keys are Unicode strings with a maximum length limit of 256
    -- characters for each key. Amazon Kinesis Data Streams uses the partition
    -- key as input to a hash function that maps the partition key and
    -- associated data to a specific shard. Specifically, an MD5 hash function
    -- is used to map partition keys to 128-bit integer values and to map
    -- associated data records to shards. As a result of this hashing
    -- mechanism, all data records with the same partition key map to the same
    -- shard within the stream.
    partitionKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceNumberForOrdering', 'putRecord_sequenceNumberForOrdering' - Guarantees strictly increasing sequence numbers, for puts from the same
-- client and to the same partition key. Usage: set the
-- @SequenceNumberForOrdering@ of record /n/ to the sequence number of
-- record /n-1/ (as returned in the result when putting record /n-1/). If
-- this parameter is not set, records are coarsely ordered based on arrival
-- time.
--
-- 'explicitHashKey', 'putRecord_explicitHashKey' - The hash value used to explicitly determine the shard the data record is
-- assigned to by overriding the partition key hash.
--
-- 'streamName', 'putRecord_streamName' - The name of the stream to put the data record into.
--
-- 'data'', 'putRecord_data' - The data blob to put into the record, which is base64-encoded when the
-- blob is serialized. When the data blob (the payload before
-- base64-encoding) is added to the partition key size, the total size must
-- not exceed the maximum record size (1 MiB).--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'partitionKey', 'putRecord_partitionKey' - Determines which shard in the stream the data record is assigned to.
-- Partition keys are Unicode strings with a maximum length limit of 256
-- characters for each key. Amazon Kinesis Data Streams uses the partition
-- key as input to a hash function that maps the partition key and
-- associated data to a specific shard. Specifically, an MD5 hash function
-- is used to map partition keys to 128-bit integer values and to map
-- associated data records to shards. As a result of this hashing
-- mechanism, all data records with the same partition key map to the same
-- shard within the stream.
newPutRecord ::
  -- | 'streamName'
  Prelude.Text ->
  -- | 'data''
  Prelude.ByteString ->
  -- | 'partitionKey'
  Prelude.Text ->
  PutRecord
newPutRecord pStreamName_ pData_ pPartitionKey_ =
  PutRecord'
    { sequenceNumberForOrdering =
        Prelude.Nothing,
      explicitHashKey = Prelude.Nothing,
      streamName = pStreamName_,
      data' = Core._Base64 Lens.# pData_,
      partitionKey = pPartitionKey_
    }

-- | Guarantees strictly increasing sequence numbers, for puts from the same
-- client and to the same partition key. Usage: set the
-- @SequenceNumberForOrdering@ of record /n/ to the sequence number of
-- record /n-1/ (as returned in the result when putting record /n-1/). If
-- this parameter is not set, records are coarsely ordered based on arrival
-- time.
putRecord_sequenceNumberForOrdering :: Lens.Lens' PutRecord (Prelude.Maybe Prelude.Text)
putRecord_sequenceNumberForOrdering = Lens.lens (\PutRecord' {sequenceNumberForOrdering} -> sequenceNumberForOrdering) (\s@PutRecord' {} a -> s {sequenceNumberForOrdering = a} :: PutRecord)

-- | The hash value used to explicitly determine the shard the data record is
-- assigned to by overriding the partition key hash.
putRecord_explicitHashKey :: Lens.Lens' PutRecord (Prelude.Maybe Prelude.Text)
putRecord_explicitHashKey = Lens.lens (\PutRecord' {explicitHashKey} -> explicitHashKey) (\s@PutRecord' {} a -> s {explicitHashKey = a} :: PutRecord)

-- | The name of the stream to put the data record into.
putRecord_streamName :: Lens.Lens' PutRecord Prelude.Text
putRecord_streamName = Lens.lens (\PutRecord' {streamName} -> streamName) (\s@PutRecord' {} a -> s {streamName = a} :: PutRecord)

-- | The data blob to put into the record, which is base64-encoded when the
-- blob is serialized. When the data blob (the payload before
-- base64-encoding) is added to the partition key size, the total size must
-- not exceed the maximum record size (1 MiB).--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
putRecord_data :: Lens.Lens' PutRecord Prelude.ByteString
putRecord_data = Lens.lens (\PutRecord' {data'} -> data') (\s@PutRecord' {} a -> s {data' = a} :: PutRecord) Prelude.. Core._Base64

-- | Determines which shard in the stream the data record is assigned to.
-- Partition keys are Unicode strings with a maximum length limit of 256
-- characters for each key. Amazon Kinesis Data Streams uses the partition
-- key as input to a hash function that maps the partition key and
-- associated data to a specific shard. Specifically, an MD5 hash function
-- is used to map partition keys to 128-bit integer values and to map
-- associated data records to shards. As a result of this hashing
-- mechanism, all data records with the same partition key map to the same
-- shard within the stream.
putRecord_partitionKey :: Lens.Lens' PutRecord Prelude.Text
putRecord_partitionKey = Lens.lens (\PutRecord' {partitionKey} -> partitionKey) (\s@PutRecord' {} a -> s {partitionKey = a} :: PutRecord)

instance Core.AWSRequest PutRecord where
  type AWSResponse PutRecord = PutRecordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRecordResponse'
            Prelude.<$> (x Core..?> "EncryptionType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ShardId")
            Prelude.<*> (x Core..:> "SequenceNumber")
      )

instance Prelude.Hashable PutRecord

instance Prelude.NFData PutRecord

instance Core.ToHeaders PutRecord where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("Kinesis_20131202.PutRecord" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutRecord where
  toJSON PutRecord' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SequenceNumberForOrdering" Core..=)
              Prelude.<$> sequenceNumberForOrdering,
            ("ExplicitHashKey" Core..=)
              Prelude.<$> explicitHashKey,
            Prelude.Just ("StreamName" Core..= streamName),
            Prelude.Just ("Data" Core..= data'),
            Prelude.Just ("PartitionKey" Core..= partitionKey)
          ]
      )

instance Core.ToPath PutRecord where
  toPath = Prelude.const "/"

instance Core.ToQuery PutRecord where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output for @PutRecord@.
--
-- /See:/ 'newPutRecordResponse' smart constructor.
data PutRecordResponse = PutRecordResponse'
  { -- | The encryption type to use on the record. This parameter can be one of
    -- the following values:
    --
    -- -   @NONE@: Do not encrypt the records in the stream.
    --
    -- -   @KMS@: Use server-side encryption on the records in the stream using
    --     a customer-managed AWS KMS key.
    encryptionType :: Prelude.Maybe EncryptionType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The shard ID of the shard where the data record was placed.
    shardId :: Prelude.Text,
    -- | The sequence number identifier that was assigned to the put data record.
    -- The sequence number for the record is unique across all records in the
    -- stream. A sequence number is the identifier associated with every record
    -- put into the stream.
    sequenceNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'putRecordResponse_encryptionType' - The encryption type to use on the record. This parameter can be one of
-- the following values:
--
-- -   @NONE@: Do not encrypt the records in the stream.
--
-- -   @KMS@: Use server-side encryption on the records in the stream using
--     a customer-managed AWS KMS key.
--
-- 'httpStatus', 'putRecordResponse_httpStatus' - The response's http status code.
--
-- 'shardId', 'putRecordResponse_shardId' - The shard ID of the shard where the data record was placed.
--
-- 'sequenceNumber', 'putRecordResponse_sequenceNumber' - The sequence number identifier that was assigned to the put data record.
-- The sequence number for the record is unique across all records in the
-- stream. A sequence number is the identifier associated with every record
-- put into the stream.
newPutRecordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'shardId'
  Prelude.Text ->
  -- | 'sequenceNumber'
  Prelude.Text ->
  PutRecordResponse
newPutRecordResponse
  pHttpStatus_
  pShardId_
  pSequenceNumber_ =
    PutRecordResponse'
      { encryptionType =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        shardId = pShardId_,
        sequenceNumber = pSequenceNumber_
      }

-- | The encryption type to use on the record. This parameter can be one of
-- the following values:
--
-- -   @NONE@: Do not encrypt the records in the stream.
--
-- -   @KMS@: Use server-side encryption on the records in the stream using
--     a customer-managed AWS KMS key.
putRecordResponse_encryptionType :: Lens.Lens' PutRecordResponse (Prelude.Maybe EncryptionType)
putRecordResponse_encryptionType = Lens.lens (\PutRecordResponse' {encryptionType} -> encryptionType) (\s@PutRecordResponse' {} a -> s {encryptionType = a} :: PutRecordResponse)

-- | The response's http status code.
putRecordResponse_httpStatus :: Lens.Lens' PutRecordResponse Prelude.Int
putRecordResponse_httpStatus = Lens.lens (\PutRecordResponse' {httpStatus} -> httpStatus) (\s@PutRecordResponse' {} a -> s {httpStatus = a} :: PutRecordResponse)

-- | The shard ID of the shard where the data record was placed.
putRecordResponse_shardId :: Lens.Lens' PutRecordResponse Prelude.Text
putRecordResponse_shardId = Lens.lens (\PutRecordResponse' {shardId} -> shardId) (\s@PutRecordResponse' {} a -> s {shardId = a} :: PutRecordResponse)

-- | The sequence number identifier that was assigned to the put data record.
-- The sequence number for the record is unique across all records in the
-- stream. A sequence number is the identifier associated with every record
-- put into the stream.
putRecordResponse_sequenceNumber :: Lens.Lens' PutRecordResponse Prelude.Text
putRecordResponse_sequenceNumber = Lens.lens (\PutRecordResponse' {sequenceNumber} -> sequenceNumber) (\s@PutRecordResponse' {} a -> s {sequenceNumber = a} :: PutRecordResponse)

instance Prelude.NFData PutRecordResponse
