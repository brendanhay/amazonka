{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Record
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.Record
  ( Record (..),

    -- * Smart constructor
    mkRecord,

    -- * Lenses
    rSequenceNumber,
    rData,
    rPartitionKey,
    rApproximateArrivalTimestamp,
    rEncryptionType,
  )
where

import qualified Network.AWS.Kinesis.Types.EncryptionType as Types
import qualified Network.AWS.Kinesis.Types.PartitionKey as Types
import qualified Network.AWS.Kinesis.Types.SequenceNumber as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The unit of data of the Kinesis data stream, which is composed of a sequence number, a partition key, and a data blob.
--
-- /See:/ 'mkRecord' smart constructor.
data Record = Record'
  { -- | The unique identifier of the record within its shard.
    sequenceNumber :: Types.SequenceNumber,
    -- | The data blob. The data in the blob is both opaque and immutable to Kinesis Data Streams, which does not inspect, interpret, or change the data in the blob in any way. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).
    data' :: Core.Base64,
    -- | Identifies which shard in the stream the data record is assigned to.
    partitionKey :: Types.PartitionKey,
    -- | The approximate time that the record was inserted into the stream.
    approximateArrivalTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The encryption type used on the record. This parameter can be one of the following values:
    --
    --
    --     * @NONE@ : Do not encrypt the records in the stream.
    --
    --
    --     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
    encryptionType :: Core.Maybe Types.EncryptionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Record' value with any optional fields omitted.
mkRecord ::
  -- | 'sequenceNumber'
  Types.SequenceNumber ->
  -- | 'data\''
  Core.Base64 ->
  -- | 'partitionKey'
  Types.PartitionKey ->
  Record
mkRecord sequenceNumber data' partitionKey =
  Record'
    { sequenceNumber,
      data',
      partitionKey,
      approximateArrivalTimestamp = Core.Nothing,
      encryptionType = Core.Nothing
    }

-- | The unique identifier of the record within its shard.
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSequenceNumber :: Lens.Lens' Record Types.SequenceNumber
rSequenceNumber = Lens.field @"sequenceNumber"
{-# DEPRECATED rSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | The data blob. The data in the blob is both opaque and immutable to Kinesis Data Streams, which does not inspect, interpret, or change the data in the blob in any way. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rData :: Lens.Lens' Record Core.Base64
rData = Lens.field @"data'"
{-# DEPRECATED rData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | Identifies which shard in the stream the data record is assigned to.
--
-- /Note:/ Consider using 'partitionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPartitionKey :: Lens.Lens' Record Types.PartitionKey
rPartitionKey = Lens.field @"partitionKey"
{-# DEPRECATED rPartitionKey "Use generic-lens or generic-optics with 'partitionKey' instead." #-}

-- | The approximate time that the record was inserted into the stream.
--
-- /Note:/ Consider using 'approximateArrivalTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rApproximateArrivalTimestamp :: Lens.Lens' Record (Core.Maybe Core.NominalDiffTime)
rApproximateArrivalTimestamp = Lens.field @"approximateArrivalTimestamp"
{-# DEPRECATED rApproximateArrivalTimestamp "Use generic-lens or generic-optics with 'approximateArrivalTimestamp' instead." #-}

-- | The encryption type used on the record. This parameter can be one of the following values:
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
rEncryptionType :: Lens.Lens' Record (Core.Maybe Types.EncryptionType)
rEncryptionType = Lens.field @"encryptionType"
{-# DEPRECATED rEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

instance Core.FromJSON Record where
  parseJSON =
    Core.withObject "Record" Core.$
      \x ->
        Record'
          Core.<$> (x Core..: "SequenceNumber")
          Core.<*> (x Core..: "Data")
          Core.<*> (x Core..: "PartitionKey")
          Core.<*> (x Core..:? "ApproximateArrivalTimestamp")
          Core.<*> (x Core..:? "EncryptionType")
