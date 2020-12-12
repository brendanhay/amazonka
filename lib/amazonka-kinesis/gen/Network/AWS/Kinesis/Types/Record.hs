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
    rEncryptionType,
    rApproximateArrivalTimestamp,
    rSequenceNumber,
    rData,
    rPartitionKey,
  )
where

import Network.AWS.Kinesis.Types.EncryptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The unit of data of the Kinesis data stream, which is composed of a sequence number, a partition key, and a data blob.
--
-- /See:/ 'mkRecord' smart constructor.
data Record = Record'
  { encryptionType :: Lude.Maybe EncryptionType,
    approximateArrivalTimestamp :: Lude.Maybe Lude.Timestamp,
    sequenceNumber :: Lude.Text,
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

-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- * 'approximateArrivalTimestamp' - The approximate time that the record was inserted into the stream.
-- * 'data'' - The data blob. The data in the blob is both opaque and immutable to Kinesis Data Streams, which does not inspect, interpret, or change the data in the blob in any way. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'encryptionType' - The encryption type used on the record. This parameter can be one of the following values:
--
--
--     * @NONE@ : Do not encrypt the records in the stream.
--
--
--     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
--
--
-- * 'partitionKey' - Identifies which shard in the stream the data record is assigned to.
-- * 'sequenceNumber' - The unique identifier of the record within its shard.
mkRecord ::
  -- | 'sequenceNumber'
  Lude.Text ->
  -- | 'data''
  Lude.Base64 ->
  -- | 'partitionKey'
  Lude.Text ->
  Record
mkRecord pSequenceNumber_ pData_ pPartitionKey_ =
  Record'
    { encryptionType = Lude.Nothing,
      approximateArrivalTimestamp = Lude.Nothing,
      sequenceNumber = pSequenceNumber_,
      data' = pData_,
      partitionKey = pPartitionKey_
    }

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
rEncryptionType :: Lens.Lens' Record (Lude.Maybe EncryptionType)
rEncryptionType = Lens.lens (encryptionType :: Record -> Lude.Maybe EncryptionType) (\s a -> s {encryptionType = a} :: Record)
{-# DEPRECATED rEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | The approximate time that the record was inserted into the stream.
--
-- /Note:/ Consider using 'approximateArrivalTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rApproximateArrivalTimestamp :: Lens.Lens' Record (Lude.Maybe Lude.Timestamp)
rApproximateArrivalTimestamp = Lens.lens (approximateArrivalTimestamp :: Record -> Lude.Maybe Lude.Timestamp) (\s a -> s {approximateArrivalTimestamp = a} :: Record)
{-# DEPRECATED rApproximateArrivalTimestamp "Use generic-lens or generic-optics with 'approximateArrivalTimestamp' instead." #-}

-- | The unique identifier of the record within its shard.
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSequenceNumber :: Lens.Lens' Record Lude.Text
rSequenceNumber = Lens.lens (sequenceNumber :: Record -> Lude.Text) (\s a -> s {sequenceNumber = a} :: Record)
{-# DEPRECATED rSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | The data blob. The data in the blob is both opaque and immutable to Kinesis Data Streams, which does not inspect, interpret, or change the data in the blob in any way. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rData :: Lens.Lens' Record Lude.Base64
rData = Lens.lens (data' :: Record -> Lude.Base64) (\s a -> s {data' = a} :: Record)
{-# DEPRECATED rData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | Identifies which shard in the stream the data record is assigned to.
--
-- /Note:/ Consider using 'partitionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPartitionKey :: Lens.Lens' Record Lude.Text
rPartitionKey = Lens.lens (partitionKey :: Record -> Lude.Text) (\s a -> s {partitionKey = a} :: Record)
{-# DEPRECATED rPartitionKey "Use generic-lens or generic-optics with 'partitionKey' instead." #-}

instance Lude.FromJSON Record where
  parseJSON =
    Lude.withObject
      "Record"
      ( \x ->
          Record'
            Lude.<$> (x Lude..:? "EncryptionType")
            Lude.<*> (x Lude..:? "ApproximateArrivalTimestamp")
            Lude.<*> (x Lude..: "SequenceNumber")
            Lude.<*> (x Lude..: "Data")
            Lude.<*> (x Lude..: "PartitionKey")
      )
