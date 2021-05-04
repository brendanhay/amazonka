{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Record
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.Record where

import Network.AWS.Kinesis.Types.EncryptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The unit of data of the Kinesis data stream, which is composed of a
-- sequence number, a partition key, and a data blob.
--
-- /See:/ 'newRecord' smart constructor.
data Record = Record'
  { -- | The encryption type used on the record. This parameter can be one of the
    -- following values:
    --
    -- -   @NONE@: Do not encrypt the records in the stream.
    --
    -- -   @KMS@: Use server-side encryption on the records in the stream using
    --     a customer-managed AWS KMS key.
    encryptionType :: Prelude.Maybe EncryptionType,
    -- | The approximate time that the record was inserted into the stream.
    approximateArrivalTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The unique identifier of the record within its shard.
    sequenceNumber :: Prelude.Text,
    -- | The data blob. The data in the blob is both opaque and immutable to
    -- Kinesis Data Streams, which does not inspect, interpret, or change the
    -- data in the blob in any way. When the data blob (the payload before
    -- base64-encoding) is added to the partition key size, the total size must
    -- not exceed the maximum record size (1 MiB).
    data' :: Prelude.Base64,
    -- | Identifies which shard in the stream the data record is assigned to.
    partitionKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Record' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'record_encryptionType' - The encryption type used on the record. This parameter can be one of the
-- following values:
--
-- -   @NONE@: Do not encrypt the records in the stream.
--
-- -   @KMS@: Use server-side encryption on the records in the stream using
--     a customer-managed AWS KMS key.
--
-- 'approximateArrivalTimestamp', 'record_approximateArrivalTimestamp' - The approximate time that the record was inserted into the stream.
--
-- 'sequenceNumber', 'record_sequenceNumber' - The unique identifier of the record within its shard.
--
-- 'data'', 'record_data' - The data blob. The data in the blob is both opaque and immutable to
-- Kinesis Data Streams, which does not inspect, interpret, or change the
-- data in the blob in any way. When the data blob (the payload before
-- base64-encoding) is added to the partition key size, the total size must
-- not exceed the maximum record size (1 MiB).--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'partitionKey', 'record_partitionKey' - Identifies which shard in the stream the data record is assigned to.
newRecord ::
  -- | 'sequenceNumber'
  Prelude.Text ->
  -- | 'data''
  Prelude.ByteString ->
  -- | 'partitionKey'
  Prelude.Text ->
  Record
newRecord pSequenceNumber_ pData_ pPartitionKey_ =
  Record'
    { encryptionType = Prelude.Nothing,
      approximateArrivalTimestamp = Prelude.Nothing,
      sequenceNumber = pSequenceNumber_,
      data' = Prelude._Base64 Lens.# pData_,
      partitionKey = pPartitionKey_
    }

-- | The encryption type used on the record. This parameter can be one of the
-- following values:
--
-- -   @NONE@: Do not encrypt the records in the stream.
--
-- -   @KMS@: Use server-side encryption on the records in the stream using
--     a customer-managed AWS KMS key.
record_encryptionType :: Lens.Lens' Record (Prelude.Maybe EncryptionType)
record_encryptionType = Lens.lens (\Record' {encryptionType} -> encryptionType) (\s@Record' {} a -> s {encryptionType = a} :: Record)

-- | The approximate time that the record was inserted into the stream.
record_approximateArrivalTimestamp :: Lens.Lens' Record (Prelude.Maybe Prelude.UTCTime)
record_approximateArrivalTimestamp = Lens.lens (\Record' {approximateArrivalTimestamp} -> approximateArrivalTimestamp) (\s@Record' {} a -> s {approximateArrivalTimestamp = a} :: Record) Prelude.. Lens.mapping Prelude._Time

-- | The unique identifier of the record within its shard.
record_sequenceNumber :: Lens.Lens' Record Prelude.Text
record_sequenceNumber = Lens.lens (\Record' {sequenceNumber} -> sequenceNumber) (\s@Record' {} a -> s {sequenceNumber = a} :: Record)

-- | The data blob. The data in the blob is both opaque and immutable to
-- Kinesis Data Streams, which does not inspect, interpret, or change the
-- data in the blob in any way. When the data blob (the payload before
-- base64-encoding) is added to the partition key size, the total size must
-- not exceed the maximum record size (1 MiB).--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
record_data :: Lens.Lens' Record Prelude.ByteString
record_data = Lens.lens (\Record' {data'} -> data') (\s@Record' {} a -> s {data' = a} :: Record) Prelude.. Prelude._Base64

-- | Identifies which shard in the stream the data record is assigned to.
record_partitionKey :: Lens.Lens' Record Prelude.Text
record_partitionKey = Lens.lens (\Record' {partitionKey} -> partitionKey) (\s@Record' {} a -> s {partitionKey = a} :: Record)

instance Prelude.FromJSON Record where
  parseJSON =
    Prelude.withObject
      "Record"
      ( \x ->
          Record'
            Prelude.<$> (x Prelude..:? "EncryptionType")
            Prelude.<*> (x Prelude..:? "ApproximateArrivalTimestamp")
            Prelude.<*> (x Prelude..: "SequenceNumber")
            Prelude.<*> (x Prelude..: "Data")
            Prelude.<*> (x Prelude..: "PartitionKey")
      )

instance Prelude.Hashable Record

instance Prelude.NFData Record
