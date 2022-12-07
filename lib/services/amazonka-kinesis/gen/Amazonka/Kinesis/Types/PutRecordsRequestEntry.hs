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
-- Module      : Amazonka.Kinesis.Types.PutRecordsRequestEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.PutRecordsRequestEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output for @PutRecords@.
--
-- /See:/ 'newPutRecordsRequestEntry' smart constructor.
data PutRecordsRequestEntry = PutRecordsRequestEntry'
  { -- | The hash value used to determine explicitly the shard that the data
    -- record is assigned to by overriding the partition key hash.
    explicitHashKey :: Prelude.Maybe Prelude.Text,
    -- | The data blob to put into the record, which is base64-encoded when the
    -- blob is serialized. When the data blob (the payload before
    -- base64-encoding) is added to the partition key size, the total size must
    -- not exceed the maximum record size (1 MiB).
    data' :: Data.Base64,
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
-- Create a value of 'PutRecordsRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'explicitHashKey', 'putRecordsRequestEntry_explicitHashKey' - The hash value used to determine explicitly the shard that the data
-- record is assigned to by overriding the partition key hash.
--
-- 'data'', 'putRecordsRequestEntry_data' - The data blob to put into the record, which is base64-encoded when the
-- blob is serialized. When the data blob (the payload before
-- base64-encoding) is added to the partition key size, the total size must
-- not exceed the maximum record size (1 MiB).--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'partitionKey', 'putRecordsRequestEntry_partitionKey' - Determines which shard in the stream the data record is assigned to.
-- Partition keys are Unicode strings with a maximum length limit of 256
-- characters for each key. Amazon Kinesis Data Streams uses the partition
-- key as input to a hash function that maps the partition key and
-- associated data to a specific shard. Specifically, an MD5 hash function
-- is used to map partition keys to 128-bit integer values and to map
-- associated data records to shards. As a result of this hashing
-- mechanism, all data records with the same partition key map to the same
-- shard within the stream.
newPutRecordsRequestEntry ::
  -- | 'data''
  Prelude.ByteString ->
  -- | 'partitionKey'
  Prelude.Text ->
  PutRecordsRequestEntry
newPutRecordsRequestEntry pData_ pPartitionKey_ =
  PutRecordsRequestEntry'
    { explicitHashKey =
        Prelude.Nothing,
      data' = Data._Base64 Lens.# pData_,
      partitionKey = pPartitionKey_
    }

-- | The hash value used to determine explicitly the shard that the data
-- record is assigned to by overriding the partition key hash.
putRecordsRequestEntry_explicitHashKey :: Lens.Lens' PutRecordsRequestEntry (Prelude.Maybe Prelude.Text)
putRecordsRequestEntry_explicitHashKey = Lens.lens (\PutRecordsRequestEntry' {explicitHashKey} -> explicitHashKey) (\s@PutRecordsRequestEntry' {} a -> s {explicitHashKey = a} :: PutRecordsRequestEntry)

-- | The data blob to put into the record, which is base64-encoded when the
-- blob is serialized. When the data blob (the payload before
-- base64-encoding) is added to the partition key size, the total size must
-- not exceed the maximum record size (1 MiB).--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
putRecordsRequestEntry_data :: Lens.Lens' PutRecordsRequestEntry Prelude.ByteString
putRecordsRequestEntry_data = Lens.lens (\PutRecordsRequestEntry' {data'} -> data') (\s@PutRecordsRequestEntry' {} a -> s {data' = a} :: PutRecordsRequestEntry) Prelude.. Data._Base64

-- | Determines which shard in the stream the data record is assigned to.
-- Partition keys are Unicode strings with a maximum length limit of 256
-- characters for each key. Amazon Kinesis Data Streams uses the partition
-- key as input to a hash function that maps the partition key and
-- associated data to a specific shard. Specifically, an MD5 hash function
-- is used to map partition keys to 128-bit integer values and to map
-- associated data records to shards. As a result of this hashing
-- mechanism, all data records with the same partition key map to the same
-- shard within the stream.
putRecordsRequestEntry_partitionKey :: Lens.Lens' PutRecordsRequestEntry Prelude.Text
putRecordsRequestEntry_partitionKey = Lens.lens (\PutRecordsRequestEntry' {partitionKey} -> partitionKey) (\s@PutRecordsRequestEntry' {} a -> s {partitionKey = a} :: PutRecordsRequestEntry)

instance Prelude.Hashable PutRecordsRequestEntry where
  hashWithSalt _salt PutRecordsRequestEntry' {..} =
    _salt `Prelude.hashWithSalt` explicitHashKey
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` partitionKey

instance Prelude.NFData PutRecordsRequestEntry where
  rnf PutRecordsRequestEntry' {..} =
    Prelude.rnf explicitHashKey
      `Prelude.seq` Prelude.rnf data'
      `Prelude.seq` Prelude.rnf partitionKey

instance Data.ToJSON PutRecordsRequestEntry where
  toJSON PutRecordsRequestEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExplicitHashKey" Data..=)
              Prelude.<$> explicitHashKey,
            Prelude.Just ("Data" Data..= data'),
            Prelude.Just ("PartitionKey" Data..= partitionKey)
          ]
      )
