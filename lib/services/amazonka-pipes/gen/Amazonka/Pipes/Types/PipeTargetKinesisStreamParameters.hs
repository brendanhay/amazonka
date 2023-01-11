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
-- Module      : Amazonka.Pipes.Types.PipeTargetKinesisStreamParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetKinesisStreamParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a Kinesis stream as a source.
--
-- /See:/ 'newPipeTargetKinesisStreamParameters' smart constructor.
data PipeTargetKinesisStreamParameters = PipeTargetKinesisStreamParameters'
  { -- | Determines which shard in the stream the data record is assigned to.
    -- Partition keys are Unicode strings with a maximum length limit of 256
    -- characters for each key. Amazon Kinesis Data Streams uses the partition
    -- key as input to a hash function that maps the partition key and
    -- associated data to a specific shard. Specifically, an MD5 hash function
    -- is used to map partition keys to 128-bit integer values and to map
    -- associated data records to shards. As a result of this hashing
    -- mechanism, all data records with the same partition key map to the same
    -- shard within the stream.
    partitionKey :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetKinesisStreamParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionKey', 'pipeTargetKinesisStreamParameters_partitionKey' - Determines which shard in the stream the data record is assigned to.
-- Partition keys are Unicode strings with a maximum length limit of 256
-- characters for each key. Amazon Kinesis Data Streams uses the partition
-- key as input to a hash function that maps the partition key and
-- associated data to a specific shard. Specifically, an MD5 hash function
-- is used to map partition keys to 128-bit integer values and to map
-- associated data records to shards. As a result of this hashing
-- mechanism, all data records with the same partition key map to the same
-- shard within the stream.
newPipeTargetKinesisStreamParameters ::
  -- | 'partitionKey'
  Prelude.Text ->
  PipeTargetKinesisStreamParameters
newPipeTargetKinesisStreamParameters pPartitionKey_ =
  PipeTargetKinesisStreamParameters'
    { partitionKey =
        Data._Sensitive Lens.# pPartitionKey_
    }

-- | Determines which shard in the stream the data record is assigned to.
-- Partition keys are Unicode strings with a maximum length limit of 256
-- characters for each key. Amazon Kinesis Data Streams uses the partition
-- key as input to a hash function that maps the partition key and
-- associated data to a specific shard. Specifically, an MD5 hash function
-- is used to map partition keys to 128-bit integer values and to map
-- associated data records to shards. As a result of this hashing
-- mechanism, all data records with the same partition key map to the same
-- shard within the stream.
pipeTargetKinesisStreamParameters_partitionKey :: Lens.Lens' PipeTargetKinesisStreamParameters Prelude.Text
pipeTargetKinesisStreamParameters_partitionKey = Lens.lens (\PipeTargetKinesisStreamParameters' {partitionKey} -> partitionKey) (\s@PipeTargetKinesisStreamParameters' {} a -> s {partitionKey = a} :: PipeTargetKinesisStreamParameters) Prelude.. Data._Sensitive

instance
  Data.FromJSON
    PipeTargetKinesisStreamParameters
  where
  parseJSON =
    Data.withObject
      "PipeTargetKinesisStreamParameters"
      ( \x ->
          PipeTargetKinesisStreamParameters'
            Prelude.<$> (x Data..: "PartitionKey")
      )

instance
  Prelude.Hashable
    PipeTargetKinesisStreamParameters
  where
  hashWithSalt
    _salt
    PipeTargetKinesisStreamParameters' {..} =
      _salt `Prelude.hashWithSalt` partitionKey

instance
  Prelude.NFData
    PipeTargetKinesisStreamParameters
  where
  rnf PipeTargetKinesisStreamParameters' {..} =
    Prelude.rnf partitionKey

instance
  Data.ToJSON
    PipeTargetKinesisStreamParameters
  where
  toJSON PipeTargetKinesisStreamParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("PartitionKey" Data..= partitionKey)]
      )
