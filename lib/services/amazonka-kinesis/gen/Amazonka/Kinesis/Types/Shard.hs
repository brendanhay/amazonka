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
-- Module      : Amazonka.Kinesis.Types.Shard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.Shard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types.HashKeyRange
import Amazonka.Kinesis.Types.SequenceNumberRange
import qualified Amazonka.Prelude as Prelude

-- | A uniquely identified group of data records in a Kinesis data stream.
--
-- /See:/ 'newShard' smart constructor.
data Shard = Shard'
  { -- | The shard ID of the shard adjacent to the shard\'s parent.
    adjacentParentShardId :: Prelude.Maybe Prelude.Text,
    -- | The shard ID of the shard\'s parent.
    parentShardId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the shard within the stream.
    shardId :: Prelude.Text,
    -- | The range of possible hash key values for the shard, which is a set of
    -- ordered contiguous positive integers.
    hashKeyRange :: HashKeyRange,
    -- | The range of possible sequence numbers for the shard.
    sequenceNumberRange :: SequenceNumberRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Shard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adjacentParentShardId', 'shard_adjacentParentShardId' - The shard ID of the shard adjacent to the shard\'s parent.
--
-- 'parentShardId', 'shard_parentShardId' - The shard ID of the shard\'s parent.
--
-- 'shardId', 'shard_shardId' - The unique identifier of the shard within the stream.
--
-- 'hashKeyRange', 'shard_hashKeyRange' - The range of possible hash key values for the shard, which is a set of
-- ordered contiguous positive integers.
--
-- 'sequenceNumberRange', 'shard_sequenceNumberRange' - The range of possible sequence numbers for the shard.
newShard ::
  -- | 'shardId'
  Prelude.Text ->
  -- | 'hashKeyRange'
  HashKeyRange ->
  -- | 'sequenceNumberRange'
  SequenceNumberRange ->
  Shard
newShard
  pShardId_
  pHashKeyRange_
  pSequenceNumberRange_ =
    Shard'
      { adjacentParentShardId = Prelude.Nothing,
        parentShardId = Prelude.Nothing,
        shardId = pShardId_,
        hashKeyRange = pHashKeyRange_,
        sequenceNumberRange = pSequenceNumberRange_
      }

-- | The shard ID of the shard adjacent to the shard\'s parent.
shard_adjacentParentShardId :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_adjacentParentShardId = Lens.lens (\Shard' {adjacentParentShardId} -> adjacentParentShardId) (\s@Shard' {} a -> s {adjacentParentShardId = a} :: Shard)

-- | The shard ID of the shard\'s parent.
shard_parentShardId :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_parentShardId = Lens.lens (\Shard' {parentShardId} -> parentShardId) (\s@Shard' {} a -> s {parentShardId = a} :: Shard)

-- | The unique identifier of the shard within the stream.
shard_shardId :: Lens.Lens' Shard Prelude.Text
shard_shardId = Lens.lens (\Shard' {shardId} -> shardId) (\s@Shard' {} a -> s {shardId = a} :: Shard)

-- | The range of possible hash key values for the shard, which is a set of
-- ordered contiguous positive integers.
shard_hashKeyRange :: Lens.Lens' Shard HashKeyRange
shard_hashKeyRange = Lens.lens (\Shard' {hashKeyRange} -> hashKeyRange) (\s@Shard' {} a -> s {hashKeyRange = a} :: Shard)

-- | The range of possible sequence numbers for the shard.
shard_sequenceNumberRange :: Lens.Lens' Shard SequenceNumberRange
shard_sequenceNumberRange = Lens.lens (\Shard' {sequenceNumberRange} -> sequenceNumberRange) (\s@Shard' {} a -> s {sequenceNumberRange = a} :: Shard)

instance Data.FromJSON Shard where
  parseJSON =
    Data.withObject
      "Shard"
      ( \x ->
          Shard'
            Prelude.<$> (x Data..:? "AdjacentParentShardId")
            Prelude.<*> (x Data..:? "ParentShardId")
            Prelude.<*> (x Data..: "ShardId")
            Prelude.<*> (x Data..: "HashKeyRange")
            Prelude.<*> (x Data..: "SequenceNumberRange")
      )

instance Prelude.Hashable Shard where
  hashWithSalt _salt Shard' {..} =
    _salt
      `Prelude.hashWithSalt` adjacentParentShardId
      `Prelude.hashWithSalt` parentShardId
      `Prelude.hashWithSalt` shardId
      `Prelude.hashWithSalt` hashKeyRange
      `Prelude.hashWithSalt` sequenceNumberRange

instance Prelude.NFData Shard where
  rnf Shard' {..} =
    Prelude.rnf adjacentParentShardId
      `Prelude.seq` Prelude.rnf parentShardId
      `Prelude.seq` Prelude.rnf shardId
      `Prelude.seq` Prelude.rnf hashKeyRange
      `Prelude.seq` Prelude.rnf sequenceNumberRange
