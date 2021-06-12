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
-- Module      : Network.AWS.Kinesis.Types.Shard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.Shard where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types.HashKeyRange
import Network.AWS.Kinesis.Types.SequenceNumberRange
import qualified Network.AWS.Lens as Lens

-- | A uniquely identified group of data records in a Kinesis data stream.
--
-- /See:/ 'newShard' smart constructor.
data Shard = Shard'
  { -- | The shard ID of the shard adjacent to the shard\'s parent.
    adjacentParentShardId :: Core.Maybe Core.Text,
    -- | The shard ID of the shard\'s parent.
    parentShardId :: Core.Maybe Core.Text,
    -- | The unique identifier of the shard within the stream.
    shardId :: Core.Text,
    -- | The range of possible hash key values for the shard, which is a set of
    -- ordered contiguous positive integers.
    hashKeyRange :: HashKeyRange,
    -- | The range of possible sequence numbers for the shard.
    sequenceNumberRange :: SequenceNumberRange
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
      { adjacentParentShardId = Core.Nothing,
        parentShardId = Core.Nothing,
        shardId = pShardId_,
        hashKeyRange = pHashKeyRange_,
        sequenceNumberRange = pSequenceNumberRange_
      }

-- | The shard ID of the shard adjacent to the shard\'s parent.
shard_adjacentParentShardId :: Lens.Lens' Shard (Core.Maybe Core.Text)
shard_adjacentParentShardId = Lens.lens (\Shard' {adjacentParentShardId} -> adjacentParentShardId) (\s@Shard' {} a -> s {adjacentParentShardId = a} :: Shard)

-- | The shard ID of the shard\'s parent.
shard_parentShardId :: Lens.Lens' Shard (Core.Maybe Core.Text)
shard_parentShardId = Lens.lens (\Shard' {parentShardId} -> parentShardId) (\s@Shard' {} a -> s {parentShardId = a} :: Shard)

-- | The unique identifier of the shard within the stream.
shard_shardId :: Lens.Lens' Shard Core.Text
shard_shardId = Lens.lens (\Shard' {shardId} -> shardId) (\s@Shard' {} a -> s {shardId = a} :: Shard)

-- | The range of possible hash key values for the shard, which is a set of
-- ordered contiguous positive integers.
shard_hashKeyRange :: Lens.Lens' Shard HashKeyRange
shard_hashKeyRange = Lens.lens (\Shard' {hashKeyRange} -> hashKeyRange) (\s@Shard' {} a -> s {hashKeyRange = a} :: Shard)

-- | The range of possible sequence numbers for the shard.
shard_sequenceNumberRange :: Lens.Lens' Shard SequenceNumberRange
shard_sequenceNumberRange = Lens.lens (\Shard' {sequenceNumberRange} -> sequenceNumberRange) (\s@Shard' {} a -> s {sequenceNumberRange = a} :: Shard)

instance Core.FromJSON Shard where
  parseJSON =
    Core.withObject
      "Shard"
      ( \x ->
          Shard'
            Core.<$> (x Core..:? "AdjacentParentShardId")
            Core.<*> (x Core..:? "ParentShardId")
            Core.<*> (x Core..: "ShardId")
            Core.<*> (x Core..: "HashKeyRange")
            Core.<*> (x Core..: "SequenceNumberRange")
      )

instance Core.Hashable Shard

instance Core.NFData Shard
