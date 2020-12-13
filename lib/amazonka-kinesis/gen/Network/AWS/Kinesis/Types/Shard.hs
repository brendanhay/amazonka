{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Shard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.Shard
  ( Shard (..),

    -- * Smart constructor
    mkShard,

    -- * Lenses
    sAdjacentParentShardId,
    sHashKeyRange,
    sParentShardId,
    sSequenceNumberRange,
    sShardId,
  )
where

import Network.AWS.Kinesis.Types.HashKeyRange
import Network.AWS.Kinesis.Types.SequenceNumberRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A uniquely identified group of data records in a Kinesis data stream.
--
-- /See:/ 'mkShard' smart constructor.
data Shard = Shard'
  { -- | The shard ID of the shard adjacent to the shard's parent.
    adjacentParentShardId :: Lude.Maybe Lude.Text,
    -- | The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
    hashKeyRange :: HashKeyRange,
    -- | The shard ID of the shard's parent.
    parentShardId :: Lude.Maybe Lude.Text,
    -- | The range of possible sequence numbers for the shard.
    sequenceNumberRange :: SequenceNumberRange,
    -- | The unique identifier of the shard within the stream.
    shardId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Shard' with the minimum fields required to make a request.
--
-- * 'adjacentParentShardId' - The shard ID of the shard adjacent to the shard's parent.
-- * 'hashKeyRange' - The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
-- * 'parentShardId' - The shard ID of the shard's parent.
-- * 'sequenceNumberRange' - The range of possible sequence numbers for the shard.
-- * 'shardId' - The unique identifier of the shard within the stream.
mkShard ::
  -- | 'hashKeyRange'
  HashKeyRange ->
  -- | 'sequenceNumberRange'
  SequenceNumberRange ->
  -- | 'shardId'
  Lude.Text ->
  Shard
mkShard pHashKeyRange_ pSequenceNumberRange_ pShardId_ =
  Shard'
    { adjacentParentShardId = Lude.Nothing,
      hashKeyRange = pHashKeyRange_,
      parentShardId = Lude.Nothing,
      sequenceNumberRange = pSequenceNumberRange_,
      shardId = pShardId_
    }

-- | The shard ID of the shard adjacent to the shard's parent.
--
-- /Note:/ Consider using 'adjacentParentShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAdjacentParentShardId :: Lens.Lens' Shard (Lude.Maybe Lude.Text)
sAdjacentParentShardId = Lens.lens (adjacentParentShardId :: Shard -> Lude.Maybe Lude.Text) (\s a -> s {adjacentParentShardId = a} :: Shard)
{-# DEPRECATED sAdjacentParentShardId "Use generic-lens or generic-optics with 'adjacentParentShardId' instead." #-}

-- | The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
--
-- /Note:/ Consider using 'hashKeyRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHashKeyRange :: Lens.Lens' Shard HashKeyRange
sHashKeyRange = Lens.lens (hashKeyRange :: Shard -> HashKeyRange) (\s a -> s {hashKeyRange = a} :: Shard)
{-# DEPRECATED sHashKeyRange "Use generic-lens or generic-optics with 'hashKeyRange' instead." #-}

-- | The shard ID of the shard's parent.
--
-- /Note:/ Consider using 'parentShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sParentShardId :: Lens.Lens' Shard (Lude.Maybe Lude.Text)
sParentShardId = Lens.lens (parentShardId :: Shard -> Lude.Maybe Lude.Text) (\s a -> s {parentShardId = a} :: Shard)
{-# DEPRECATED sParentShardId "Use generic-lens or generic-optics with 'parentShardId' instead." #-}

-- | The range of possible sequence numbers for the shard.
--
-- /Note:/ Consider using 'sequenceNumberRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSequenceNumberRange :: Lens.Lens' Shard SequenceNumberRange
sSequenceNumberRange = Lens.lens (sequenceNumberRange :: Shard -> SequenceNumberRange) (\s a -> s {sequenceNumberRange = a} :: Shard)
{-# DEPRECATED sSequenceNumberRange "Use generic-lens or generic-optics with 'sequenceNumberRange' instead." #-}

-- | The unique identifier of the shard within the stream.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShardId :: Lens.Lens' Shard Lude.Text
sShardId = Lens.lens (shardId :: Shard -> Lude.Text) (\s a -> s {shardId = a} :: Shard)
{-# DEPRECATED sShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

instance Lude.FromJSON Shard where
  parseJSON =
    Lude.withObject
      "Shard"
      ( \x ->
          Shard'
            Lude.<$> (x Lude..:? "AdjacentParentShardId")
            Lude.<*> (x Lude..: "HashKeyRange")
            Lude.<*> (x Lude..:? "ParentShardId")
            Lude.<*> (x Lude..: "SequenceNumberRange")
            Lude.<*> (x Lude..: "ShardId")
      )
