{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Shard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.Shard
  ( Shard (..)
  -- * Smart constructor
  , mkShard
  -- * Lenses
  , sShardId
  , sHashKeyRange
  , sSequenceNumberRange
  , sAdjacentParentShardId
  , sParentShardId
  ) where

import qualified Network.AWS.Kinesis.Types.AdjacentParentShardId as Types
import qualified Network.AWS.Kinesis.Types.HashKeyRange as Types
import qualified Network.AWS.Kinesis.Types.ParentShardId as Types
import qualified Network.AWS.Kinesis.Types.SequenceNumberRange as Types
import qualified Network.AWS.Kinesis.Types.ShardId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A uniquely identified group of data records in a Kinesis data stream.
--
-- /See:/ 'mkShard' smart constructor.
data Shard = Shard'
  { shardId :: Types.ShardId
    -- ^ The unique identifier of the shard within the stream.
  , hashKeyRange :: Types.HashKeyRange
    -- ^ The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
  , sequenceNumberRange :: Types.SequenceNumberRange
    -- ^ The range of possible sequence numbers for the shard.
  , adjacentParentShardId :: Core.Maybe Types.AdjacentParentShardId
    -- ^ The shard ID of the shard adjacent to the shard's parent.
  , parentShardId :: Core.Maybe Types.ParentShardId
    -- ^ The shard ID of the shard's parent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Shard' value with any optional fields omitted.
mkShard
    :: Types.ShardId -- ^ 'shardId'
    -> Types.HashKeyRange -- ^ 'hashKeyRange'
    -> Types.SequenceNumberRange -- ^ 'sequenceNumberRange'
    -> Shard
mkShard shardId hashKeyRange sequenceNumberRange
  = Shard'{shardId, hashKeyRange, sequenceNumberRange,
           adjacentParentShardId = Core.Nothing, parentShardId = Core.Nothing}

-- | The unique identifier of the shard within the stream.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShardId :: Lens.Lens' Shard Types.ShardId
sShardId = Lens.field @"shardId"
{-# INLINEABLE sShardId #-}
{-# DEPRECATED shardId "Use generic-lens or generic-optics with 'shardId' instead"  #-}

-- | The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
--
-- /Note:/ Consider using 'hashKeyRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHashKeyRange :: Lens.Lens' Shard Types.HashKeyRange
sHashKeyRange = Lens.field @"hashKeyRange"
{-# INLINEABLE sHashKeyRange #-}
{-# DEPRECATED hashKeyRange "Use generic-lens or generic-optics with 'hashKeyRange' instead"  #-}

-- | The range of possible sequence numbers for the shard.
--
-- /Note:/ Consider using 'sequenceNumberRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSequenceNumberRange :: Lens.Lens' Shard Types.SequenceNumberRange
sSequenceNumberRange = Lens.field @"sequenceNumberRange"
{-# INLINEABLE sSequenceNumberRange #-}
{-# DEPRECATED sequenceNumberRange "Use generic-lens or generic-optics with 'sequenceNumberRange' instead"  #-}

-- | The shard ID of the shard adjacent to the shard's parent.
--
-- /Note:/ Consider using 'adjacentParentShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAdjacentParentShardId :: Lens.Lens' Shard (Core.Maybe Types.AdjacentParentShardId)
sAdjacentParentShardId = Lens.field @"adjacentParentShardId"
{-# INLINEABLE sAdjacentParentShardId #-}
{-# DEPRECATED adjacentParentShardId "Use generic-lens or generic-optics with 'adjacentParentShardId' instead"  #-}

-- | The shard ID of the shard's parent.
--
-- /Note:/ Consider using 'parentShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sParentShardId :: Lens.Lens' Shard (Core.Maybe Types.ParentShardId)
sParentShardId = Lens.field @"parentShardId"
{-# INLINEABLE sParentShardId #-}
{-# DEPRECATED parentShardId "Use generic-lens or generic-optics with 'parentShardId' instead"  #-}

instance Core.FromJSON Shard where
        parseJSON
          = Core.withObject "Shard" Core.$
              \ x ->
                Shard' Core.<$>
                  (x Core..: "ShardId") Core.<*> x Core..: "HashKeyRange" Core.<*>
                    x Core..: "SequenceNumberRange"
                    Core.<*> x Core..:? "AdjacentParentShardId"
                    Core.<*> x Core..:? "ParentShardId"
