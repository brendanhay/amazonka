{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Shard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Shard
  ( Shard (..),

    -- * Smart constructor
    mkShard,

    -- * Lenses
    sParentShardId,
    sSequenceNumberRange,
    sShardId,
  )
where

import qualified Network.AWS.DynamoDBStreams.Types.ParentShardId as Types
import qualified Network.AWS.DynamoDBStreams.Types.SequenceNumberRange as Types
import qualified Network.AWS.DynamoDBStreams.Types.ShardId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A uniquely identified group of stream records within a stream.
--
-- /See:/ 'mkShard' smart constructor.
data Shard = Shard'
  { -- | The shard ID of the current shard's parent.
    parentShardId :: Core.Maybe Types.ParentShardId,
    -- | The range of possible sequence numbers for the shard.
    sequenceNumberRange :: Core.Maybe Types.SequenceNumberRange,
    -- | The system-generated identifier for this shard.
    shardId :: Core.Maybe Types.ShardId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Shard' value with any optional fields omitted.
mkShard ::
  Shard
mkShard =
  Shard'
    { parentShardId = Core.Nothing,
      sequenceNumberRange = Core.Nothing,
      shardId = Core.Nothing
    }

-- | The shard ID of the current shard's parent.
--
-- /Note:/ Consider using 'parentShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sParentShardId :: Lens.Lens' Shard (Core.Maybe Types.ParentShardId)
sParentShardId = Lens.field @"parentShardId"
{-# DEPRECATED sParentShardId "Use generic-lens or generic-optics with 'parentShardId' instead." #-}

-- | The range of possible sequence numbers for the shard.
--
-- /Note:/ Consider using 'sequenceNumberRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSequenceNumberRange :: Lens.Lens' Shard (Core.Maybe Types.SequenceNumberRange)
sSequenceNumberRange = Lens.field @"sequenceNumberRange"
{-# DEPRECATED sSequenceNumberRange "Use generic-lens or generic-optics with 'sequenceNumberRange' instead." #-}

-- | The system-generated identifier for this shard.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShardId :: Lens.Lens' Shard (Core.Maybe Types.ShardId)
sShardId = Lens.field @"shardId"
{-# DEPRECATED sShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

instance Core.FromJSON Shard where
  parseJSON =
    Core.withObject "Shard" Core.$
      \x ->
        Shard'
          Core.<$> (x Core..:? "ParentShardId")
          Core.<*> (x Core..:? "SequenceNumberRange")
          Core.<*> (x Core..:? "ShardId")
