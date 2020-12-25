{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ChildShard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ChildShard
  ( ChildShard (..),

    -- * Smart constructor
    mkChildShard,

    -- * Lenses
    csShardId,
    csParentShards,
    csHashKeyRange,
  )
where

import qualified Network.AWS.Kinesis.Types.HashKeyRange as Types
import qualified Network.AWS.Kinesis.Types.ShardId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkChildShard' smart constructor.
data ChildShard = ChildShard'
  { shardId :: Types.ShardId,
    parentShards :: [Types.ShardId],
    hashKeyRange :: Types.HashKeyRange
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChildShard' value with any optional fields omitted.
mkChildShard ::
  -- | 'shardId'
  Types.ShardId ->
  -- | 'hashKeyRange'
  Types.HashKeyRange ->
  ChildShard
mkChildShard shardId hashKeyRange =
  ChildShard' {shardId, parentShards = Core.mempty, hashKeyRange}

-- | Undocumented field.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csShardId :: Lens.Lens' ChildShard Types.ShardId
csShardId = Lens.field @"shardId"
{-# DEPRECATED csShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'parentShards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csParentShards :: Lens.Lens' ChildShard [Types.ShardId]
csParentShards = Lens.field @"parentShards"
{-# DEPRECATED csParentShards "Use generic-lens or generic-optics with 'parentShards' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hashKeyRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csHashKeyRange :: Lens.Lens' ChildShard Types.HashKeyRange
csHashKeyRange = Lens.field @"hashKeyRange"
{-# DEPRECATED csHashKeyRange "Use generic-lens or generic-optics with 'hashKeyRange' instead." #-}

instance Core.FromJSON ChildShard where
  parseJSON =
    Core.withObject "ChildShard" Core.$
      \x ->
        ChildShard'
          Core.<$> (x Core..: "ShardId")
          Core.<*> (x Core..:? "ParentShards" Core..!= Core.mempty)
          Core.<*> (x Core..: "HashKeyRange")
