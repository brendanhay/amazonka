{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ShardFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.ShardFilter
  ( ShardFilter (..)
  -- * Smart constructor
  , mkShardFilter
  -- * Lenses
  , sfType
  , sfShardId
  , sfTimestamp
  ) where

import qualified Network.AWS.Kinesis.Types.ShardFilterType as Types
import qualified Network.AWS.Kinesis.Types.ShardId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkShardFilter' smart constructor.
data ShardFilter = ShardFilter'
  { type' :: Types.ShardFilterType
  , shardId :: Core.Maybe Types.ShardId
  , timestamp :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ShardFilter' value with any optional fields omitted.
mkShardFilter
    :: Types.ShardFilterType -- ^ 'type\''
    -> ShardFilter
mkShardFilter type'
  = ShardFilter'{type', shardId = Core.Nothing,
                 timestamp = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfType :: Lens.Lens' ShardFilter Types.ShardFilterType
sfType = Lens.field @"type'"
{-# INLINEABLE sfType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfShardId :: Lens.Lens' ShardFilter (Core.Maybe Types.ShardId)
sfShardId = Lens.field @"shardId"
{-# INLINEABLE sfShardId #-}
{-# DEPRECATED shardId "Use generic-lens or generic-optics with 'shardId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfTimestamp :: Lens.Lens' ShardFilter (Core.Maybe Core.NominalDiffTime)
sfTimestamp = Lens.field @"timestamp"
{-# INLINEABLE sfTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromJSON ShardFilter where
        toJSON ShardFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Type" Core..= type'),
                  ("ShardId" Core..=) Core.<$> shardId,
                  ("Timestamp" Core..=) Core.<$> timestamp])
