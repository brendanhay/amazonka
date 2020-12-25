{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.PendingModifiedValues
  ( PendingModifiedValues (..),

    -- * Smart constructor
    mkPendingModifiedValues,

    -- * Lenses
    pmvAuthTokenStatus,
    pmvCacheNodeIdsToRemove,
    pmvCacheNodeType,
    pmvEngineVersion,
    pmvNumCacheNodes,
  )
where

import qualified Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A group of settings that are applied to the cluster in the future, or that are currently being applied.
--
-- /See:/ 'mkPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { -- | The auth token status
    authTokenStatus :: Core.Maybe Types.AuthTokenUpdateStatus,
    -- | A list of cache node IDs that are being removed (or will be removed) from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002, etc.).
    cacheNodeIdsToRemove :: Core.Maybe [Types.String],
    -- | The cache node type that this cluster or replication group is scaled to.
    cacheNodeType :: Core.Maybe Types.String,
    -- | The new cache engine version that the cluster runs.
    engineVersion :: Core.Maybe Types.String,
    -- | The new number of cache nodes for the cluster.
    --
    -- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
    numCacheNodes :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PendingModifiedValues' value with any optional fields omitted.
mkPendingModifiedValues ::
  PendingModifiedValues
mkPendingModifiedValues =
  PendingModifiedValues'
    { authTokenStatus = Core.Nothing,
      cacheNodeIdsToRemove = Core.Nothing,
      cacheNodeType = Core.Nothing,
      engineVersion = Core.Nothing,
      numCacheNodes = Core.Nothing
    }

-- | The auth token status
--
-- /Note:/ Consider using 'authTokenStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvAuthTokenStatus :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.AuthTokenUpdateStatus)
pmvAuthTokenStatus = Lens.field @"authTokenStatus"
{-# DEPRECATED pmvAuthTokenStatus "Use generic-lens or generic-optics with 'authTokenStatus' instead." #-}

-- | A list of cache node IDs that are being removed (or will be removed) from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002, etc.).
--
-- /Note:/ Consider using 'cacheNodeIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvCacheNodeIdsToRemove :: Lens.Lens' PendingModifiedValues (Core.Maybe [Types.String])
pmvCacheNodeIdsToRemove = Lens.field @"cacheNodeIdsToRemove"
{-# DEPRECATED pmvCacheNodeIdsToRemove "Use generic-lens or generic-optics with 'cacheNodeIdsToRemove' instead." #-}

-- | The cache node type that this cluster or replication group is scaled to.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvCacheNodeType :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.String)
pmvCacheNodeType = Lens.field @"cacheNodeType"
{-# DEPRECATED pmvCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The new cache engine version that the cluster runs.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvEngineVersion :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.String)
pmvEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED pmvEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The new number of cache nodes for the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvNumCacheNodes :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pmvNumCacheNodes = Lens.field @"numCacheNodes"
{-# DEPRECATED pmvNumCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead." #-}

instance Core.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Core.<$> (x Core..@? "AuthTokenStatus")
      Core.<*> ( x Core..@? "CacheNodeIdsToRemove"
                   Core..<@> Core.parseXMLList "CacheNodeId"
               )
      Core.<*> (x Core..@? "CacheNodeType")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "NumCacheNodes")
