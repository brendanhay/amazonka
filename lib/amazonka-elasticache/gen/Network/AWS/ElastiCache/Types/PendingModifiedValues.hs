{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.PendingModifiedValues
  ( PendingModifiedValues (..)
  -- * Smart constructor
  , mkPendingModifiedValues
  -- * Lenses
  , pmvAuthTokenStatus
  , pmvCacheNodeIdsToRemove
  , pmvCacheNodeType
  , pmvEngineVersion
  , pmvNumCacheNodes
  ) where

import qualified Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A group of settings that are applied to the cluster in the future, or that are currently being applied.
--
-- /See:/ 'mkPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { authTokenStatus :: Core.Maybe Types.AuthTokenUpdateStatus
    -- ^ The auth token status
  , cacheNodeIdsToRemove :: Core.Maybe [Core.Text]
    -- ^ A list of cache node IDs that are being removed (or will be removed) from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002, etc.).
  , cacheNodeType :: Core.Maybe Core.Text
    -- ^ The cache node type that this cluster or replication group is scaled to.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The new cache engine version that the cluster runs.
  , numCacheNodes :: Core.Maybe Core.Int
    -- ^ The new number of cache nodes for the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PendingModifiedValues' value with any optional fields omitted.
mkPendingModifiedValues
    :: PendingModifiedValues
mkPendingModifiedValues
  = PendingModifiedValues'{authTokenStatus = Core.Nothing,
                           cacheNodeIdsToRemove = Core.Nothing, cacheNodeType = Core.Nothing,
                           engineVersion = Core.Nothing, numCacheNodes = Core.Nothing}

-- | The auth token status
--
-- /Note:/ Consider using 'authTokenStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvAuthTokenStatus :: Lens.Lens' PendingModifiedValues (Core.Maybe Types.AuthTokenUpdateStatus)
pmvAuthTokenStatus = Lens.field @"authTokenStatus"
{-# INLINEABLE pmvAuthTokenStatus #-}
{-# DEPRECATED authTokenStatus "Use generic-lens or generic-optics with 'authTokenStatus' instead"  #-}

-- | A list of cache node IDs that are being removed (or will be removed) from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002, etc.).
--
-- /Note:/ Consider using 'cacheNodeIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvCacheNodeIdsToRemove :: Lens.Lens' PendingModifiedValues (Core.Maybe [Core.Text])
pmvCacheNodeIdsToRemove = Lens.field @"cacheNodeIdsToRemove"
{-# INLINEABLE pmvCacheNodeIdsToRemove #-}
{-# DEPRECATED cacheNodeIdsToRemove "Use generic-lens or generic-optics with 'cacheNodeIdsToRemove' instead"  #-}

-- | The cache node type that this cluster or replication group is scaled to.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvCacheNodeType :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pmvCacheNodeType = Lens.field @"cacheNodeType"
{-# INLINEABLE pmvCacheNodeType #-}
{-# DEPRECATED cacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead"  #-}

-- | The new cache engine version that the cluster runs.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvEngineVersion :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pmvEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE pmvEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The new number of cache nodes for the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvNumCacheNodes :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pmvNumCacheNodes = Lens.field @"numCacheNodes"
{-# INLINEABLE pmvNumCacheNodes #-}
{-# DEPRECATED numCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead"  #-}

instance Core.FromXML PendingModifiedValues where
        parseXML x
          = PendingModifiedValues' Core.<$>
              (x Core..@? "AuthTokenStatus") Core.<*>
                x Core..@? "CacheNodeIdsToRemove" Core..<@>
                  Core.parseXMLList "CacheNodeId"
                Core.<*> x Core..@? "CacheNodeType"
                Core.<*> x Core..@? "EngineVersion"
                Core.<*> x Core..@? "NumCacheNodes"
