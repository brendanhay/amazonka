{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.NodeGroupMember
  ( NodeGroupMember (..)
  -- * Smart constructor
  , mkNodeGroupMember
  -- * Lenses
  , ngmCacheClusterId
  , ngmCacheNodeId
  , ngmCurrentRole
  , ngmPreferredAvailabilityZone
  , ngmPreferredOutpostArn
  , ngmReadEndpoint
  ) where

import qualified Network.AWS.ElastiCache.Types.Endpoint as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a single node within a node group (shard).
--
-- /See:/ 'mkNodeGroupMember' smart constructor.
data NodeGroupMember = NodeGroupMember'
  { cacheClusterId :: Core.Maybe Core.Text
    -- ^ The ID of the cluster to which the node belongs.
  , cacheNodeId :: Core.Maybe Core.Text
    -- ^ The ID of the node within its cluster. A node ID is a numeric identifier (0001, 0002, etc.).
  , currentRole :: Core.Maybe Core.Text
    -- ^ The role that is currently assigned to the node - @primary@ or @replica@ . This member is only applicable for Redis (cluster mode disabled) replication groups.
  , preferredAvailabilityZone :: Core.Maybe Core.Text
    -- ^ The name of the Availability Zone in which the node is located.
  , preferredOutpostArn :: Core.Maybe Core.Text
    -- ^ The outpost ARN of the node group member.
  , readEndpoint :: Core.Maybe Types.Endpoint
    -- ^ The information required for client programs to connect to a node for read operations. The read endpoint is only applicable on Redis (cluster mode disabled) clusters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeGroupMember' value with any optional fields omitted.
mkNodeGroupMember
    :: NodeGroupMember
mkNodeGroupMember
  = NodeGroupMember'{cacheClusterId = Core.Nothing,
                     cacheNodeId = Core.Nothing, currentRole = Core.Nothing,
                     preferredAvailabilityZone = Core.Nothing,
                     preferredOutpostArn = Core.Nothing, readEndpoint = Core.Nothing}

-- | The ID of the cluster to which the node belongs.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmCacheClusterId :: Lens.Lens' NodeGroupMember (Core.Maybe Core.Text)
ngmCacheClusterId = Lens.field @"cacheClusterId"
{-# INLINEABLE ngmCacheClusterId #-}
{-# DEPRECATED cacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead"  #-}

-- | The ID of the node within its cluster. A node ID is a numeric identifier (0001, 0002, etc.).
--
-- /Note:/ Consider using 'cacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmCacheNodeId :: Lens.Lens' NodeGroupMember (Core.Maybe Core.Text)
ngmCacheNodeId = Lens.field @"cacheNodeId"
{-# INLINEABLE ngmCacheNodeId #-}
{-# DEPRECATED cacheNodeId "Use generic-lens or generic-optics with 'cacheNodeId' instead"  #-}

-- | The role that is currently assigned to the node - @primary@ or @replica@ . This member is only applicable for Redis (cluster mode disabled) replication groups.
--
-- /Note:/ Consider using 'currentRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmCurrentRole :: Lens.Lens' NodeGroupMember (Core.Maybe Core.Text)
ngmCurrentRole = Lens.field @"currentRole"
{-# INLINEABLE ngmCurrentRole #-}
{-# DEPRECATED currentRole "Use generic-lens or generic-optics with 'currentRole' instead"  #-}

-- | The name of the Availability Zone in which the node is located.
--
-- /Note:/ Consider using 'preferredAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmPreferredAvailabilityZone :: Lens.Lens' NodeGroupMember (Core.Maybe Core.Text)
ngmPreferredAvailabilityZone = Lens.field @"preferredAvailabilityZone"
{-# INLINEABLE ngmPreferredAvailabilityZone #-}
{-# DEPRECATED preferredAvailabilityZone "Use generic-lens or generic-optics with 'preferredAvailabilityZone' instead"  #-}

-- | The outpost ARN of the node group member.
--
-- /Note:/ Consider using 'preferredOutpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmPreferredOutpostArn :: Lens.Lens' NodeGroupMember (Core.Maybe Core.Text)
ngmPreferredOutpostArn = Lens.field @"preferredOutpostArn"
{-# INLINEABLE ngmPreferredOutpostArn #-}
{-# DEPRECATED preferredOutpostArn "Use generic-lens or generic-optics with 'preferredOutpostArn' instead"  #-}

-- | The information required for client programs to connect to a node for read operations. The read endpoint is only applicable on Redis (cluster mode disabled) clusters.
--
-- /Note:/ Consider using 'readEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmReadEndpoint :: Lens.Lens' NodeGroupMember (Core.Maybe Types.Endpoint)
ngmReadEndpoint = Lens.field @"readEndpoint"
{-# INLINEABLE ngmReadEndpoint #-}
{-# DEPRECATED readEndpoint "Use generic-lens or generic-optics with 'readEndpoint' instead"  #-}

instance Core.FromXML NodeGroupMember where
        parseXML x
          = NodeGroupMember' Core.<$>
              (x Core..@? "CacheClusterId") Core.<*> x Core..@? "CacheNodeId"
                Core.<*> x Core..@? "CurrentRole"
                Core.<*> x Core..@? "PreferredAvailabilityZone"
                Core.<*> x Core..@? "PreferredOutpostArn"
                Core.<*> x Core..@? "ReadEndpoint"
