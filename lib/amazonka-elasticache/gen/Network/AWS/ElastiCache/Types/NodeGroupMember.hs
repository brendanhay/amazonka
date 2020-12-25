{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupMember
  ( NodeGroupMember (..),

    -- * Smart constructor
    mkNodeGroupMember,

    -- * Lenses
    ngmCacheClusterId,
    ngmCacheNodeId,
    ngmCurrentRole,
    ngmPreferredAvailabilityZone,
    ngmPreferredOutpostArn,
    ngmReadEndpoint,
  )
where

import qualified Network.AWS.ElastiCache.Types.Endpoint as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a single node within a node group (shard).
--
-- /See:/ 'mkNodeGroupMember' smart constructor.
data NodeGroupMember = NodeGroupMember'
  { -- | The ID of the cluster to which the node belongs.
    cacheClusterId :: Core.Maybe Types.String,
    -- | The ID of the node within its cluster. A node ID is a numeric identifier (0001, 0002, etc.).
    cacheNodeId :: Core.Maybe Types.String,
    -- | The role that is currently assigned to the node - @primary@ or @replica@ . This member is only applicable for Redis (cluster mode disabled) replication groups.
    currentRole :: Core.Maybe Types.String,
    -- | The name of the Availability Zone in which the node is located.
    preferredAvailabilityZone :: Core.Maybe Types.String,
    -- | The outpost ARN of the node group member.
    preferredOutpostArn :: Core.Maybe Types.String,
    -- | The information required for client programs to connect to a node for read operations. The read endpoint is only applicable on Redis (cluster mode disabled) clusters.
    readEndpoint :: Core.Maybe Types.Endpoint
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeGroupMember' value with any optional fields omitted.
mkNodeGroupMember ::
  NodeGroupMember
mkNodeGroupMember =
  NodeGroupMember'
    { cacheClusterId = Core.Nothing,
      cacheNodeId = Core.Nothing,
      currentRole = Core.Nothing,
      preferredAvailabilityZone = Core.Nothing,
      preferredOutpostArn = Core.Nothing,
      readEndpoint = Core.Nothing
    }

-- | The ID of the cluster to which the node belongs.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmCacheClusterId :: Lens.Lens' NodeGroupMember (Core.Maybe Types.String)
ngmCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED ngmCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The ID of the node within its cluster. A node ID is a numeric identifier (0001, 0002, etc.).
--
-- /Note:/ Consider using 'cacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmCacheNodeId :: Lens.Lens' NodeGroupMember (Core.Maybe Types.String)
ngmCacheNodeId = Lens.field @"cacheNodeId"
{-# DEPRECATED ngmCacheNodeId "Use generic-lens or generic-optics with 'cacheNodeId' instead." #-}

-- | The role that is currently assigned to the node - @primary@ or @replica@ . This member is only applicable for Redis (cluster mode disabled) replication groups.
--
-- /Note:/ Consider using 'currentRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmCurrentRole :: Lens.Lens' NodeGroupMember (Core.Maybe Types.String)
ngmCurrentRole = Lens.field @"currentRole"
{-# DEPRECATED ngmCurrentRole "Use generic-lens or generic-optics with 'currentRole' instead." #-}

-- | The name of the Availability Zone in which the node is located.
--
-- /Note:/ Consider using 'preferredAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmPreferredAvailabilityZone :: Lens.Lens' NodeGroupMember (Core.Maybe Types.String)
ngmPreferredAvailabilityZone = Lens.field @"preferredAvailabilityZone"
{-# DEPRECATED ngmPreferredAvailabilityZone "Use generic-lens or generic-optics with 'preferredAvailabilityZone' instead." #-}

-- | The outpost ARN of the node group member.
--
-- /Note:/ Consider using 'preferredOutpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmPreferredOutpostArn :: Lens.Lens' NodeGroupMember (Core.Maybe Types.String)
ngmPreferredOutpostArn = Lens.field @"preferredOutpostArn"
{-# DEPRECATED ngmPreferredOutpostArn "Use generic-lens or generic-optics with 'preferredOutpostArn' instead." #-}

-- | The information required for client programs to connect to a node for read operations. The read endpoint is only applicable on Redis (cluster mode disabled) clusters.
--
-- /Note:/ Consider using 'readEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmReadEndpoint :: Lens.Lens' NodeGroupMember (Core.Maybe Types.Endpoint)
ngmReadEndpoint = Lens.field @"readEndpoint"
{-# DEPRECATED ngmReadEndpoint "Use generic-lens or generic-optics with 'readEndpoint' instead." #-}

instance Core.FromXML NodeGroupMember where
  parseXML x =
    NodeGroupMember'
      Core.<$> (x Core..@? "CacheClusterId")
      Core.<*> (x Core..@? "CacheNodeId")
      Core.<*> (x Core..@? "CurrentRole")
      Core.<*> (x Core..@? "PreferredAvailabilityZone")
      Core.<*> (x Core..@? "PreferredOutpostArn")
      Core.<*> (x Core..@? "ReadEndpoint")
