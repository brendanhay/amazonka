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
    ngmPreferredAvailabilityZone,
    ngmCurrentRole,
    ngmPreferredOutpostARN,
    ngmReadEndpoint,
  )
where

import Network.AWS.ElastiCache.Types.Endpoint
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a single node within a node group (shard).
--
-- /See:/ 'mkNodeGroupMember' smart constructor.
data NodeGroupMember = NodeGroupMember'
  { cacheClusterId ::
      Lude.Maybe Lude.Text,
    cacheNodeId :: Lude.Maybe Lude.Text,
    preferredAvailabilityZone :: Lude.Maybe Lude.Text,
    currentRole :: Lude.Maybe Lude.Text,
    preferredOutpostARN :: Lude.Maybe Lude.Text,
    readEndpoint :: Lude.Maybe Endpoint
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeGroupMember' with the minimum fields required to make a request.
--
-- * 'cacheClusterId' - The ID of the cluster to which the node belongs.
-- * 'cacheNodeId' - The ID of the node within its cluster. A node ID is a numeric identifier (0001, 0002, etc.).
-- * 'currentRole' - The role that is currently assigned to the node - @primary@ or @replica@ . This member is only applicable for Redis (cluster mode disabled) replication groups.
-- * 'preferredAvailabilityZone' - The name of the Availability Zone in which the node is located.
-- * 'preferredOutpostARN' - The outpost ARN of the node group member.
-- * 'readEndpoint' - The information required for client programs to connect to a node for read operations. The read endpoint is only applicable on Redis (cluster mode disabled) clusters.
mkNodeGroupMember ::
  NodeGroupMember
mkNodeGroupMember =
  NodeGroupMember'
    { cacheClusterId = Lude.Nothing,
      cacheNodeId = Lude.Nothing,
      preferredAvailabilityZone = Lude.Nothing,
      currentRole = Lude.Nothing,
      preferredOutpostARN = Lude.Nothing,
      readEndpoint = Lude.Nothing
    }

-- | The ID of the cluster to which the node belongs.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmCacheClusterId :: Lens.Lens' NodeGroupMember (Lude.Maybe Lude.Text)
ngmCacheClusterId = Lens.lens (cacheClusterId :: NodeGroupMember -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: NodeGroupMember)
{-# DEPRECATED ngmCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The ID of the node within its cluster. A node ID is a numeric identifier (0001, 0002, etc.).
--
-- /Note:/ Consider using 'cacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmCacheNodeId :: Lens.Lens' NodeGroupMember (Lude.Maybe Lude.Text)
ngmCacheNodeId = Lens.lens (cacheNodeId :: NodeGroupMember -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeId = a} :: NodeGroupMember)
{-# DEPRECATED ngmCacheNodeId "Use generic-lens or generic-optics with 'cacheNodeId' instead." #-}

-- | The name of the Availability Zone in which the node is located.
--
-- /Note:/ Consider using 'preferredAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmPreferredAvailabilityZone :: Lens.Lens' NodeGroupMember (Lude.Maybe Lude.Text)
ngmPreferredAvailabilityZone = Lens.lens (preferredAvailabilityZone :: NodeGroupMember -> Lude.Maybe Lude.Text) (\s a -> s {preferredAvailabilityZone = a} :: NodeGroupMember)
{-# DEPRECATED ngmPreferredAvailabilityZone "Use generic-lens or generic-optics with 'preferredAvailabilityZone' instead." #-}

-- | The role that is currently assigned to the node - @primary@ or @replica@ . This member is only applicable for Redis (cluster mode disabled) replication groups.
--
-- /Note:/ Consider using 'currentRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmCurrentRole :: Lens.Lens' NodeGroupMember (Lude.Maybe Lude.Text)
ngmCurrentRole = Lens.lens (currentRole :: NodeGroupMember -> Lude.Maybe Lude.Text) (\s a -> s {currentRole = a} :: NodeGroupMember)
{-# DEPRECATED ngmCurrentRole "Use generic-lens or generic-optics with 'currentRole' instead." #-}

-- | The outpost ARN of the node group member.
--
-- /Note:/ Consider using 'preferredOutpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmPreferredOutpostARN :: Lens.Lens' NodeGroupMember (Lude.Maybe Lude.Text)
ngmPreferredOutpostARN = Lens.lens (preferredOutpostARN :: NodeGroupMember -> Lude.Maybe Lude.Text) (\s a -> s {preferredOutpostARN = a} :: NodeGroupMember)
{-# DEPRECATED ngmPreferredOutpostARN "Use generic-lens or generic-optics with 'preferredOutpostARN' instead." #-}

-- | The information required for client programs to connect to a node for read operations. The read endpoint is only applicable on Redis (cluster mode disabled) clusters.
--
-- /Note:/ Consider using 'readEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmReadEndpoint :: Lens.Lens' NodeGroupMember (Lude.Maybe Endpoint)
ngmReadEndpoint = Lens.lens (readEndpoint :: NodeGroupMember -> Lude.Maybe Endpoint) (\s a -> s {readEndpoint = a} :: NodeGroupMember)
{-# DEPRECATED ngmReadEndpoint "Use generic-lens or generic-optics with 'readEndpoint' instead." #-}

instance Lude.FromXML NodeGroupMember where
  parseXML x =
    NodeGroupMember'
      Lude.<$> (x Lude..@? "CacheClusterId")
      Lude.<*> (x Lude..@? "CacheNodeId")
      Lude.<*> (x Lude..@? "PreferredAvailabilityZone")
      Lude.<*> (x Lude..@? "CurrentRole")
      Lude.<*> (x Lude..@? "PreferredOutpostArn")
      Lude.<*> (x Lude..@? "ReadEndpoint")
