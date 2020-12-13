{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.RebootCacheCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots some, or all, of the cache nodes within a provisioned cluster. This operation applies any modified cache parameter groups to the cluster. The reboot operation takes place as soon as possible, and results in a momentary outage to the cluster. During the reboot, the cluster status is set to REBOOTING.
--
-- The reboot causes the contents of the cache (for each cache node being rebooted) to be lost.
-- When the reboot is complete, a cluster event is created.
-- Rebooting a cluster is currently supported on Memcached and Redis (cluster mode disabled) clusters. Rebooting is not supported on Redis (cluster mode enabled) clusters.
-- If you make changes to parameters that require a Redis (cluster mode enabled) cluster reboot for the changes to be applied, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> for an alternate process.
module Network.AWS.ElastiCache.RebootCacheCluster
  ( -- * Creating a request
    RebootCacheCluster (..),
    mkRebootCacheCluster,

    -- ** Request lenses
    rccCacheClusterId,
    rccCacheNodeIdsToReboot,

    -- * Destructuring the response
    RebootCacheClusterResponse (..),
    mkRebootCacheClusterResponse,

    -- ** Response lenses
    rccrsCacheCluster,
    rccrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @RebootCacheCluster@ operation.
--
-- /See:/ 'mkRebootCacheCluster' smart constructor.
data RebootCacheCluster = RebootCacheCluster'
  { -- | The cluster identifier. This parameter is stored as a lowercase string.
    cacheClusterId :: Lude.Text,
    -- | A list of cache node IDs to reboot. A node ID is a numeric identifier (0001, 0002, etc.). To reboot an entire cluster, specify all of the cache node IDs.
    cacheNodeIdsToReboot :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootCacheCluster' with the minimum fields required to make a request.
--
-- * 'cacheClusterId' - The cluster identifier. This parameter is stored as a lowercase string.
-- * 'cacheNodeIdsToReboot' - A list of cache node IDs to reboot. A node ID is a numeric identifier (0001, 0002, etc.). To reboot an entire cluster, specify all of the cache node IDs.
mkRebootCacheCluster ::
  -- | 'cacheClusterId'
  Lude.Text ->
  RebootCacheCluster
mkRebootCacheCluster pCacheClusterId_ =
  RebootCacheCluster'
    { cacheClusterId = pCacheClusterId_,
      cacheNodeIdsToReboot = Lude.mempty
    }

-- | The cluster identifier. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccCacheClusterId :: Lens.Lens' RebootCacheCluster Lude.Text
rccCacheClusterId = Lens.lens (cacheClusterId :: RebootCacheCluster -> Lude.Text) (\s a -> s {cacheClusterId = a} :: RebootCacheCluster)
{-# DEPRECATED rccCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | A list of cache node IDs to reboot. A node ID is a numeric identifier (0001, 0002, etc.). To reboot an entire cluster, specify all of the cache node IDs.
--
-- /Note:/ Consider using 'cacheNodeIdsToReboot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccCacheNodeIdsToReboot :: Lens.Lens' RebootCacheCluster [Lude.Text]
rccCacheNodeIdsToReboot = Lens.lens (cacheNodeIdsToReboot :: RebootCacheCluster -> [Lude.Text]) (\s a -> s {cacheNodeIdsToReboot = a} :: RebootCacheCluster)
{-# DEPRECATED rccCacheNodeIdsToReboot "Use generic-lens or generic-optics with 'cacheNodeIdsToReboot' instead." #-}

instance Lude.AWSRequest RebootCacheCluster where
  type Rs RebootCacheCluster = RebootCacheClusterResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "RebootCacheClusterResult"
      ( \s h x ->
          RebootCacheClusterResponse'
            Lude.<$> (x Lude..@? "CacheCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebootCacheCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RebootCacheCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery RebootCacheCluster where
  toQuery RebootCacheCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RebootCacheCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheClusterId" Lude.=: cacheClusterId,
        "CacheNodeIdsToReboot"
          Lude.=: Lude.toQueryList "CacheNodeId" cacheNodeIdsToReboot
      ]

-- | /See:/ 'mkRebootCacheClusterResponse' smart constructor.
data RebootCacheClusterResponse = RebootCacheClusterResponse'
  { cacheCluster :: Lude.Maybe CacheCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootCacheClusterResponse' with the minimum fields required to make a request.
--
-- * 'cacheCluster' -
-- * 'responseStatus' - The response status code.
mkRebootCacheClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebootCacheClusterResponse
mkRebootCacheClusterResponse pResponseStatus_ =
  RebootCacheClusterResponse'
    { cacheCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccrsCacheCluster :: Lens.Lens' RebootCacheClusterResponse (Lude.Maybe CacheCluster)
rccrsCacheCluster = Lens.lens (cacheCluster :: RebootCacheClusterResponse -> Lude.Maybe CacheCluster) (\s a -> s {cacheCluster = a} :: RebootCacheClusterResponse)
{-# DEPRECATED rccrsCacheCluster "Use generic-lens or generic-optics with 'cacheCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccrsResponseStatus :: Lens.Lens' RebootCacheClusterResponse Lude.Int
rccrsResponseStatus = Lens.lens (responseStatus :: RebootCacheClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebootCacheClusterResponse)
{-# DEPRECATED rccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
