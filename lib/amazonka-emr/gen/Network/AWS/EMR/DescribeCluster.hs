{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides cluster-level details including status, hardware and software configuration, VPC settings, and so on.
module Network.AWS.EMR.DescribeCluster
  ( -- * Creating a request
    DescribeCluster (..),
    mkDescribeCluster,

    -- ** Request lenses
    dcClusterId,

    -- * Destructuring the response
    DescribeClusterResponse (..),
    mkDescribeClusterResponse,

    -- ** Response lenses
    dcrsResponseStatus,
    dcrsCluster,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | This input determines which cluster to describe.
--
-- /See:/ 'mkDescribeCluster' smart constructor.
newtype DescribeCluster = DescribeCluster' {clusterId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCluster' with the minimum fields required to make a request.
--
-- * 'clusterId' - The identifier of the cluster to describe.
mkDescribeCluster ::
  -- | 'clusterId'
  Lude.Text ->
  DescribeCluster
mkDescribeCluster pClusterId_ =
  DescribeCluster' {clusterId = pClusterId_}

-- | The identifier of the cluster to describe.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterId :: Lens.Lens' DescribeCluster Lude.Text
dcClusterId = Lens.lens (clusterId :: DescribeCluster -> Lude.Text) (\s a -> s {clusterId = a} :: DescribeCluster)
{-# DEPRECATED dcClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest DescribeCluster where
  type Rs DescribeCluster = DescribeClusterResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeClusterResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "Cluster")
      )

instance Lude.ToHeaders DescribeCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.DescribeCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCluster where
  toJSON DescribeCluster' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ClusterId" Lude..= clusterId)])

instance Lude.ToPath DescribeCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCluster where
  toQuery = Lude.const Lude.mempty

-- | This output contains the description of the cluster.
--
-- /See:/ 'mkDescribeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { responseStatus ::
      Lude.Int,
    cluster :: Cluster
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - This output contains the details for the requested cluster.
-- * 'responseStatus' - The response status code.
mkDescribeClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'cluster'
  Cluster ->
  DescribeClusterResponse
mkDescribeClusterResponse pResponseStatus_ pCluster_ =
  DescribeClusterResponse'
    { responseStatus = pResponseStatus_,
      cluster = pCluster_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeClusterResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClusterResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | This output contains the details for the requested cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCluster :: Lens.Lens' DescribeClusterResponse Cluster
dcrsCluster = Lens.lens (cluster :: DescribeClusterResponse -> Cluster) (\s a -> s {cluster = a} :: DescribeClusterResponse)
{-# DEPRECATED dcrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}
