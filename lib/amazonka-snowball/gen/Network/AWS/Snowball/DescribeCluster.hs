{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific cluster including shipping information, cluster status, and other important metadata.
module Network.AWS.Snowball.DescribeCluster
  ( -- * Creating a request
    DescribeCluster (..),
    mkDescribeCluster,

    -- ** Request lenses
    dcClusterId,

    -- * Destructuring the response
    DescribeClusterResponse (..),
    mkDescribeClusterResponse,

    -- ** Response lenses
    dcrsClusterMetadata,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkDescribeCluster' smart constructor.
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
-- * 'clusterId' - The automatically generated ID for a cluster.
mkDescribeCluster ::
  -- | 'clusterId'
  Lude.Text ->
  DescribeCluster
mkDescribeCluster pClusterId_ =
  DescribeCluster' {clusterId = pClusterId_}

-- | The automatically generated ID for a cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterId :: Lens.Lens' DescribeCluster Lude.Text
dcClusterId = Lens.lens (clusterId :: DescribeCluster -> Lude.Text) (\s a -> s {clusterId = a} :: DescribeCluster)
{-# DEPRECATED dcClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest DescribeCluster where
  type Rs DescribeCluster = DescribeClusterResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeClusterResponse'
            Lude.<$> (x Lude..?> "ClusterMetadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.DescribeCluster" ::
                          Lude.ByteString
                      ),
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

-- | /See:/ 'mkDescribeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { clusterMetadata ::
      Lude.Maybe ClusterMetadata,
    responseStatus :: Lude.Int
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
-- * 'clusterMetadata' - Information about a specific cluster, including shipping information, cluster status, and other important metadata.
-- * 'responseStatus' - The response status code.
mkDescribeClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClusterResponse
mkDescribeClusterResponse pResponseStatus_ =
  DescribeClusterResponse'
    { clusterMetadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a specific cluster, including shipping information, cluster status, and other important metadata.
--
-- /Note:/ Consider using 'clusterMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsClusterMetadata :: Lens.Lens' DescribeClusterResponse (Lude.Maybe ClusterMetadata)
dcrsClusterMetadata = Lens.lens (clusterMetadata :: DescribeClusterResponse -> Lude.Maybe ClusterMetadata) (\s a -> s {clusterMetadata = a} :: DescribeClusterResponse)
{-# DEPRECATED dcrsClusterMetadata "Use generic-lens or generic-optics with 'clusterMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeClusterResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClusterResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
