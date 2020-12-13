{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.DeleteCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS CloudHSM cluster. Before you can delete a cluster, you must delete all HSMs in the cluster. To see if the cluster contains any HSMs, use 'DescribeClusters' . To delete an HSM, use 'DeleteHsm' .
module Network.AWS.CloudHSMv2.DeleteCluster
  ( -- * Creating a request
    DeleteCluster (..),
    mkDeleteCluster,

    -- ** Request lenses
    dcClusterId,

    -- * Destructuring the response
    DeleteClusterResponse (..),
    mkDeleteClusterResponse,

    -- ** Response lenses
    drsCluster,
    drsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCluster' smart constructor.
newtype DeleteCluster = DeleteCluster'
  { -- | The identifier (ID) of the cluster that you are deleting. To find the cluster ID, use 'DescribeClusters' .
    clusterId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCluster' with the minimum fields required to make a request.
--
-- * 'clusterId' - The identifier (ID) of the cluster that you are deleting. To find the cluster ID, use 'DescribeClusters' .
mkDeleteCluster ::
  -- | 'clusterId'
  Lude.Text ->
  DeleteCluster
mkDeleteCluster pClusterId_ =
  DeleteCluster' {clusterId = pClusterId_}

-- | The identifier (ID) of the cluster that you are deleting. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterId :: Lens.Lens' DeleteCluster Lude.Text
dcClusterId = Lens.lens (clusterId :: DeleteCluster -> Lude.Text) (\s a -> s {clusterId = a} :: DeleteCluster)
{-# DEPRECATED dcClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest DeleteCluster where
  type Rs DeleteCluster = DeleteClusterResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteClusterResponse'
            Lude.<$> (x Lude..?> "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.DeleteCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCluster where
  toJSON DeleteCluster' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ClusterId" Lude..= clusterId)])

instance Lude.ToPath DeleteCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { -- | Information about the cluster that was deleted.
    cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - Information about the cluster that was deleted.
-- * 'responseStatus' - The response status code.
mkDeleteClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteClusterResponse
mkDeleteClusterResponse pResponseStatus_ =
  DeleteClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the cluster that was deleted.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCluster :: Lens.Lens' DeleteClusterResponse (Lude.Maybe Cluster)
drsCluster = Lens.lens (cluster :: DeleteClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: DeleteClusterResponse)
{-# DEPRECATED drsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteClusterResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteClusterResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
