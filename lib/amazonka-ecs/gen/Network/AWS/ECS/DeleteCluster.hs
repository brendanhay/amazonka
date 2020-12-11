{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cluster. The cluster will transition to the @INACTIVE@ state. Clusters with an @INACTIVE@ status may remain discoverable in your account for a period of time. However, this behavior is subject to change in the future, so you should not rely on @INACTIVE@ clusters persisting.
--
-- You must deregister all container instances from this cluster before you may delete it. You can list the container instances in a cluster with 'ListContainerInstances' and deregister them with 'DeregisterContainerInstance' .
module Network.AWS.ECS.DeleteCluster
  ( -- * Creating a request
    DeleteCluster (..),
    mkDeleteCluster,

    -- ** Request lenses
    dcCluster,

    -- * Destructuring the response
    DeleteClusterResponse (..),
    mkDeleteClusterResponse,

    -- ** Response lenses
    drsCluster,
    drsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCluster' smart constructor.
newtype DeleteCluster = DeleteCluster' {cluster :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCluster' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to delete.
mkDeleteCluster ::
  -- | 'cluster'
  Lude.Text ->
  DeleteCluster
mkDeleteCluster pCluster_ = DeleteCluster' {cluster = pCluster_}

-- | The short name or full Amazon Resource Name (ARN) of the cluster to delete.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCluster :: Lens.Lens' DeleteCluster Lude.Text
dcCluster = Lens.lens (cluster :: DeleteCluster -> Lude.Text) (\s a -> s {cluster = a} :: DeleteCluster)
{-# DEPRECATED dcCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

instance Lude.AWSRequest DeleteCluster where
  type Rs DeleteCluster = DeleteClusterResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteClusterResponse'
            Lude.<$> (x Lude..?> "cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DeleteCluster" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCluster where
  toJSON DeleteCluster' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("cluster" Lude..= cluster)])

instance Lude.ToPath DeleteCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { cluster ::
      Lude.Maybe Cluster,
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

-- | Creates a value of 'DeleteClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - The full description of the deleted cluster.
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

-- | The full description of the deleted cluster.
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
