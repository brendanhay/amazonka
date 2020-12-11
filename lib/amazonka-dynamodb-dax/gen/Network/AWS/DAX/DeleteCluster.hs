{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DeleteCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned DAX cluster. /DeleteCluster/ deletes all associated nodes, node endpoints and the DAX cluster itself. When you receive a successful response from this action, DAX immediately begins deleting the cluster; you cannot cancel or revert this action.
module Network.AWS.DAX.DeleteCluster
  ( -- * Creating a request
    DeleteCluster (..),
    mkDeleteCluster,

    -- ** Request lenses
    dcClusterName,

    -- * Destructuring the response
    DeleteClusterResponse (..),
    mkDeleteClusterResponse,

    -- ** Response lenses
    drsCluster,
    drsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCluster' smart constructor.
newtype DeleteCluster = DeleteCluster' {clusterName :: Lude.Text}
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
-- * 'clusterName' - The name of the cluster to be deleted.
mkDeleteCluster ::
  -- | 'clusterName'
  Lude.Text ->
  DeleteCluster
mkDeleteCluster pClusterName_ =
  DeleteCluster' {clusterName = pClusterName_}

-- | The name of the cluster to be deleted.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterName :: Lens.Lens' DeleteCluster Lude.Text
dcClusterName = Lens.lens (clusterName :: DeleteCluster -> Lude.Text) (\s a -> s {clusterName = a} :: DeleteCluster)
{-# DEPRECATED dcClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

instance Lude.AWSRequest DeleteCluster where
  type Rs DeleteCluster = DeleteClusterResponse
  request = Req.postJSON daxService
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
              Lude.=# ("AmazonDAXV3.DeleteCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCluster where
  toJSON DeleteCluster' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ClusterName" Lude..= clusterName)])

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
-- * 'cluster' - A description of the DAX cluster that is being deleted.
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

-- | A description of the DAX cluster that is being deleted.
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
