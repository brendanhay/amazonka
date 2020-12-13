{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a global database cluster. The primary and secondary clusters must already be detached or destroyed first.
module Network.AWS.RDS.DeleteGlobalCluster
  ( -- * Creating a request
    DeleteGlobalCluster (..),
    mkDeleteGlobalCluster,

    -- ** Request lenses
    dGlobalClusterIdentifier,

    -- * Destructuring the response
    DeleteGlobalClusterResponse (..),
    mkDeleteGlobalClusterResponse,

    -- ** Response lenses
    dgcrsGlobalCluster,
    dgcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGlobalCluster' smart constructor.
newtype DeleteGlobalCluster = DeleteGlobalCluster'
  { -- | The cluster identifier of the global database cluster being deleted.
    globalClusterIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGlobalCluster' with the minimum fields required to make a request.
--
-- * 'globalClusterIdentifier' - The cluster identifier of the global database cluster being deleted.
mkDeleteGlobalCluster ::
  -- | 'globalClusterIdentifier'
  Lude.Text ->
  DeleteGlobalCluster
mkDeleteGlobalCluster pGlobalClusterIdentifier_ =
  DeleteGlobalCluster'
    { globalClusterIdentifier =
        pGlobalClusterIdentifier_
    }

-- | The cluster identifier of the global database cluster being deleted.
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGlobalClusterIdentifier :: Lens.Lens' DeleteGlobalCluster Lude.Text
dGlobalClusterIdentifier = Lens.lens (globalClusterIdentifier :: DeleteGlobalCluster -> Lude.Text) (\s a -> s {globalClusterIdentifier = a} :: DeleteGlobalCluster)
{-# DEPRECATED dGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

instance Lude.AWSRequest DeleteGlobalCluster where
  type Rs DeleteGlobalCluster = DeleteGlobalClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteGlobalClusterResult"
      ( \s h x ->
          DeleteGlobalClusterResponse'
            Lude.<$> (x Lude..@? "GlobalCluster")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGlobalCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteGlobalCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGlobalCluster where
  toQuery DeleteGlobalCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteGlobalCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "GlobalClusterIdentifier" Lude.=: globalClusterIdentifier
      ]

-- | /See:/ 'mkDeleteGlobalClusterResponse' smart constructor.
data DeleteGlobalClusterResponse = DeleteGlobalClusterResponse'
  { globalCluster :: Lude.Maybe GlobalCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGlobalClusterResponse' with the minimum fields required to make a request.
--
-- * 'globalCluster' -
-- * 'responseStatus' - The response status code.
mkDeleteGlobalClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGlobalClusterResponse
mkDeleteGlobalClusterResponse pResponseStatus_ =
  DeleteGlobalClusterResponse'
    { globalCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrsGlobalCluster :: Lens.Lens' DeleteGlobalClusterResponse (Lude.Maybe GlobalCluster)
dgcrsGlobalCluster = Lens.lens (globalCluster :: DeleteGlobalClusterResponse -> Lude.Maybe GlobalCluster) (\s a -> s {globalCluster = a} :: DeleteGlobalClusterResponse)
{-# DEPRECATED dgcrsGlobalCluster "Use generic-lens or generic-optics with 'globalCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrsResponseStatus :: Lens.Lens' DeleteGlobalClusterResponse Lude.Int
dgcrsResponseStatus = Lens.lens (responseStatus :: DeleteGlobalClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGlobalClusterResponse)
{-# DEPRECATED dgcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
