{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.PauseCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pauses a cluster.
module Network.AWS.Redshift.PauseCluster
  ( -- * Creating a request
    PauseCluster (..),
    mkPauseCluster,

    -- ** Request lenses
    pcClusterIdentifier,

    -- * Destructuring the response
    PauseClusterResponse (..),
    mkPauseClusterResponse,

    -- ** Response lenses
    pcrsCluster,
    pcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Describes a pause cluster operation. For example, a scheduled action to run the @PauseCluster@ API operation.
--
-- /See:/ 'mkPauseCluster' smart constructor.
newtype PauseCluster = PauseCluster'
  { clusterIdentifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PauseCluster' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The identifier of the cluster to be paused.
mkPauseCluster ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  PauseCluster
mkPauseCluster pClusterIdentifier_ =
  PauseCluster' {clusterIdentifier = pClusterIdentifier_}

-- | The identifier of the cluster to be paused.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcClusterIdentifier :: Lens.Lens' PauseCluster Lude.Text
pcClusterIdentifier = Lens.lens (clusterIdentifier :: PauseCluster -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: PauseCluster)
{-# DEPRECATED pcClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest PauseCluster where
  type Rs PauseCluster = PauseClusterResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "PauseClusterResult"
      ( \s h x ->
          PauseClusterResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PauseCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PauseCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery PauseCluster where
  toQuery PauseCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PauseCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]

-- | /See:/ 'mkPauseClusterResponse' smart constructor.
data PauseClusterResponse = PauseClusterResponse'
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

-- | Creates a value of 'PauseClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkPauseClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PauseClusterResponse
mkPauseClusterResponse pResponseStatus_ =
  PauseClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsCluster :: Lens.Lens' PauseClusterResponse (Lude.Maybe Cluster)
pcrsCluster = Lens.lens (cluster :: PauseClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: PauseClusterResponse)
{-# DEPRECATED pcrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsResponseStatus :: Lens.Lens' PauseClusterResponse Lude.Int
pcrsResponseStatus = Lens.lens (responseStatus :: PauseClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PauseClusterResponse)
{-# DEPRECATED pcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
