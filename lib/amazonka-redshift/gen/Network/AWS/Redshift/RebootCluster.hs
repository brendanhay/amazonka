{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RebootCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a cluster. This action is taken as soon as possible. It results in a momentary outage to the cluster, during which the cluster status is set to @rebooting@ . A cluster event is created when the reboot is completed. Any pending cluster modifications (see 'ModifyCluster' ) are applied at this reboot. For more information about managing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.RebootCluster
  ( -- * Creating a request
    RebootCluster (..),
    mkRebootCluster,

    -- ** Request lenses
    rebClusterIdentifier,

    -- * Destructuring the response
    RebootClusterResponse (..),
    mkRebootClusterResponse,

    -- ** Response lenses
    rrsCluster,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRebootCluster' smart constructor.
newtype RebootCluster = RebootCluster'
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

-- | Creates a value of 'RebootCluster' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The cluster identifier.
mkRebootCluster ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  RebootCluster
mkRebootCluster pClusterIdentifier_ =
  RebootCluster' {clusterIdentifier = pClusterIdentifier_}

-- | The cluster identifier.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rebClusterIdentifier :: Lens.Lens' RebootCluster Lude.Text
rebClusterIdentifier = Lens.lens (clusterIdentifier :: RebootCluster -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: RebootCluster)
{-# DEPRECATED rebClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest RebootCluster where
  type Rs RebootCluster = RebootClusterResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "RebootClusterResult"
      ( \s h x ->
          RebootClusterResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebootCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RebootCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery RebootCluster where
  toQuery RebootCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RebootCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]

-- | /See:/ 'mkRebootClusterResponse' smart constructor.
data RebootClusterResponse = RebootClusterResponse'
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

-- | Creates a value of 'RebootClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkRebootClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebootClusterResponse
mkRebootClusterResponse pResponseStatus_ =
  RebootClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsCluster :: Lens.Lens' RebootClusterResponse (Lude.Maybe Cluster)
rrsCluster = Lens.lens (cluster :: RebootClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: RebootClusterResponse)
{-# DEPRECATED rrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RebootClusterResponse Lude.Int
rrsResponseStatus = Lens.lens (responseStatus :: RebootClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebootClusterResponse)
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
