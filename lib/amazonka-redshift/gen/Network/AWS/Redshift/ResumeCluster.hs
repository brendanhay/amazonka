{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ResumeCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes a paused cluster.
module Network.AWS.Redshift.ResumeCluster
  ( -- * Creating a request
    ResumeCluster (..),
    mkResumeCluster,

    -- ** Request lenses
    rcgClusterIdentifier,

    -- * Destructuring the response
    ResumeClusterResponse (..),
    mkResumeClusterResponse,

    -- ** Response lenses
    rcrsCluster,
    rcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Describes a resume cluster operation. For example, a scheduled action to run the @ResumeCluster@ API operation.
--
-- /See:/ 'mkResumeCluster' smart constructor.
newtype ResumeCluster = ResumeCluster'
  { -- | The identifier of the cluster to be resumed.
    clusterIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResumeCluster' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The identifier of the cluster to be resumed.
mkResumeCluster ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  ResumeCluster
mkResumeCluster pClusterIdentifier_ =
  ResumeCluster' {clusterIdentifier = pClusterIdentifier_}

-- | The identifier of the cluster to be resumed.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcgClusterIdentifier :: Lens.Lens' ResumeCluster Lude.Text
rcgClusterIdentifier = Lens.lens (clusterIdentifier :: ResumeCluster -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ResumeCluster)
{-# DEPRECATED rcgClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest ResumeCluster where
  type Rs ResumeCluster = ResumeClusterResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ResumeClusterResult"
      ( \s h x ->
          ResumeClusterResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResumeCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResumeCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery ResumeCluster where
  toQuery ResumeCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ResumeCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]

-- | /See:/ 'mkResumeClusterResponse' smart constructor.
data ResumeClusterResponse = ResumeClusterResponse'
  { cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResumeClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' -
-- * 'responseStatus' - The response status code.
mkResumeClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResumeClusterResponse
mkResumeClusterResponse pResponseStatus_ =
  ResumeClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsCluster :: Lens.Lens' ResumeClusterResponse (Lude.Maybe Cluster)
rcrsCluster = Lens.lens (cluster :: ResumeClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: ResumeClusterResponse)
{-# DEPRECATED rcrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsResponseStatus :: Lens.Lens' ResumeClusterResponse Lude.Int
rcrsResponseStatus = Lens.lens (responseStatus :: ResumeClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResumeClusterResponse)
{-# DEPRECATED rcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
