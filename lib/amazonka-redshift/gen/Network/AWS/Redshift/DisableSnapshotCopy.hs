{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DisableSnapshotCopy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the automatic copying of snapshots from one region to another region for a specified cluster.
--
-- If your cluster and its snapshots are encrypted using a customer master key (CMK) from AWS KMS, use 'DeleteSnapshotCopyGrant' to delete the grant that grants Amazon Redshift permission to the CMK in the destination region.
module Network.AWS.Redshift.DisableSnapshotCopy
  ( -- * Creating a request
    DisableSnapshotCopy (..),
    mkDisableSnapshotCopy,

    -- ** Request lenses
    dscClusterIdentifier,

    -- * Destructuring the response
    DisableSnapshotCopyResponse (..),
    mkDisableSnapshotCopyResponse,

    -- ** Response lenses
    dscrsCluster,
    dscrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDisableSnapshotCopy' smart constructor.
newtype DisableSnapshotCopy = DisableSnapshotCopy'
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

-- | Creates a value of 'DisableSnapshotCopy' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The unique identifier of the source cluster that you want to disable copying of snapshots to a destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
mkDisableSnapshotCopy ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  DisableSnapshotCopy
mkDisableSnapshotCopy pClusterIdentifier_ =
  DisableSnapshotCopy' {clusterIdentifier = pClusterIdentifier_}

-- | The unique identifier of the source cluster that you want to disable copying of snapshots to a destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscClusterIdentifier :: Lens.Lens' DisableSnapshotCopy Lude.Text
dscClusterIdentifier = Lens.lens (clusterIdentifier :: DisableSnapshotCopy -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: DisableSnapshotCopy)
{-# DEPRECATED dscClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest DisableSnapshotCopy where
  type Rs DisableSnapshotCopy = DisableSnapshotCopyResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DisableSnapshotCopyResult"
      ( \s h x ->
          DisableSnapshotCopyResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableSnapshotCopy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableSnapshotCopy where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableSnapshotCopy where
  toQuery DisableSnapshotCopy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DisableSnapshotCopy" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]

-- | /See:/ 'mkDisableSnapshotCopyResponse' smart constructor.
data DisableSnapshotCopyResponse = DisableSnapshotCopyResponse'
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

-- | Creates a value of 'DisableSnapshotCopyResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDisableSnapshotCopyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableSnapshotCopyResponse
mkDisableSnapshotCopyResponse pResponseStatus_ =
  DisableSnapshotCopyResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsCluster :: Lens.Lens' DisableSnapshotCopyResponse (Lude.Maybe Cluster)
dscrsCluster = Lens.lens (cluster :: DisableSnapshotCopyResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: DisableSnapshotCopyResponse)
{-# DEPRECATED dscrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsResponseStatus :: Lens.Lens' DisableSnapshotCopyResponse Lude.Int
dscrsResponseStatus = Lens.lens (responseStatus :: DisableSnapshotCopyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableSnapshotCopyResponse)
{-# DEPRECATED dscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
