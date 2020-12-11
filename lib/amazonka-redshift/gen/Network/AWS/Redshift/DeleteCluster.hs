{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned cluster without its final snapshot being created. A successful response from the web service indicates that the request was received correctly. Use 'DescribeClusters' to monitor the status of the deletion. The delete operation cannot be canceled or reverted once submitted. For more information about managing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
-- If you want to shut down the cluster and retain it for future use, set /SkipFinalClusterSnapshot/ to @false@ and specify a name for /FinalClusterSnapshotIdentifier/ . You can later restore this snapshot to resume using the cluster. If a final cluster snapshot is requested, the status of the cluster will be "final-snapshot" while the snapshot is being taken, then it's "deleting" once Amazon Redshift begins deleting the cluster.
-- For more information about managing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.DeleteCluster
  ( -- * Creating a request
    DeleteCluster (..),
    mkDeleteCluster,

    -- ** Request lenses
    delSkipFinalClusterSnapshot,
    delFinalClusterSnapshotRetentionPeriod,
    delFinalClusterSnapshotIdentifier,
    delClusterIdentifier,

    -- * Destructuring the response
    DeleteClusterResponse (..),
    mkDeleteClusterResponse,

    -- ** Response lenses
    drsCluster,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { skipFinalClusterSnapshot ::
      Lude.Maybe Lude.Bool,
    finalClusterSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    finalClusterSnapshotIdentifier :: Lude.Maybe Lude.Text,
    clusterIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCluster' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The identifier of the cluster to be deleted.
--
-- Constraints:
--
--     * Must contain lowercase characters.
--
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
-- * 'finalClusterSnapshotIdentifier' - The identifier of the final snapshot that is to be created immediately before deleting the cluster. If this parameter is provided, /SkipFinalClusterSnapshot/ must be @false@ .
--
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
-- * 'finalClusterSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
-- * 'skipFinalClusterSnapshot' - Determines whether a final snapshot of the cluster is created before Amazon Redshift deletes the cluster. If @true@ , a final cluster snapshot is not created. If @false@ , a final cluster snapshot is created before the cluster is deleted.
--
-- Default: @false@
mkDeleteCluster ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  DeleteCluster
mkDeleteCluster pClusterIdentifier_ =
  DeleteCluster'
    { skipFinalClusterSnapshot = Lude.Nothing,
      finalClusterSnapshotRetentionPeriod = Lude.Nothing,
      finalClusterSnapshotIdentifier = Lude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | Determines whether a final snapshot of the cluster is created before Amazon Redshift deletes the cluster. If @true@ , a final cluster snapshot is not created. If @false@ , a final cluster snapshot is created before the cluster is deleted.
--
-- Default: @false@
--
-- /Note:/ Consider using 'skipFinalClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delSkipFinalClusterSnapshot :: Lens.Lens' DeleteCluster (Lude.Maybe Lude.Bool)
delSkipFinalClusterSnapshot = Lens.lens (skipFinalClusterSnapshot :: DeleteCluster -> Lude.Maybe Lude.Bool) (\s a -> s {skipFinalClusterSnapshot = a} :: DeleteCluster)
{-# DEPRECATED delSkipFinalClusterSnapshot "Use generic-lens or generic-optics with 'skipFinalClusterSnapshot' instead." #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'finalClusterSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delFinalClusterSnapshotRetentionPeriod :: Lens.Lens' DeleteCluster (Lude.Maybe Lude.Int)
delFinalClusterSnapshotRetentionPeriod = Lens.lens (finalClusterSnapshotRetentionPeriod :: DeleteCluster -> Lude.Maybe Lude.Int) (\s a -> s {finalClusterSnapshotRetentionPeriod = a} :: DeleteCluster)
{-# DEPRECATED delFinalClusterSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'finalClusterSnapshotRetentionPeriod' instead." #-}

-- | The identifier of the final snapshot that is to be created immediately before deleting the cluster. If this parameter is provided, /SkipFinalClusterSnapshot/ must be @false@ .
--
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'finalClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delFinalClusterSnapshotIdentifier :: Lens.Lens' DeleteCluster (Lude.Maybe Lude.Text)
delFinalClusterSnapshotIdentifier = Lens.lens (finalClusterSnapshotIdentifier :: DeleteCluster -> Lude.Maybe Lude.Text) (\s a -> s {finalClusterSnapshotIdentifier = a} :: DeleteCluster)
{-# DEPRECATED delFinalClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'finalClusterSnapshotIdentifier' instead." #-}

-- | The identifier of the cluster to be deleted.
--
-- Constraints:
--
--     * Must contain lowercase characters.
--
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delClusterIdentifier :: Lens.Lens' DeleteCluster Lude.Text
delClusterIdentifier = Lens.lens (clusterIdentifier :: DeleteCluster -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: DeleteCluster)
{-# DEPRECATED delClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest DeleteCluster where
  type Rs DeleteCluster = DeleteClusterResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DeleteClusterResult"
      ( \s h x ->
          DeleteClusterResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCluster where
  toQuery DeleteCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SkipFinalClusterSnapshot" Lude.=: skipFinalClusterSnapshot,
        "FinalClusterSnapshotRetentionPeriod"
          Lude.=: finalClusterSnapshotRetentionPeriod,
        "FinalClusterSnapshotIdentifier"
          Lude.=: finalClusterSnapshotIdentifier,
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]

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
-- * 'cluster' - Undocumented field.
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

-- | Undocumented field.
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
