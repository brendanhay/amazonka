{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dcfClusterIdentifier,
    dcfFinalClusterSnapshotIdentifier,
    dcfFinalClusterSnapshotRetentionPeriod,
    dcfSkipFinalClusterSnapshot,

    -- * Destructuring the response
    DeleteClusterResponse (..),
    mkDeleteClusterResponse,

    -- ** Response lenses
    dcrfrsCluster,
    dcrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { -- | The identifier of the cluster to be deleted.
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
    clusterIdentifier :: Types.String,
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
    finalClusterSnapshotIdentifier :: Core.Maybe Types.String,
    -- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    -- The default value is -1.
    finalClusterSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | Determines whether a final snapshot of the cluster is created before Amazon Redshift deletes the cluster. If @true@ , a final cluster snapshot is not created. If @false@ , a final cluster snapshot is created before the cluster is deleted.
    --
    -- Default: @false@
    skipFinalClusterSnapshot :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCluster' value with any optional fields omitted.
mkDeleteCluster ::
  -- | 'clusterIdentifier'
  Types.String ->
  DeleteCluster
mkDeleteCluster clusterIdentifier =
  DeleteCluster'
    { clusterIdentifier,
      finalClusterSnapshotIdentifier = Core.Nothing,
      finalClusterSnapshotRetentionPeriod = Core.Nothing,
      skipFinalClusterSnapshot = Core.Nothing
    }

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
dcfClusterIdentifier :: Lens.Lens' DeleteCluster Types.String
dcfClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED dcfClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

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
dcfFinalClusterSnapshotIdentifier :: Lens.Lens' DeleteCluster (Core.Maybe Types.String)
dcfFinalClusterSnapshotIdentifier = Lens.field @"finalClusterSnapshotIdentifier"
{-# DEPRECATED dcfFinalClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'finalClusterSnapshotIdentifier' instead." #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'finalClusterSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfFinalClusterSnapshotRetentionPeriod :: Lens.Lens' DeleteCluster (Core.Maybe Core.Int)
dcfFinalClusterSnapshotRetentionPeriod = Lens.field @"finalClusterSnapshotRetentionPeriod"
{-# DEPRECATED dcfFinalClusterSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'finalClusterSnapshotRetentionPeriod' instead." #-}

-- | Determines whether a final snapshot of the cluster is created before Amazon Redshift deletes the cluster. If @true@ , a final cluster snapshot is not created. If @false@ , a final cluster snapshot is created before the cluster is deleted.
--
-- Default: @false@
--
-- /Note:/ Consider using 'skipFinalClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfSkipFinalClusterSnapshot :: Lens.Lens' DeleteCluster (Core.Maybe Core.Bool)
dcfSkipFinalClusterSnapshot = Lens.field @"skipFinalClusterSnapshot"
{-# DEPRECATED dcfSkipFinalClusterSnapshot "Use generic-lens or generic-optics with 'skipFinalClusterSnapshot' instead." #-}

instance Core.AWSRequest DeleteCluster where
  type Rs DeleteCluster = DeleteClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteCluster")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> ( Core.toQueryValue "FinalClusterSnapshotIdentifier"
                            Core.<$> finalClusterSnapshotIdentifier
                        )
                Core.<> ( Core.toQueryValue "FinalClusterSnapshotRetentionPeriod"
                            Core.<$> finalClusterSnapshotRetentionPeriod
                        )
                Core.<> ( Core.toQueryValue "SkipFinalClusterSnapshot"
                            Core.<$> skipFinalClusterSnapshot
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteClusterResult"
      ( \s h x ->
          DeleteClusterResponse'
            Core.<$> (x Core..@? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteClusterResponse' value with any optional fields omitted.
mkDeleteClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteClusterResponse
mkDeleteClusterResponse responseStatus =
  DeleteClusterResponse' {cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsCluster :: Lens.Lens' DeleteClusterResponse (Core.Maybe Types.Cluster)
dcrfrsCluster = Lens.field @"cluster"
{-# DEPRECATED dcrfrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsResponseStatus :: Lens.Lens' DeleteClusterResponse Core.Int
dcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
