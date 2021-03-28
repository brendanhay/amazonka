{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteCluster (..)
    , mkDeleteCluster
    -- ** Request lenses
    , dcfClusterIdentifier
    , dcfFinalClusterSnapshotIdentifier
    , dcfFinalClusterSnapshotRetentionPeriod
    , dcfSkipFinalClusterSnapshot

    -- * Destructuring the response
    , DeleteClusterResponse (..)
    , mkDeleteClusterResponse
    -- ** Response lenses
    , dcrfrsCluster
    , dcrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { clusterIdentifier :: Core.Text
    -- ^ The identifier of the cluster to be deleted.
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
  , finalClusterSnapshotIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the final snapshot that is to be created immediately before deleting the cluster. If this parameter is provided, /SkipFinalClusterSnapshot/ must be @false@ . 
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
  , finalClusterSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
  , skipFinalClusterSnapshot :: Core.Maybe Core.Bool
    -- ^ Determines whether a final snapshot of the cluster is created before Amazon Redshift deletes the cluster. If @true@ , a final cluster snapshot is not created. If @false@ , a final cluster snapshot is created before the cluster is deleted. 
--
-- Default: @false@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCluster' value with any optional fields omitted.
mkDeleteCluster
    :: Core.Text -- ^ 'clusterIdentifier'
    -> DeleteCluster
mkDeleteCluster clusterIdentifier
  = DeleteCluster'{clusterIdentifier,
                   finalClusterSnapshotIdentifier = Core.Nothing,
                   finalClusterSnapshotRetentionPeriod = Core.Nothing,
                   skipFinalClusterSnapshot = Core.Nothing}

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
dcfClusterIdentifier :: Lens.Lens' DeleteCluster Core.Text
dcfClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE dcfClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

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
dcfFinalClusterSnapshotIdentifier :: Lens.Lens' DeleteCluster (Core.Maybe Core.Text)
dcfFinalClusterSnapshotIdentifier = Lens.field @"finalClusterSnapshotIdentifier"
{-# INLINEABLE dcfFinalClusterSnapshotIdentifier #-}
{-# DEPRECATED finalClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'finalClusterSnapshotIdentifier' instead"  #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'finalClusterSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfFinalClusterSnapshotRetentionPeriod :: Lens.Lens' DeleteCluster (Core.Maybe Core.Int)
dcfFinalClusterSnapshotRetentionPeriod = Lens.field @"finalClusterSnapshotRetentionPeriod"
{-# INLINEABLE dcfFinalClusterSnapshotRetentionPeriod #-}
{-# DEPRECATED finalClusterSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'finalClusterSnapshotRetentionPeriod' instead"  #-}

-- | Determines whether a final snapshot of the cluster is created before Amazon Redshift deletes the cluster. If @true@ , a final cluster snapshot is not created. If @false@ , a final cluster snapshot is created before the cluster is deleted. 
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'skipFinalClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfSkipFinalClusterSnapshot :: Lens.Lens' DeleteCluster (Core.Maybe Core.Bool)
dcfSkipFinalClusterSnapshot = Lens.field @"skipFinalClusterSnapshot"
{-# INLINEABLE dcfSkipFinalClusterSnapshot #-}
{-# DEPRECATED skipFinalClusterSnapshot "Use generic-lens or generic-optics with 'skipFinalClusterSnapshot' instead"  #-}

instance Core.ToQuery DeleteCluster where
        toQuery DeleteCluster{..}
          = Core.toQueryPair "Action" ("DeleteCluster" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "FinalClusterSnapshotIdentifier")
                finalClusterSnapshotIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "FinalClusterSnapshotRetentionPeriod")
                finalClusterSnapshotRetentionPeriod
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SkipFinalClusterSnapshot")
                skipFinalClusterSnapshot

instance Core.ToHeaders DeleteCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteCluster where
        type Rs DeleteCluster = DeleteClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DeleteClusterResult"
              (\ s h x ->
                 DeleteClusterResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteClusterResponse' value with any optional fields omitted.
mkDeleteClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteClusterResponse
mkDeleteClusterResponse responseStatus
  = DeleteClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsCluster :: Lens.Lens' DeleteClusterResponse (Core.Maybe Types.Cluster)
dcrfrsCluster = Lens.field @"cluster"
{-# INLINEABLE dcrfrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsResponseStatus :: Lens.Lens' DeleteClusterResponse Core.Int
dcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
