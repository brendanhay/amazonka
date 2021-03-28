{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CopyClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified automated cluster snapshot to a new manual cluster snapshot. The source must be an automated snapshot and it must be in the available state.
--
-- When you delete a cluster, Amazon Redshift deletes any automated snapshots of the cluster. Also, when the retention period of the snapshot expires, Amazon Redshift automatically deletes it. If you want to keep an automated snapshot for a longer period, you can make a manual copy of the snapshot. Manual snapshots are retained until you delete them.
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CopyClusterSnapshot
    (
    -- * Creating a request
      CopyClusterSnapshot (..)
    , mkCopyClusterSnapshot
    -- ** Request lenses
    , ccsfSourceSnapshotIdentifier
    , ccsfTargetSnapshotIdentifier
    , ccsfManualSnapshotRetentionPeriod
    , ccsfSourceSnapshotClusterIdentifier

    -- * Destructuring the response
    , CopyClusterSnapshotResponse (..)
    , mkCopyClusterSnapshotResponse
    -- ** Response lenses
    , ccsrfrsSnapshot
    , ccsrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCopyClusterSnapshot' smart constructor.
data CopyClusterSnapshot = CopyClusterSnapshot'
  { sourceSnapshotIdentifier :: Core.Text
    -- ^ The identifier for the source snapshot.
--
-- Constraints:
--
--     * Must be the identifier for a valid automated snapshot whose state is @available@ .
--
--
  , targetSnapshotIdentifier :: Core.Text
    -- ^ The identifier given to the new manual snapshot.
--
-- Constraints:
--
--     * Cannot be null, empty, or blank.
--
--
--     * Must contain from 1 to 255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for the AWS account that is making the request.
--
--
  , manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. 
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
  , sourceSnapshotClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints:
--
--     * Must be the identifier for a valid cluster.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyClusterSnapshot' value with any optional fields omitted.
mkCopyClusterSnapshot
    :: Core.Text -- ^ 'sourceSnapshotIdentifier'
    -> Core.Text -- ^ 'targetSnapshotIdentifier'
    -> CopyClusterSnapshot
mkCopyClusterSnapshot sourceSnapshotIdentifier
  targetSnapshotIdentifier
  = CopyClusterSnapshot'{sourceSnapshotIdentifier,
                         targetSnapshotIdentifier,
                         manualSnapshotRetentionPeriod = Core.Nothing,
                         sourceSnapshotClusterIdentifier = Core.Nothing}

-- | The identifier for the source snapshot.
--
-- Constraints:
--
--     * Must be the identifier for a valid automated snapshot whose state is @available@ .
--
--
--
-- /Note:/ Consider using 'sourceSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsfSourceSnapshotIdentifier :: Lens.Lens' CopyClusterSnapshot Core.Text
ccsfSourceSnapshotIdentifier = Lens.field @"sourceSnapshotIdentifier"
{-# INLINEABLE ccsfSourceSnapshotIdentifier #-}
{-# DEPRECATED sourceSnapshotIdentifier "Use generic-lens or generic-optics with 'sourceSnapshotIdentifier' instead"  #-}

-- | The identifier given to the new manual snapshot.
--
-- Constraints:
--
--     * Cannot be null, empty, or blank.
--
--
--     * Must contain from 1 to 255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for the AWS account that is making the request.
--
--
--
-- /Note:/ Consider using 'targetSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsfTargetSnapshotIdentifier :: Lens.Lens' CopyClusterSnapshot Core.Text
ccsfTargetSnapshotIdentifier = Lens.field @"targetSnapshotIdentifier"
{-# INLINEABLE ccsfTargetSnapshotIdentifier #-}
{-# DEPRECATED targetSnapshotIdentifier "Use generic-lens or generic-optics with 'targetSnapshotIdentifier' instead"  #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. 
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsfManualSnapshotRetentionPeriod :: Lens.Lens' CopyClusterSnapshot (Core.Maybe Core.Int)
ccsfManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# INLINEABLE ccsfManualSnapshotRetentionPeriod #-}
{-# DEPRECATED manualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead"  #-}

-- | The identifier of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints:
--
--     * Must be the identifier for a valid cluster.
--
--
--
-- /Note:/ Consider using 'sourceSnapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsfSourceSnapshotClusterIdentifier :: Lens.Lens' CopyClusterSnapshot (Core.Maybe Core.Text)
ccsfSourceSnapshotClusterIdentifier = Lens.field @"sourceSnapshotClusterIdentifier"
{-# INLINEABLE ccsfSourceSnapshotClusterIdentifier #-}
{-# DEPRECATED sourceSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'sourceSnapshotClusterIdentifier' instead"  #-}

instance Core.ToQuery CopyClusterSnapshot where
        toQuery CopyClusterSnapshot{..}
          = Core.toQueryPair "Action" ("CopyClusterSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "SourceSnapshotIdentifier"
                sourceSnapshotIdentifier
              Core.<>
              Core.toQueryPair "TargetSnapshotIdentifier"
                targetSnapshotIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ManualSnapshotRetentionPeriod")
                manualSnapshotRetentionPeriod
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SourceSnapshotClusterIdentifier")
                sourceSnapshotClusterIdentifier

instance Core.ToHeaders CopyClusterSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CopyClusterSnapshot where
        type Rs CopyClusterSnapshot = CopyClusterSnapshotResponse
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
          = Response.receiveXMLWrapper "CopyClusterSnapshotResult"
              (\ s h x ->
                 CopyClusterSnapshotResponse' Core.<$>
                   (x Core..@? "Snapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCopyClusterSnapshotResponse' smart constructor.
data CopyClusterSnapshotResponse = CopyClusterSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CopyClusterSnapshotResponse' value with any optional fields omitted.
mkCopyClusterSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CopyClusterSnapshotResponse
mkCopyClusterSnapshotResponse responseStatus
  = CopyClusterSnapshotResponse'{snapshot = Core.Nothing,
                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrfrsSnapshot :: Lens.Lens' CopyClusterSnapshotResponse (Core.Maybe Types.Snapshot)
ccsrfrsSnapshot = Lens.field @"snapshot"
{-# INLINEABLE ccsrfrsSnapshot #-}
{-# DEPRECATED snapshot "Use generic-lens or generic-optics with 'snapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrfrsResponseStatus :: Lens.Lens' CopyClusterSnapshotResponse Core.Int
ccsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
