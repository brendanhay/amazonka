{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified manual snapshot. The snapshot must be in the @available@ state, with no other users authorized to access the snapshot. 
--
-- Unlike automated snapshots, manual snapshots are retained even after you delete your cluster. Amazon Redshift does not delete your manual snapshots. You must delete manual snapshot explicitly to avoid getting charged. If other accounts are authorized to access the snapshot, you must revoke all of the authorizations before you can delete the snapshot.
module Network.AWS.Redshift.DeleteClusterSnapshot
    (
    -- * Creating a request
      DeleteClusterSnapshot (..)
    , mkDeleteClusterSnapshot
    -- ** Request lenses
    , dcsSnapshotIdentifier
    , dcsSnapshotClusterIdentifier

    -- * Destructuring the response
    , DeleteClusterSnapshotResponse (..)
    , mkDeleteClusterSnapshotResponse
    -- ** Response lenses
    , dcsrrsSnapshot
    , dcsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteClusterSnapshot' smart constructor.
data DeleteClusterSnapshot = DeleteClusterSnapshot'
  { snapshotIdentifier :: Core.Text
    -- ^ The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
  , snapshotClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints: Must be the name of valid cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterSnapshot' value with any optional fields omitted.
mkDeleteClusterSnapshot
    :: Core.Text -- ^ 'snapshotIdentifier'
    -> DeleteClusterSnapshot
mkDeleteClusterSnapshot snapshotIdentifier
  = DeleteClusterSnapshot'{snapshotIdentifier,
                           snapshotClusterIdentifier = Core.Nothing}

-- | The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSnapshotIdentifier :: Lens.Lens' DeleteClusterSnapshot Core.Text
dcsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE dcsSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

-- | The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints: Must be the name of valid cluster.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSnapshotClusterIdentifier :: Lens.Lens' DeleteClusterSnapshot (Core.Maybe Core.Text)
dcsSnapshotClusterIdentifier = Lens.field @"snapshotClusterIdentifier"
{-# INLINEABLE dcsSnapshotClusterIdentifier #-}
{-# DEPRECATED snapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead"  #-}

instance Core.ToQuery DeleteClusterSnapshot where
        toQuery DeleteClusterSnapshot{..}
          = Core.toQueryPair "Action" ("DeleteClusterSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "SnapshotIdentifier" snapshotIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SnapshotClusterIdentifier")
                snapshotClusterIdentifier

instance Core.ToHeaders DeleteClusterSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteClusterSnapshot where
        type Rs DeleteClusterSnapshot = DeleteClusterSnapshotResponse
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
          = Response.receiveXMLWrapper "DeleteClusterSnapshotResult"
              (\ s h x ->
                 DeleteClusterSnapshotResponse' Core.<$>
                   (x Core..@? "Snapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClusterSnapshotResponse' smart constructor.
data DeleteClusterSnapshotResponse = DeleteClusterSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteClusterSnapshotResponse' value with any optional fields omitted.
mkDeleteClusterSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteClusterSnapshotResponse
mkDeleteClusterSnapshotResponse responseStatus
  = DeleteClusterSnapshotResponse'{snapshot = Core.Nothing,
                                   responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsSnapshot :: Lens.Lens' DeleteClusterSnapshotResponse (Core.Maybe Types.Snapshot)
dcsrrsSnapshot = Lens.field @"snapshot"
{-# INLINEABLE dcsrrsSnapshot #-}
{-# DEPRECATED snapshot "Use generic-lens or generic-optics with 'snapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsResponseStatus :: Lens.Lens' DeleteClusterSnapshotResponse Core.Int
dcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
