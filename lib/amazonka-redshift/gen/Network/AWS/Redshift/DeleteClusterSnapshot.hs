{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteClusterSnapshot (..),
    mkDeleteClusterSnapshot,

    -- ** Request lenses
    dcsSnapshotIdentifier,
    dcsSnapshotClusterIdentifier,

    -- * Destructuring the response
    DeleteClusterSnapshotResponse (..),
    mkDeleteClusterSnapshotResponse,

    -- ** Response lenses
    dcsrrsSnapshot,
    dcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteClusterSnapshot' smart constructor.
data DeleteClusterSnapshot = DeleteClusterSnapshot'
  { -- | The unique identifier of the manual snapshot to be deleted.
    --
    -- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
    snapshotIdentifier :: Types.SnapshotIdentifier,
    -- | The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
    --
    -- Constraints: Must be the name of valid cluster.
    snapshotClusterIdentifier :: Core.Maybe Types.SnapshotClusterIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterSnapshot' value with any optional fields omitted.
mkDeleteClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Types.SnapshotIdentifier ->
  DeleteClusterSnapshot
mkDeleteClusterSnapshot snapshotIdentifier =
  DeleteClusterSnapshot'
    { snapshotIdentifier,
      snapshotClusterIdentifier = Core.Nothing
    }

-- | The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSnapshotIdentifier :: Lens.Lens' DeleteClusterSnapshot Types.SnapshotIdentifier
dcsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED dcsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints: Must be the name of valid cluster.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSnapshotClusterIdentifier :: Lens.Lens' DeleteClusterSnapshot (Core.Maybe Types.SnapshotClusterIdentifier)
dcsSnapshotClusterIdentifier = Lens.field @"snapshotClusterIdentifier"
{-# DEPRECATED dcsSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}

instance Core.AWSRequest DeleteClusterSnapshot where
  type Rs DeleteClusterSnapshot = DeleteClusterSnapshotResponse
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
            ( Core.pure ("Action", "DeleteClusterSnapshot")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "SnapshotIdentifier" snapshotIdentifier)
                Core.<> ( Core.toQueryValue "SnapshotClusterIdentifier"
                            Core.<$> snapshotClusterIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteClusterSnapshotResult"
      ( \s h x ->
          DeleteClusterSnapshotResponse'
            Core.<$> (x Core..@? "Snapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteClusterSnapshotResponse' smart constructor.
data DeleteClusterSnapshotResponse = DeleteClusterSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteClusterSnapshotResponse' value with any optional fields omitted.
mkDeleteClusterSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteClusterSnapshotResponse
mkDeleteClusterSnapshotResponse responseStatus =
  DeleteClusterSnapshotResponse'
    { snapshot = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsSnapshot :: Lens.Lens' DeleteClusterSnapshotResponse (Core.Maybe Types.Snapshot)
dcsrrsSnapshot = Lens.field @"snapshot"
{-# DEPRECATED dcsrrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsResponseStatus :: Lens.Lens' DeleteClusterSnapshotResponse Core.Int
dcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
