{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RestoreFromSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a directory using an existing directory snapshot.
--
-- When you restore a directory from a snapshot, any changes made to the directory after the snapshot date are overwritten.
-- This action returns as soon as the restore operation is initiated. You can monitor the progress of the restore operation by calling the 'DescribeDirectories' operation with the directory identifier. When the __DirectoryDescription.Stage__ value changes to @Active@ , the restore operation is complete.
module Network.AWS.DirectoryService.RestoreFromSnapshot
  ( -- * Creating a request
    RestoreFromSnapshot (..),
    mkRestoreFromSnapshot,

    -- ** Request lenses
    rfsSnapshotId,

    -- * Destructuring the response
    RestoreFromSnapshotResponse (..),
    mkRestoreFromSnapshotResponse,

    -- ** Response lenses
    rfsrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | An object representing the inputs for the 'RestoreFromSnapshot' operation.
--
-- /See:/ 'mkRestoreFromSnapshot' smart constructor.
newtype RestoreFromSnapshot = RestoreFromSnapshot'
  { -- | The identifier of the snapshot to restore from.
    snapshotId :: Types.SnapshotId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreFromSnapshot' value with any optional fields omitted.
mkRestoreFromSnapshot ::
  -- | 'snapshotId'
  Types.SnapshotId ->
  RestoreFromSnapshot
mkRestoreFromSnapshot snapshotId = RestoreFromSnapshot' {snapshotId}

-- | The identifier of the snapshot to restore from.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfsSnapshotId :: Lens.Lens' RestoreFromSnapshot Types.SnapshotId
rfsSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED rfsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Core.FromJSON RestoreFromSnapshot where
  toJSON RestoreFromSnapshot {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SnapshotId" Core..= snapshotId)])

instance Core.AWSRequest RestoreFromSnapshot where
  type Rs RestoreFromSnapshot = RestoreFromSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.RestoreFromSnapshot")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RestoreFromSnapshotResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Contains the results of the 'RestoreFromSnapshot' operation.
--
-- /See:/ 'mkRestoreFromSnapshotResponse' smart constructor.
newtype RestoreFromSnapshotResponse = RestoreFromSnapshotResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreFromSnapshotResponse' value with any optional fields omitted.
mkRestoreFromSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreFromSnapshotResponse
mkRestoreFromSnapshotResponse responseStatus =
  RestoreFromSnapshotResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfsrrsResponseStatus :: Lens.Lens' RestoreFromSnapshotResponse Core.Int
rfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
