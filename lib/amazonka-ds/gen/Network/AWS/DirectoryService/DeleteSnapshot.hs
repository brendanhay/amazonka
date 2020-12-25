{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a directory snapshot.
module Network.AWS.DirectoryService.DeleteSnapshot
  ( -- * Creating a request
    DeleteSnapshot (..),
    mkDeleteSnapshot,

    -- ** Request lenses
    dsSnapshotId,

    -- * Destructuring the response
    DeleteSnapshotResponse (..),
    mkDeleteSnapshotResponse,

    -- ** Response lenses
    dsrfrsSnapshotId,
    dsrfrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DeleteSnapshot' operation.
--
-- /See:/ 'mkDeleteSnapshot' smart constructor.
newtype DeleteSnapshot = DeleteSnapshot'
  { -- | The identifier of the directory snapshot to be deleted.
    snapshotId :: Types.SnapshotId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshot' value with any optional fields omitted.
mkDeleteSnapshot ::
  -- | 'snapshotId'
  Types.SnapshotId ->
  DeleteSnapshot
mkDeleteSnapshot snapshotId = DeleteSnapshot' {snapshotId}

-- | The identifier of the directory snapshot to be deleted.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotId :: Lens.Lens' DeleteSnapshot Types.SnapshotId
dsSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED dsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Core.FromJSON DeleteSnapshot where
  toJSON DeleteSnapshot {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SnapshotId" Core..= snapshotId)])

instance Core.AWSRequest DeleteSnapshot where
  type Rs DeleteSnapshot = DeleteSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.DeleteSnapshot")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSnapshotResponse'
            Core.<$> (x Core..:? "SnapshotId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the results of the 'DeleteSnapshot' operation.
--
-- /See:/ 'mkDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  { -- | The identifier of the directory snapshot that was deleted.
    snapshotId :: Core.Maybe Types.SnapshotId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotResponse' value with any optional fields omitted.
mkDeleteSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSnapshotResponse
mkDeleteSnapshotResponse responseStatus =
  DeleteSnapshotResponse'
    { snapshotId = Core.Nothing,
      responseStatus
    }

-- | The identifier of the directory snapshot that was deleted.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsSnapshotId :: Lens.Lens' DeleteSnapshotResponse (Core.Maybe Types.SnapshotId)
dsrfrsSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED dsrfrsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DeleteSnapshotResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
