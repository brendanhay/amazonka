{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the snapshots for the specified WorkSpace.
module Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
  ( -- * Creating a request
    DescribeWorkspaceSnapshots (..),
    mkDescribeWorkspaceSnapshots,

    -- ** Request lenses
    dwsWorkspaceId,

    -- * Destructuring the response
    DescribeWorkspaceSnapshotsResponse (..),
    mkDescribeWorkspaceSnapshotsResponse,

    -- ** Response lenses
    dwsrrsRebuildSnapshots,
    dwsrrsRestoreSnapshots,
    dwsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeWorkspaceSnapshots' smart constructor.
newtype DescribeWorkspaceSnapshots = DescribeWorkspaceSnapshots'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Types.WorkspaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkspaceSnapshots' value with any optional fields omitted.
mkDescribeWorkspaceSnapshots ::
  -- | 'workspaceId'
  Types.WorkspaceId ->
  DescribeWorkspaceSnapshots
mkDescribeWorkspaceSnapshots workspaceId =
  DescribeWorkspaceSnapshots' {workspaceId}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsWorkspaceId :: Lens.Lens' DescribeWorkspaceSnapshots Types.WorkspaceId
dwsWorkspaceId = Lens.field @"workspaceId"
{-# DEPRECATED dwsWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Core.FromJSON DescribeWorkspaceSnapshots where
  toJSON DescribeWorkspaceSnapshots {..} =
    Core.object
      (Core.catMaybes [Core.Just ("WorkspaceId" Core..= workspaceId)])

instance Core.AWSRequest DescribeWorkspaceSnapshots where
  type
    Rs DescribeWorkspaceSnapshots =
      DescribeWorkspaceSnapshotsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkspacesService.DescribeWorkspaceSnapshots")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceSnapshotsResponse'
            Core.<$> (x Core..:? "RebuildSnapshots")
            Core.<*> (x Core..:? "RestoreSnapshots")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeWorkspaceSnapshotsResponse' smart constructor.
data DescribeWorkspaceSnapshotsResponse = DescribeWorkspaceSnapshotsResponse'
  { -- | Information about the snapshots that can be used to rebuild a WorkSpace. These snapshots include the user volume.
    rebuildSnapshots :: Core.Maybe [Types.Snapshot],
    -- | Information about the snapshots that can be used to restore a WorkSpace. These snapshots include both the root volume and the user volume.
    restoreSnapshots :: Core.Maybe [Types.Snapshot],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeWorkspaceSnapshotsResponse' value with any optional fields omitted.
mkDescribeWorkspaceSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeWorkspaceSnapshotsResponse
mkDescribeWorkspaceSnapshotsResponse responseStatus =
  DescribeWorkspaceSnapshotsResponse'
    { rebuildSnapshots =
        Core.Nothing,
      restoreSnapshots = Core.Nothing,
      responseStatus
    }

-- | Information about the snapshots that can be used to rebuild a WorkSpace. These snapshots include the user volume.
--
-- /Note:/ Consider using 'rebuildSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsRebuildSnapshots :: Lens.Lens' DescribeWorkspaceSnapshotsResponse (Core.Maybe [Types.Snapshot])
dwsrrsRebuildSnapshots = Lens.field @"rebuildSnapshots"
{-# DEPRECATED dwsrrsRebuildSnapshots "Use generic-lens or generic-optics with 'rebuildSnapshots' instead." #-}

-- | Information about the snapshots that can be used to restore a WorkSpace. These snapshots include both the root volume and the user volume.
--
-- /Note:/ Consider using 'restoreSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsRestoreSnapshots :: Lens.Lens' DescribeWorkspaceSnapshotsResponse (Core.Maybe [Types.Snapshot])
dwsrrsRestoreSnapshots = Lens.field @"restoreSnapshots"
{-# DEPRECATED dwsrrsRestoreSnapshots "Use generic-lens or generic-optics with 'restoreSnapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsResponseStatus :: Lens.Lens' DescribeWorkspaceSnapshotsResponse Core.Int
dwsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
