{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeWorkspaceSnapshots (..)
    , mkDescribeWorkspaceSnapshots
    -- ** Request lenses
    , dwsWorkspaceId

    -- * Destructuring the response
    , DescribeWorkspaceSnapshotsResponse (..)
    , mkDescribeWorkspaceSnapshotsResponse
    -- ** Response lenses
    , dwsrrsRebuildSnapshots
    , dwsrrsRestoreSnapshots
    , dwsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeWorkspaceSnapshots' smart constructor.
newtype DescribeWorkspaceSnapshots = DescribeWorkspaceSnapshots'
  { workspaceId :: Types.WorkspaceId
    -- ^ The identifier of the WorkSpace.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkspaceSnapshots' value with any optional fields omitted.
mkDescribeWorkspaceSnapshots
    :: Types.WorkspaceId -- ^ 'workspaceId'
    -> DescribeWorkspaceSnapshots
mkDescribeWorkspaceSnapshots workspaceId
  = DescribeWorkspaceSnapshots'{workspaceId}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsWorkspaceId :: Lens.Lens' DescribeWorkspaceSnapshots Types.WorkspaceId
dwsWorkspaceId = Lens.field @"workspaceId"
{-# INLINEABLE dwsWorkspaceId #-}
{-# DEPRECATED workspaceId "Use generic-lens or generic-optics with 'workspaceId' instead"  #-}

instance Core.ToQuery DescribeWorkspaceSnapshots where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeWorkspaceSnapshots where
        toHeaders DescribeWorkspaceSnapshots{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.DescribeWorkspaceSnapshots")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeWorkspaceSnapshots where
        toJSON DescribeWorkspaceSnapshots{..}
          = Core.object
              (Core.catMaybes [Core.Just ("WorkspaceId" Core..= workspaceId)])

instance Core.AWSRequest DescribeWorkspaceSnapshots where
        type Rs DescribeWorkspaceSnapshots =
             DescribeWorkspaceSnapshotsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeWorkspaceSnapshotsResponse' Core.<$>
                   (x Core..:? "RebuildSnapshots") Core.<*>
                     x Core..:? "RestoreSnapshots"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeWorkspaceSnapshotsResponse' smart constructor.
data DescribeWorkspaceSnapshotsResponse = DescribeWorkspaceSnapshotsResponse'
  { rebuildSnapshots :: Core.Maybe [Types.Snapshot]
    -- ^ Information about the snapshots that can be used to rebuild a WorkSpace. These snapshots include the user volume.
  , restoreSnapshots :: Core.Maybe [Types.Snapshot]
    -- ^ Information about the snapshots that can be used to restore a WorkSpace. These snapshots include both the root volume and the user volume.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeWorkspaceSnapshotsResponse' value with any optional fields omitted.
mkDescribeWorkspaceSnapshotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeWorkspaceSnapshotsResponse
mkDescribeWorkspaceSnapshotsResponse responseStatus
  = DescribeWorkspaceSnapshotsResponse'{rebuildSnapshots =
                                          Core.Nothing,
                                        restoreSnapshots = Core.Nothing, responseStatus}

-- | Information about the snapshots that can be used to rebuild a WorkSpace. These snapshots include the user volume.
--
-- /Note:/ Consider using 'rebuildSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsRebuildSnapshots :: Lens.Lens' DescribeWorkspaceSnapshotsResponse (Core.Maybe [Types.Snapshot])
dwsrrsRebuildSnapshots = Lens.field @"rebuildSnapshots"
{-# INLINEABLE dwsrrsRebuildSnapshots #-}
{-# DEPRECATED rebuildSnapshots "Use generic-lens or generic-optics with 'rebuildSnapshots' instead"  #-}

-- | Information about the snapshots that can be used to restore a WorkSpace. These snapshots include both the root volume and the user volume.
--
-- /Note:/ Consider using 'restoreSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsRestoreSnapshots :: Lens.Lens' DescribeWorkspaceSnapshotsResponse (Core.Maybe [Types.Snapshot])
dwsrrsRestoreSnapshots = Lens.field @"restoreSnapshots"
{-# INLINEABLE dwsrrsRestoreSnapshots #-}
{-# DEPRECATED restoreSnapshots "Use generic-lens or generic-optics with 'restoreSnapshots' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrrsResponseStatus :: Lens.Lens' DescribeWorkspaceSnapshotsResponse Core.Int
dwsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
