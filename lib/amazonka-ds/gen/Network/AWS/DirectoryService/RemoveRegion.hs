{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RemoveRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops all replication and removes the domain controllers from the specified Region. You cannot remove the primary Region with this operation. Instead, use the @DeleteDirectory@ API.
module Network.AWS.DirectoryService.RemoveRegion
  ( -- * Creating a request
    RemoveRegion (..),
    mkRemoveRegion,

    -- ** Request lenses
    rrDirectoryId,

    -- * Destructuring the response
    RemoveRegionResponse (..),
    mkRemoveRegionResponse,

    -- ** Response lenses
    rrrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveRegion' smart constructor.
newtype RemoveRegion = RemoveRegion'
  { -- | The identifier of the directory for which you want to remove Region replication.
    directoryId :: Types.DirectoryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveRegion' value with any optional fields omitted.
mkRemoveRegion ::
  -- | 'directoryId'
  Types.DirectoryId ->
  RemoveRegion
mkRemoveRegion directoryId = RemoveRegion' {directoryId}

-- | The identifier of the directory for which you want to remove Region replication.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDirectoryId :: Lens.Lens' RemoveRegion Types.DirectoryId
rrDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED rrDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Core.FromJSON RemoveRegion where
  toJSON RemoveRegion {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DirectoryId" Core..= directoryId)])

instance Core.AWSRequest RemoveRegion where
  type Rs RemoveRegion = RemoveRegionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.RemoveRegion")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveRegionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveRegionResponse' smart constructor.
newtype RemoveRegionResponse = RemoveRegionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveRegionResponse' value with any optional fields omitted.
mkRemoveRegionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveRegionResponse
mkRemoveRegionResponse responseStatus =
  RemoveRegionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrrsResponseStatus :: Lens.Lens' RemoveRegionResponse Core.Int
rrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
