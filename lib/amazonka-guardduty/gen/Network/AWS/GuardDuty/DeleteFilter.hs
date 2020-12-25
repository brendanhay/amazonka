{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the filter specified by the filter name.
module Network.AWS.GuardDuty.DeleteFilter
  ( -- * Creating a request
    DeleteFilter (..),
    mkDeleteFilter,

    -- ** Request lenses
    dfDetectorId,
    dfFilterName,

    -- * Destructuring the response
    DeleteFilterResponse (..),
    mkDeleteFilterResponse,

    -- ** Response lenses
    dfrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFilter' smart constructor.
data DeleteFilter = DeleteFilter'
  { -- | The unique ID of the detector that the filter is associated with.
    detectorId :: Types.DetectorId,
    -- | The name of the filter that you want to delete.
    filterName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFilter' value with any optional fields omitted.
mkDeleteFilter ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'filterName'
  Types.String ->
  DeleteFilter
mkDeleteFilter detectorId filterName =
  DeleteFilter' {detectorId, filterName}

-- | The unique ID of the detector that the filter is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfDetectorId :: Lens.Lens' DeleteFilter Types.DetectorId
dfDetectorId = Lens.field @"detectorId"
{-# DEPRECATED dfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The name of the filter that you want to delete.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFilterName :: Lens.Lens' DeleteFilter Types.String
dfFilterName = Lens.field @"filterName"
{-# DEPRECATED dfFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

instance Core.AWSRequest DeleteFilter where
  type Rs DeleteFilter = DeleteFilterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId) Core.<> ("/filter/")
                Core.<> (Core.toText filterName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFilterResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteFilterResponse' smart constructor.
newtype DeleteFilterResponse = DeleteFilterResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFilterResponse' value with any optional fields omitted.
mkDeleteFilterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteFilterResponse
mkDeleteFilterResponse responseStatus =
  DeleteFilterResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsResponseStatus :: Lens.Lens' DeleteFilterResponse Core.Int
dfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
