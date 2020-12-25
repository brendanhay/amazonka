{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteThreatIntelSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the ThreatIntelSet specified by the ThreatIntelSet ID.
module Network.AWS.GuardDuty.DeleteThreatIntelSet
  ( -- * Creating a request
    DeleteThreatIntelSet (..),
    mkDeleteThreatIntelSet,

    -- ** Request lenses
    dtisDetectorId,
    dtisThreatIntelSetId,

    -- * Destructuring the response
    DeleteThreatIntelSetResponse (..),
    mkDeleteThreatIntelSetResponse,

    -- ** Response lenses
    dtisrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteThreatIntelSet' smart constructor.
data DeleteThreatIntelSet = DeleteThreatIntelSet'
  { -- | The unique ID of the detector that the threatIntelSet is associated with.
    detectorId :: Types.DetectorId,
    -- | The unique ID of the threatIntelSet that you want to delete.
    threatIntelSetId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThreatIntelSet' value with any optional fields omitted.
mkDeleteThreatIntelSet ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'threatIntelSetId'
  Types.String ->
  DeleteThreatIntelSet
mkDeleteThreatIntelSet detectorId threatIntelSetId =
  DeleteThreatIntelSet' {detectorId, threatIntelSetId}

-- | The unique ID of the detector that the threatIntelSet is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtisDetectorId :: Lens.Lens' DeleteThreatIntelSet Types.DetectorId
dtisDetectorId = Lens.field @"detectorId"
{-# DEPRECATED dtisDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The unique ID of the threatIntelSet that you want to delete.
--
-- /Note:/ Consider using 'threatIntelSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtisThreatIntelSetId :: Lens.Lens' DeleteThreatIntelSet Types.String
dtisThreatIntelSetId = Lens.field @"threatIntelSetId"
{-# DEPRECATED dtisThreatIntelSetId "Use generic-lens or generic-optics with 'threatIntelSetId' instead." #-}

instance Core.AWSRequest DeleteThreatIntelSet where
  type Rs DeleteThreatIntelSet = DeleteThreatIntelSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/threatintelset/")
                Core.<> (Core.toText threatIntelSetId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteThreatIntelSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteThreatIntelSetResponse' smart constructor.
newtype DeleteThreatIntelSetResponse = DeleteThreatIntelSetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThreatIntelSetResponse' value with any optional fields omitted.
mkDeleteThreatIntelSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteThreatIntelSetResponse
mkDeleteThreatIntelSetResponse responseStatus =
  DeleteThreatIntelSetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtisrrsResponseStatus :: Lens.Lens' DeleteThreatIntelSetResponse Core.Int
dtisrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtisrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
