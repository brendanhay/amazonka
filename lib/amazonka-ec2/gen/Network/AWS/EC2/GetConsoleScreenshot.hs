{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetConsoleScreenshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JPG-format screenshot of a running instance to help with troubleshooting.
--
-- The returned content is Base64-encoded.
module Network.AWS.EC2.GetConsoleScreenshot
  ( -- * Creating a request
    GetConsoleScreenshot (..),
    mkGetConsoleScreenshot,

    -- ** Request lenses
    gcsInstanceId,
    gcsDryRun,
    gcsWakeUp,

    -- * Destructuring the response
    GetConsoleScreenshotResponse (..),
    mkGetConsoleScreenshotResponse,

    -- ** Response lenses
    gcsrrsImageData,
    gcsrrsInstanceId,
    gcsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConsoleScreenshot' smart constructor.
data GetConsoleScreenshot = GetConsoleScreenshot'
  { -- | The ID of the instance.
    instanceId :: Types.InstanceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | When set to @true@ , acts as keystroke input and wakes up an instance that's in standby or "sleep" mode.
    wakeUp :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConsoleScreenshot' value with any optional fields omitted.
mkGetConsoleScreenshot ::
  -- | 'instanceId'
  Types.InstanceId ->
  GetConsoleScreenshot
mkGetConsoleScreenshot instanceId =
  GetConsoleScreenshot'
    { instanceId,
      dryRun = Core.Nothing,
      wakeUp = Core.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsInstanceId :: Lens.Lens' GetConsoleScreenshot Types.InstanceId
gcsInstanceId = Lens.field @"instanceId"
{-# DEPRECATED gcsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsDryRun :: Lens.Lens' GetConsoleScreenshot (Core.Maybe Core.Bool)
gcsDryRun = Lens.field @"dryRun"
{-# DEPRECATED gcsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | When set to @true@ , acts as keystroke input and wakes up an instance that's in standby or "sleep" mode.
--
-- /Note:/ Consider using 'wakeUp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsWakeUp :: Lens.Lens' GetConsoleScreenshot (Core.Maybe Core.Bool)
gcsWakeUp = Lens.field @"wakeUp"
{-# DEPRECATED gcsWakeUp "Use generic-lens or generic-optics with 'wakeUp' instead." #-}

instance Core.AWSRequest GetConsoleScreenshot where
  type Rs GetConsoleScreenshot = GetConsoleScreenshotResponse
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
            ( Core.pure ("Action", "GetConsoleScreenshot")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "WakeUp" Core.<$> wakeUp)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetConsoleScreenshotResponse'
            Core.<$> (x Core..@? "imageData")
            Core.<*> (x Core..@? "instanceId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetConsoleScreenshotResponse' smart constructor.
data GetConsoleScreenshotResponse = GetConsoleScreenshotResponse'
  { -- | The data that comprises the image.
    imageData :: Core.Maybe Types.String,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConsoleScreenshotResponse' value with any optional fields omitted.
mkGetConsoleScreenshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetConsoleScreenshotResponse
mkGetConsoleScreenshotResponse responseStatus =
  GetConsoleScreenshotResponse'
    { imageData = Core.Nothing,
      instanceId = Core.Nothing,
      responseStatus
    }

-- | The data that comprises the image.
--
-- /Note:/ Consider using 'imageData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsImageData :: Lens.Lens' GetConsoleScreenshotResponse (Core.Maybe Types.String)
gcsrrsImageData = Lens.field @"imageData"
{-# DEPRECATED gcsrrsImageData "Use generic-lens or generic-optics with 'imageData' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsInstanceId :: Lens.Lens' GetConsoleScreenshotResponse (Core.Maybe Types.String)
gcsrrsInstanceId = Lens.field @"instanceId"
{-# DEPRECATED gcsrrsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsResponseStatus :: Lens.Lens' GetConsoleScreenshotResponse Core.Int
gcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
