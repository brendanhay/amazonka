{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetConsoleScreenshot (..)
    , mkGetConsoleScreenshot
    -- ** Request lenses
    , gcsInstanceId
    , gcsDryRun
    , gcsWakeUp

    -- * Destructuring the response
    , GetConsoleScreenshotResponse (..)
    , mkGetConsoleScreenshotResponse
    -- ** Response lenses
    , gcsrrsImageData
    , gcsrrsInstanceId
    , gcsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConsoleScreenshot' smart constructor.
data GetConsoleScreenshot = GetConsoleScreenshot'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , wakeUp :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , acts as keystroke input and wakes up an instance that's in standby or "sleep" mode.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConsoleScreenshot' value with any optional fields omitted.
mkGetConsoleScreenshot
    :: Types.InstanceId -- ^ 'instanceId'
    -> GetConsoleScreenshot
mkGetConsoleScreenshot instanceId
  = GetConsoleScreenshot'{instanceId, dryRun = Core.Nothing,
                          wakeUp = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsInstanceId :: Lens.Lens' GetConsoleScreenshot Types.InstanceId
gcsInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gcsInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsDryRun :: Lens.Lens' GetConsoleScreenshot (Core.Maybe Core.Bool)
gcsDryRun = Lens.field @"dryRun"
{-# INLINEABLE gcsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | When set to @true@ , acts as keystroke input and wakes up an instance that's in standby or "sleep" mode.
--
-- /Note:/ Consider using 'wakeUp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsWakeUp :: Lens.Lens' GetConsoleScreenshot (Core.Maybe Core.Bool)
gcsWakeUp = Lens.field @"wakeUp"
{-# INLINEABLE gcsWakeUp #-}
{-# DEPRECATED wakeUp "Use generic-lens or generic-optics with 'wakeUp' instead"  #-}

instance Core.ToQuery GetConsoleScreenshot where
        toQuery GetConsoleScreenshot{..}
          = Core.toQueryPair "Action" ("GetConsoleScreenshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "WakeUp") wakeUp

instance Core.ToHeaders GetConsoleScreenshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetConsoleScreenshot where
        type Rs GetConsoleScreenshot = GetConsoleScreenshotResponse
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
          = Response.receiveXML
              (\ s h x ->
                 GetConsoleScreenshotResponse' Core.<$>
                   (x Core..@? "imageData") Core.<*> x Core..@? "instanceId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetConsoleScreenshotResponse' smart constructor.
data GetConsoleScreenshotResponse = GetConsoleScreenshotResponse'
  { imageData :: Core.Maybe Core.Text
    -- ^ The data that comprises the image.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConsoleScreenshotResponse' value with any optional fields omitted.
mkGetConsoleScreenshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetConsoleScreenshotResponse
mkGetConsoleScreenshotResponse responseStatus
  = GetConsoleScreenshotResponse'{imageData = Core.Nothing,
                                  instanceId = Core.Nothing, responseStatus}

-- | The data that comprises the image.
--
-- /Note:/ Consider using 'imageData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsImageData :: Lens.Lens' GetConsoleScreenshotResponse (Core.Maybe Core.Text)
gcsrrsImageData = Lens.field @"imageData"
{-# INLINEABLE gcsrrsImageData #-}
{-# DEPRECATED imageData "Use generic-lens or generic-optics with 'imageData' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsInstanceId :: Lens.Lens' GetConsoleScreenshotResponse (Core.Maybe Core.Text)
gcsrrsInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gcsrrsInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsResponseStatus :: Lens.Lens' GetConsoleScreenshotResponse Core.Int
gcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
