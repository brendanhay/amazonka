{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListInputDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List input devices
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputDevices
    (
    -- * Creating a request
      ListInputDevices (..)
    , mkListInputDevices
    -- ** Request lenses
    , lidMaxResults
    , lidNextToken

    -- * Destructuring the response
    , ListInputDevicesResponse (..)
    , mkListInputDevicesResponse
    -- ** Response lenses
    , lidrrsInputDevices
    , lidrrsNextToken
    , lidrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListInputDevicesRequest
--
-- /See:/ 'mkListInputDevices' smart constructor.
data ListInputDevices = ListInputDevices'
  { maxResults :: Core.Maybe Core.Natural
  , nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInputDevices' value with any optional fields omitted.
mkListInputDevices
    :: ListInputDevices
mkListInputDevices
  = ListInputDevices'{maxResults = Core.Nothing,
                      nextToken = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidMaxResults :: Lens.Lens' ListInputDevices (Core.Maybe Core.Natural)
lidMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lidMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidNextToken :: Lens.Lens' ListInputDevices (Core.Maybe Core.Text)
lidNextToken = Lens.field @"nextToken"
{-# INLINEABLE lidNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListInputDevices where
        toQuery ListInputDevices{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListInputDevices where
        toHeaders ListInputDevices{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListInputDevices where
        type Rs ListInputDevices = ListInputDevicesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/prod/inputDevices",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListInputDevicesResponse' Core.<$>
                   (x Core..:? "inputDevices") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListInputDevices where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"inputDevices" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Placeholder documentation for ListInputDevicesResponse
--
-- /See:/ 'mkListInputDevicesResponse' smart constructor.
data ListInputDevicesResponse = ListInputDevicesResponse'
  { inputDevices :: Core.Maybe [Types.InputDeviceSummary]
    -- ^ The list of input devices.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A token to get additional list results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInputDevicesResponse' value with any optional fields omitted.
mkListInputDevicesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListInputDevicesResponse
mkListInputDevicesResponse responseStatus
  = ListInputDevicesResponse'{inputDevices = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | The list of input devices.
--
-- /Note:/ Consider using 'inputDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidrrsInputDevices :: Lens.Lens' ListInputDevicesResponse (Core.Maybe [Types.InputDeviceSummary])
lidrrsInputDevices = Lens.field @"inputDevices"
{-# INLINEABLE lidrrsInputDevices #-}
{-# DEPRECATED inputDevices "Use generic-lens or generic-optics with 'inputDevices' instead"  #-}

-- | A token to get additional list results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidrrsNextToken :: Lens.Lens' ListInputDevicesResponse (Core.Maybe Core.Text)
lidrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lidrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidrrsResponseStatus :: Lens.Lens' ListInputDevicesResponse Core.Int
lidrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lidrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
