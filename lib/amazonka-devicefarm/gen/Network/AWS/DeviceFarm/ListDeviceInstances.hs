{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListDeviceInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the private device instances associated with one or more AWS accounts.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListDeviceInstances
  ( -- * Creating a request
    ListDeviceInstances (..),
    mkListDeviceInstances,

    -- ** Request lenses
    ldiMaxResults,
    ldiNextToken,

    -- * Destructuring the response
    ListDeviceInstancesResponse (..),
    mkListDeviceInstancesResponse,

    -- ** Response lenses
    ldirrsDeviceInstances,
    ldirrsNextToken,
    ldirrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDeviceInstances' smart constructor.
data ListDeviceInstances = ListDeviceInstances'
  { -- | An integer that specifies the maximum number of items you want to return in the API response.
    maxResults :: Core.Maybe Core.Int,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeviceInstances' value with any optional fields omitted.
mkListDeviceInstances ::
  ListDeviceInstances
mkListDeviceInstances =
  ListDeviceInstances'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | An integer that specifies the maximum number of items you want to return in the API response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiMaxResults :: Lens.Lens' ListDeviceInstances (Core.Maybe Core.Int)
ldiMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiNextToken :: Lens.Lens' ListDeviceInstances (Core.Maybe Types.PaginationToken)
ldiNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListDeviceInstances where
  toJSON ListDeviceInstances {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListDeviceInstances where
  type Rs ListDeviceInstances = ListDeviceInstancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.ListDeviceInstances")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceInstancesResponse'
            Core.<$> (x Core..:? "deviceInstances")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDeviceInstances where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"deviceInstances" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDeviceInstancesResponse' smart constructor.
data ListDeviceInstancesResponse = ListDeviceInstancesResponse'
  { -- | An object that contains information about your device instances.
    deviceInstances :: Core.Maybe [Types.DeviceInstance],
    -- | An identifier that can be used in the next call to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeviceInstancesResponse' value with any optional fields omitted.
mkListDeviceInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDeviceInstancesResponse
mkListDeviceInstancesResponse responseStatus =
  ListDeviceInstancesResponse'
    { deviceInstances = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An object that contains information about your device instances.
--
-- /Note:/ Consider using 'deviceInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirrsDeviceInstances :: Lens.Lens' ListDeviceInstancesResponse (Core.Maybe [Types.DeviceInstance])
ldirrsDeviceInstances = Lens.field @"deviceInstances"
{-# DEPRECATED ldirrsDeviceInstances "Use generic-lens or generic-optics with 'deviceInstances' instead." #-}

-- | An identifier that can be used in the next call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirrsNextToken :: Lens.Lens' ListDeviceInstancesResponse (Core.Maybe Types.PaginationToken)
ldirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirrsResponseStatus :: Lens.Lens' ListDeviceInstancesResponse Core.Int
ldirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
