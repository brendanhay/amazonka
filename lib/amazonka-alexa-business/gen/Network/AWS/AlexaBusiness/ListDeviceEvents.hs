{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListDeviceEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the device event history, including device connection status, for up to 30 days.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListDeviceEvents
  ( -- * Creating a request
    ListDeviceEvents (..),
    mkListDeviceEvents,

    -- ** Request lenses
    ldeDeviceArn,
    ldeEventType,
    ldeMaxResults,
    ldeNextToken,

    -- * Destructuring the response
    ListDeviceEventsResponse (..),
    mkListDeviceEventsResponse,

    -- ** Response lenses
    lderrsDeviceEvents,
    lderrsNextToken,
    lderrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDeviceEvents' smart constructor.
data ListDeviceEvents = ListDeviceEvents'
  { -- | The ARN of a device.
    deviceArn :: Types.Arn,
    -- | The event type to filter device events. If EventType isn't specified, this returns a list of all device events in reverse chronological order. If EventType is specified, this returns a list of device events for that EventType in reverse chronological order.
    eventType :: Core.Maybe Types.DeviceEventType,
    -- | The maximum number of results to include in the response. The default value is 50. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults. When the end of results is reached, the response has a value of null.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeviceEvents' value with any optional fields omitted.
mkListDeviceEvents ::
  -- | 'deviceArn'
  Types.Arn ->
  ListDeviceEvents
mkListDeviceEvents deviceArn =
  ListDeviceEvents'
    { deviceArn,
      eventType = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ARN of a device.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeDeviceArn :: Lens.Lens' ListDeviceEvents Types.Arn
ldeDeviceArn = Lens.field @"deviceArn"
{-# DEPRECATED ldeDeviceArn "Use generic-lens or generic-optics with 'deviceArn' instead." #-}

-- | The event type to filter device events. If EventType isn't specified, this returns a list of all device events in reverse chronological order. If EventType is specified, this returns a list of device events for that EventType in reverse chronological order.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeEventType :: Lens.Lens' ListDeviceEvents (Core.Maybe Types.DeviceEventType)
ldeEventType = Lens.field @"eventType"
{-# DEPRECATED ldeEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | The maximum number of results to include in the response. The default value is 50. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeMaxResults :: Lens.Lens' ListDeviceEvents (Core.Maybe Core.Natural)
ldeMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults. When the end of results is reached, the response has a value of null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeNextToken :: Lens.Lens' ListDeviceEvents (Core.Maybe Types.NextToken)
ldeNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListDeviceEvents where
  toJSON ListDeviceEvents {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeviceArn" Core..= deviceArn),
            ("EventType" Core..=) Core.<$> eventType,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListDeviceEvents where
  type Rs ListDeviceEvents = ListDeviceEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.ListDeviceEvents")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceEventsResponse'
            Core.<$> (x Core..:? "DeviceEvents")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDeviceEvents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"deviceEvents" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDeviceEventsResponse' smart constructor.
data ListDeviceEventsResponse = ListDeviceEventsResponse'
  { -- | The device events requested for the device ARN.
    deviceEvents :: Core.Maybe [Types.DeviceEvent],
    -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDeviceEventsResponse' value with any optional fields omitted.
mkListDeviceEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDeviceEventsResponse
mkListDeviceEventsResponse responseStatus =
  ListDeviceEventsResponse'
    { deviceEvents = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The device events requested for the device ARN.
--
-- /Note:/ Consider using 'deviceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lderrsDeviceEvents :: Lens.Lens' ListDeviceEventsResponse (Core.Maybe [Types.DeviceEvent])
lderrsDeviceEvents = Lens.field @"deviceEvents"
{-# DEPRECATED lderrsDeviceEvents "Use generic-lens or generic-optics with 'deviceEvents' instead." #-}

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lderrsNextToken :: Lens.Lens' ListDeviceEventsResponse (Core.Maybe Types.NextToken)
lderrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lderrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lderrsResponseStatus :: Lens.Lens' ListDeviceEventsResponse Core.Int
lderrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lderrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
