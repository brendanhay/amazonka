{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.LookupEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Looks up <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-concepts.html#cloudtrail-concepts-management-events management events> or <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-concepts.html#cloudtrail-concepts-insights-events CloudTrail Insights events> that are captured by CloudTrail. You can look up events that occurred in a region within the last 90 days. Lookup supports the following attributes for management events:
--
--
--     * AWS access key
--
--
--     * Event ID
--
--
--     * Event name
--
--
--     * Event source
--
--
--     * Read only
--
--
--     * Resource name
--
--
--     * Resource type
--
--
--     * User name
--
--
-- Lookup supports the following attributes for Insights events:
--
--     * Event ID
--
--
--     * Event name
--
--
--     * Event source
--
--
-- All attributes are optional. The default number of results returned is 50, with a maximum of 50 possible. The response includes a token that you can use to get the next page of results.
-- /Important:/ The rate of lookup requests is limited to two per second, per account, per region. If this limit is exceeded, a throttling error occurs.
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.LookupEvents
  ( -- * Creating a request
    LookupEvents (..),
    mkLookupEvents,

    -- ** Request lenses
    leEndTime,
    leEventCategory,
    leLookupAttributes,
    leMaxResults,
    leNextToken,
    leStartTime,

    -- * Destructuring the response
    LookupEventsResponse (..),
    mkLookupEventsResponse,

    -- ** Response lenses
    lerrsEvents,
    lerrsNextToken,
    lerrsResponseStatus,
  )
where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains a request for LookupEvents.
--
-- /See:/ 'mkLookupEvents' smart constructor.
data LookupEvents = LookupEvents'
  { -- | Specifies that only events that occur before or at the specified time are returned. If the specified end time is before the specified start time, an error is returned.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | Specifies the event category. If you do not specify an event category, events of the category are not returned in the response. For example, if you do not specify @insight@ as the value of @EventCategory@ , no Insights events are returned.
    eventCategory :: Core.Maybe Types.EventCategory,
    -- | Contains a list of lookup attributes. Currently the list can contain only one item.
    lookupAttributes :: Core.Maybe [Types.LookupAttribute],
    -- | The number of events to return. Possible values are 1 through 50. The default is 50.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Specifies that only events that occur after or at the specified time are returned. If the specified start time is after the specified end time, an error is returned.
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LookupEvents' value with any optional fields omitted.
mkLookupEvents ::
  LookupEvents
mkLookupEvents =
  LookupEvents'
    { endTime = Core.Nothing,
      eventCategory = Core.Nothing,
      lookupAttributes = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      startTime = Core.Nothing
    }

-- | Specifies that only events that occur before or at the specified time are returned. If the specified end time is before the specified start time, an error is returned.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leEndTime :: Lens.Lens' LookupEvents (Core.Maybe Core.NominalDiffTime)
leEndTime = Lens.field @"endTime"
{-# DEPRECATED leEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Specifies the event category. If you do not specify an event category, events of the category are not returned in the response. For example, if you do not specify @insight@ as the value of @EventCategory@ , no Insights events are returned.
--
-- /Note:/ Consider using 'eventCategory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leEventCategory :: Lens.Lens' LookupEvents (Core.Maybe Types.EventCategory)
leEventCategory = Lens.field @"eventCategory"
{-# DEPRECATED leEventCategory "Use generic-lens or generic-optics with 'eventCategory' instead." #-}

-- | Contains a list of lookup attributes. Currently the list can contain only one item.
--
-- /Note:/ Consider using 'lookupAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leLookupAttributes :: Lens.Lens' LookupEvents (Core.Maybe [Types.LookupAttribute])
leLookupAttributes = Lens.field @"lookupAttributes"
{-# DEPRECATED leLookupAttributes "Use generic-lens or generic-optics with 'lookupAttributes' instead." #-}

-- | The number of events to return. Possible values are 1 through 50. The default is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' LookupEvents (Core.Maybe Core.Natural)
leMaxResults = Lens.field @"maxResults"
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' LookupEvents (Core.Maybe Types.NextToken)
leNextToken = Lens.field @"nextToken"
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies that only events that occur after or at the specified time are returned. If the specified start time is after the specified end time, an error is returned.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStartTime :: Lens.Lens' LookupEvents (Core.Maybe Core.NominalDiffTime)
leStartTime = Lens.field @"startTime"
{-# DEPRECATED leStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON LookupEvents where
  toJSON LookupEvents {..} =
    Core.object
      ( Core.catMaybes
          [ ("EndTime" Core..=) Core.<$> endTime,
            ("EventCategory" Core..=) Core.<$> eventCategory,
            ("LookupAttributes" Core..=) Core.<$> lookupAttributes,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("StartTime" Core..=) Core.<$> startTime
          ]
      )

instance Core.AWSRequest LookupEvents where
  type Rs LookupEvents = LookupEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.LookupEvents"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          LookupEventsResponse'
            Core.<$> (x Core..:? "Events")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager LookupEvents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"events" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Contains a response to a LookupEvents action.
--
-- /See:/ 'mkLookupEventsResponse' smart constructor.
data LookupEventsResponse = LookupEventsResponse'
  { -- | A list of events returned based on the lookup attributes specified and the CloudTrail event. The events list is sorted by time. The most recent event is listed first.
    events :: Core.Maybe [Types.Event],
    -- | The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LookupEventsResponse' value with any optional fields omitted.
mkLookupEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  LookupEventsResponse
mkLookupEventsResponse responseStatus =
  LookupEventsResponse'
    { events = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of events returned based on the lookup attributes specified and the CloudTrail event. The events list is sorted by time. The most recent event is listed first.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsEvents :: Lens.Lens' LookupEventsResponse (Core.Maybe [Types.Event])
lerrsEvents = Lens.field @"events"
{-# DEPRECATED lerrsEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsNextToken :: Lens.Lens' LookupEventsResponse (Core.Maybe Types.NextToken)
lerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsResponseStatus :: Lens.Lens' LookupEventsResponse Core.Int
lerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
