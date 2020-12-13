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
    leEventCategory,
    leStartTime,
    leLookupAttributes,
    leNextToken,
    leEndTime,
    leMaxResults,

    -- * Destructuring the response
    LookupEventsResponse (..),
    mkLookupEventsResponse,

    -- ** Response lenses
    lersNextToken,
    lersEvents,
    lersResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains a request for LookupEvents.
--
-- /See:/ 'mkLookupEvents' smart constructor.
data LookupEvents = LookupEvents'
  { -- | Specifies the event category. If you do not specify an event category, events of the category are not returned in the response. For example, if you do not specify @insight@ as the value of @EventCategory@ , no Insights events are returned.
    eventCategory :: Lude.Maybe EventCategory,
    -- | Specifies that only events that occur after or at the specified time are returned. If the specified start time is after the specified end time, an error is returned.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | Contains a list of lookup attributes. Currently the list can contain only one item.
    lookupAttributes :: Lude.Maybe [LookupAttribute],
    -- | The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specifies that only events that occur before or at the specified time are returned. If the specified end time is before the specified start time, an error is returned.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The number of events to return. Possible values are 1 through 50. The default is 50.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LookupEvents' with the minimum fields required to make a request.
--
-- * 'eventCategory' - Specifies the event category. If you do not specify an event category, events of the category are not returned in the response. For example, if you do not specify @insight@ as the value of @EventCategory@ , no Insights events are returned.
-- * 'startTime' - Specifies that only events that occur after or at the specified time are returned. If the specified start time is after the specified end time, an error is returned.
-- * 'lookupAttributes' - Contains a list of lookup attributes. Currently the list can contain only one item.
-- * 'nextToken' - The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
-- * 'endTime' - Specifies that only events that occur before or at the specified time are returned. If the specified end time is before the specified start time, an error is returned.
-- * 'maxResults' - The number of events to return. Possible values are 1 through 50. The default is 50.
mkLookupEvents ::
  LookupEvents
mkLookupEvents =
  LookupEvents'
    { eventCategory = Lude.Nothing,
      startTime = Lude.Nothing,
      lookupAttributes = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Specifies the event category. If you do not specify an event category, events of the category are not returned in the response. For example, if you do not specify @insight@ as the value of @EventCategory@ , no Insights events are returned.
--
-- /Note:/ Consider using 'eventCategory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leEventCategory :: Lens.Lens' LookupEvents (Lude.Maybe EventCategory)
leEventCategory = Lens.lens (eventCategory :: LookupEvents -> Lude.Maybe EventCategory) (\s a -> s {eventCategory = a} :: LookupEvents)
{-# DEPRECATED leEventCategory "Use generic-lens or generic-optics with 'eventCategory' instead." #-}

-- | Specifies that only events that occur after or at the specified time are returned. If the specified start time is after the specified end time, an error is returned.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStartTime :: Lens.Lens' LookupEvents (Lude.Maybe Lude.Timestamp)
leStartTime = Lens.lens (startTime :: LookupEvents -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: LookupEvents)
{-# DEPRECATED leStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Contains a list of lookup attributes. Currently the list can contain only one item.
--
-- /Note:/ Consider using 'lookupAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leLookupAttributes :: Lens.Lens' LookupEvents (Lude.Maybe [LookupAttribute])
leLookupAttributes = Lens.lens (lookupAttributes :: LookupEvents -> Lude.Maybe [LookupAttribute]) (\s a -> s {lookupAttributes = a} :: LookupEvents)
{-# DEPRECATED leLookupAttributes "Use generic-lens or generic-optics with 'lookupAttributes' instead." #-}

-- | The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' LookupEvents (Lude.Maybe Lude.Text)
leNextToken = Lens.lens (nextToken :: LookupEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: LookupEvents)
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies that only events that occur before or at the specified time are returned. If the specified end time is before the specified start time, an error is returned.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leEndTime :: Lens.Lens' LookupEvents (Lude.Maybe Lude.Timestamp)
leEndTime = Lens.lens (endTime :: LookupEvents -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: LookupEvents)
{-# DEPRECATED leEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The number of events to return. Possible values are 1 through 50. The default is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' LookupEvents (Lude.Maybe Lude.Natural)
leMaxResults = Lens.lens (maxResults :: LookupEvents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: LookupEvents)
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager LookupEvents where
  page rq rs
    | Page.stop (rs Lens.^. lersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lersEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& leNextToken Lens..~ rs Lens.^. lersNextToken

instance Lude.AWSRequest LookupEvents where
  type Rs LookupEvents = LookupEventsResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          LookupEventsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Events" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders LookupEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.LookupEvents" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON LookupEvents where
  toJSON LookupEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventCategory" Lude..=) Lude.<$> eventCategory,
            ("StartTime" Lude..=) Lude.<$> startTime,
            ("LookupAttributes" Lude..=) Lude.<$> lookupAttributes,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath LookupEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery LookupEvents where
  toQuery = Lude.const Lude.mempty

-- | Contains a response to a LookupEvents action.
--
-- /See:/ 'mkLookupEventsResponse' smart constructor.
data LookupEventsResponse = LookupEventsResponse'
  { -- | The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of events returned based on the lookup attributes specified and the CloudTrail event. The events list is sorted by time. The most recent event is listed first.
    events :: Lude.Maybe [Event],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LookupEventsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
-- * 'events' - A list of events returned based on the lookup attributes specified and the CloudTrail event. The events list is sorted by time. The most recent event is listed first.
-- * 'responseStatus' - The response status code.
mkLookupEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  LookupEventsResponse
mkLookupEventsResponse pResponseStatus_ =
  LookupEventsResponse'
    { nextToken = Lude.Nothing,
      events = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersNextToken :: Lens.Lens' LookupEventsResponse (Lude.Maybe Lude.Text)
lersNextToken = Lens.lens (nextToken :: LookupEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: LookupEventsResponse)
{-# DEPRECATED lersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of events returned based on the lookup attributes specified and the CloudTrail event. The events list is sorted by time. The most recent event is listed first.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersEvents :: Lens.Lens' LookupEventsResponse (Lude.Maybe [Event])
lersEvents = Lens.lens (events :: LookupEventsResponse -> Lude.Maybe [Event]) (\s a -> s {events = a} :: LookupEventsResponse)
{-# DEPRECATED lersEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersResponseStatus :: Lens.Lens' LookupEventsResponse Lude.Int
lersResponseStatus = Lens.lens (responseStatus :: LookupEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: LookupEventsResponse)
{-# DEPRECATED lersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
