{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves entries from the specified fleet's event log. You can specify a time range to limit the result set. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a collection of event log entries matching the request are returned.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
-- __Related operations__
--
--     * 'CreateFleet'
--
--
--     * 'ListFleets'
--
--
--     * 'DeleteFleet'
--
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--
--     * 'DescribeFleetCapacity'
--
--
--     * 'DescribeFleetPortSettings'
--
--
--     * 'DescribeFleetUtilization'
--
--
--     * 'DescribeRuntimeConfiguration'
--
--
--     * 'DescribeEC2InstanceLimits'
--
--
--     * 'DescribeFleetEvents'
--
--
--
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeFleetEvents
  ( -- * Creating a request
    DescribeFleetEvents (..),
    mkDescribeFleetEvents,

    -- ** Request lenses
    dfeStartTime,
    dfeNextToken,
    dfeEndTime,
    dfeLimit,
    dfeFleetId,

    -- * Destructuring the response
    DescribeFleetEventsResponse (..),
    mkDescribeFleetEventsResponse,

    -- ** Response lenses
    dfersNextToken,
    dfersEvents,
    dfersResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeFleetEvents' smart constructor.
data DescribeFleetEvents = DescribeFleetEvents'
  { startTime ::
      Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp,
    limit :: Lude.Maybe Lude.Natural,
    fleetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetEvents' with the minimum fields required to make a request.
--
-- * 'endTime' - Most recent date to retrieve event logs for. If no end time is specified, this call returns entries from the specified start time up to the present. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
-- * 'fleetId' - A unique identifier for a fleet to get event logs for. You can use either the fleet ID or ARN value.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'startTime' - Earliest date to retrieve event logs for. If no start time is specified, this call returns entries starting from when the fleet was created to the specified end time. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
mkDescribeFleetEvents ::
  -- | 'fleetId'
  Lude.Text ->
  DescribeFleetEvents
mkDescribeFleetEvents pFleetId_ =
  DescribeFleetEvents'
    { startTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = Lude.Nothing,
      limit = Lude.Nothing,
      fleetId = pFleetId_
    }

-- | Earliest date to retrieve event logs for. If no start time is specified, this call returns entries starting from when the fleet was created to the specified end time. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeStartTime :: Lens.Lens' DescribeFleetEvents (Lude.Maybe Lude.Timestamp)
dfeStartTime = Lens.lens (startTime :: DescribeFleetEvents -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: DescribeFleetEvents)
{-# DEPRECATED dfeStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeNextToken :: Lens.Lens' DescribeFleetEvents (Lude.Maybe Lude.Text)
dfeNextToken = Lens.lens (nextToken :: DescribeFleetEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetEvents)
{-# DEPRECATED dfeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Most recent date to retrieve event logs for. If no end time is specified, this call returns entries from the specified start time up to the present. Format is a number expressed in Unix time as milliseconds (ex: "1469498468.057").
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeEndTime :: Lens.Lens' DescribeFleetEvents (Lude.Maybe Lude.Timestamp)
dfeEndTime = Lens.lens (endTime :: DescribeFleetEvents -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DescribeFleetEvents)
{-# DEPRECATED dfeEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeLimit :: Lens.Lens' DescribeFleetEvents (Lude.Maybe Lude.Natural)
dfeLimit = Lens.lens (limit :: DescribeFleetEvents -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeFleetEvents)
{-# DEPRECATED dfeLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for a fleet to get event logs for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeFleetId :: Lens.Lens' DescribeFleetEvents Lude.Text
dfeFleetId = Lens.lens (fleetId :: DescribeFleetEvents -> Lude.Text) (\s a -> s {fleetId = a} :: DescribeFleetEvents)
{-# DEPRECATED dfeFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Page.AWSPager DescribeFleetEvents where
  page rq rs
    | Page.stop (rs Lens.^. dfersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dfersEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dfeNextToken Lens..~ rs Lens.^. dfersNextToken

instance Lude.AWSRequest DescribeFleetEvents where
  type Rs DescribeFleetEvents = DescribeFleetEventsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeFleetEventsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Events" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFleetEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeFleetEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeFleetEvents where
  toJSON DescribeFleetEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StartTime" Lude..=) Lude.<$> startTime,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("FleetId" Lude..= fleetId)
          ]
      )

instance Lude.ToPath DescribeFleetEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFleetEvents where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeFleetEventsResponse' smart constructor.
data DescribeFleetEventsResponse = DescribeFleetEventsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    events :: Lude.Maybe [Event],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetEventsResponse' with the minimum fields required to make a request.
--
-- * 'events' - A collection of objects containing event log entries for the specified fleet.
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkDescribeFleetEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFleetEventsResponse
mkDescribeFleetEventsResponse pResponseStatus_ =
  DescribeFleetEventsResponse'
    { nextToken = Lude.Nothing,
      events = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfersNextToken :: Lens.Lens' DescribeFleetEventsResponse (Lude.Maybe Lude.Text)
dfersNextToken = Lens.lens (nextToken :: DescribeFleetEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetEventsResponse)
{-# DEPRECATED dfersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A collection of objects containing event log entries for the specified fleet.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfersEvents :: Lens.Lens' DescribeFleetEventsResponse (Lude.Maybe [Event])
dfersEvents = Lens.lens (events :: DescribeFleetEventsResponse -> Lude.Maybe [Event]) (\s a -> s {events = a} :: DescribeFleetEventsResponse)
{-# DEPRECATED dfersEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfersResponseStatus :: Lens.Lens' DescribeFleetEventsResponse Lude.Int
dfersResponseStatus = Lens.lens (responseStatus :: DescribeFleetEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFleetEventsResponse)
{-# DEPRECATED dfersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
