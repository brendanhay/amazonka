{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves entries from the specified fleet\'s event log. You can specify
-- a time range to limit the result set. Use the pagination parameters to
-- retrieve results as a set of sequential pages. If successful, a
-- collection of event log entries matching the request are returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- __Related operations__
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   Describe fleets:
--
--     -   DescribeFleetAttributes
--
--     -   DescribeFleetCapacity
--
--     -   DescribeFleetPortSettings
--
--     -   DescribeFleetUtilization
--
--     -   DescribeRuntimeConfiguration
--
--     -   DescribeEC2InstanceLimits
--
--     -   DescribeFleetEvents
--
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeFleetEvents
  ( -- * Creating a Request
    DescribeFleetEvents (..),
    newDescribeFleetEvents,

    -- * Request Lenses
    describeFleetEvents_nextToken,
    describeFleetEvents_startTime,
    describeFleetEvents_endTime,
    describeFleetEvents_limit,
    describeFleetEvents_fleetId,

    -- * Destructuring the Response
    DescribeFleetEventsResponse (..),
    newDescribeFleetEventsResponse,

    -- * Response Lenses
    describeFleetEventsResponse_nextToken,
    describeFleetEventsResponse_events,
    describeFleetEventsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeFleetEvents' smart constructor.
data DescribeFleetEvents = DescribeFleetEvents'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Core.Text,
    -- | Earliest date to retrieve event logs for. If no start time is specified,
    -- this call returns entries starting from when the fleet was created to
    -- the specified end time. Format is a number expressed in Unix time as
    -- milliseconds (ex: \"1469498468.057\").
    startTime :: Core.Maybe Core.POSIX,
    -- | Most recent date to retrieve event logs for. If no end time is
    -- specified, this call returns entries from the specified start time up to
    -- the present. Format is a number expressed in Unix time as milliseconds
    -- (ex: \"1469498468.057\").
    endTime :: Core.Maybe Core.POSIX,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural,
    -- | A unique identifier for a fleet to get event logs for. You can use
    -- either the fleet ID or ARN value.
    fleetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFleetEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetEvents_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'startTime', 'describeFleetEvents_startTime' - Earliest date to retrieve event logs for. If no start time is specified,
-- this call returns entries starting from when the fleet was created to
-- the specified end time. Format is a number expressed in Unix time as
-- milliseconds (ex: \"1469498468.057\").
--
-- 'endTime', 'describeFleetEvents_endTime' - Most recent date to retrieve event logs for. If no end time is
-- specified, this call returns entries from the specified start time up to
-- the present. Format is a number expressed in Unix time as milliseconds
-- (ex: \"1469498468.057\").
--
-- 'limit', 'describeFleetEvents_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'fleetId', 'describeFleetEvents_fleetId' - A unique identifier for a fleet to get event logs for. You can use
-- either the fleet ID or ARN value.
newDescribeFleetEvents ::
  -- | 'fleetId'
  Core.Text ->
  DescribeFleetEvents
newDescribeFleetEvents pFleetId_ =
  DescribeFleetEvents'
    { nextToken = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      limit = Core.Nothing,
      fleetId = pFleetId_
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeFleetEvents_nextToken :: Lens.Lens' DescribeFleetEvents (Core.Maybe Core.Text)
describeFleetEvents_nextToken = Lens.lens (\DescribeFleetEvents' {nextToken} -> nextToken) (\s@DescribeFleetEvents' {} a -> s {nextToken = a} :: DescribeFleetEvents)

-- | Earliest date to retrieve event logs for. If no start time is specified,
-- this call returns entries starting from when the fleet was created to
-- the specified end time. Format is a number expressed in Unix time as
-- milliseconds (ex: \"1469498468.057\").
describeFleetEvents_startTime :: Lens.Lens' DescribeFleetEvents (Core.Maybe Core.UTCTime)
describeFleetEvents_startTime = Lens.lens (\DescribeFleetEvents' {startTime} -> startTime) (\s@DescribeFleetEvents' {} a -> s {startTime = a} :: DescribeFleetEvents) Core.. Lens.mapping Core._Time

-- | Most recent date to retrieve event logs for. If no end time is
-- specified, this call returns entries from the specified start time up to
-- the present. Format is a number expressed in Unix time as milliseconds
-- (ex: \"1469498468.057\").
describeFleetEvents_endTime :: Lens.Lens' DescribeFleetEvents (Core.Maybe Core.UTCTime)
describeFleetEvents_endTime = Lens.lens (\DescribeFleetEvents' {endTime} -> endTime) (\s@DescribeFleetEvents' {} a -> s {endTime = a} :: DescribeFleetEvents) Core.. Lens.mapping Core._Time

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeFleetEvents_limit :: Lens.Lens' DescribeFleetEvents (Core.Maybe Core.Natural)
describeFleetEvents_limit = Lens.lens (\DescribeFleetEvents' {limit} -> limit) (\s@DescribeFleetEvents' {} a -> s {limit = a} :: DescribeFleetEvents)

-- | A unique identifier for a fleet to get event logs for. You can use
-- either the fleet ID or ARN value.
describeFleetEvents_fleetId :: Lens.Lens' DescribeFleetEvents Core.Text
describeFleetEvents_fleetId = Lens.lens (\DescribeFleetEvents' {fleetId} -> fleetId) (\s@DescribeFleetEvents' {} a -> s {fleetId = a} :: DescribeFleetEvents)

instance Core.AWSPager DescribeFleetEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFleetEventsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFleetEventsResponse_events Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeFleetEvents_nextToken
          Lens..~ rs
          Lens.^? describeFleetEventsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeFleetEvents where
  type
    AWSResponse DescribeFleetEvents =
      DescribeFleetEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetEventsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Events" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeFleetEvents

instance Core.NFData DescribeFleetEvents

instance Core.ToHeaders DescribeFleetEvents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.DescribeFleetEvents" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeFleetEvents where
  toJSON DescribeFleetEvents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("StartTime" Core..=) Core.<$> startTime,
            ("EndTime" Core..=) Core.<$> endTime,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("FleetId" Core..= fleetId)
          ]
      )

instance Core.ToPath DescribeFleetEvents where
  toPath = Core.const "/"

instance Core.ToQuery DescribeFleetEvents where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeFleetEventsResponse' smart constructor.
data DescribeFleetEventsResponse = DescribeFleetEventsResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A collection of objects containing event log entries for the specified
    -- fleet.
    events :: Core.Maybe [Event],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFleetEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetEventsResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'events', 'describeFleetEventsResponse_events' - A collection of objects containing event log entries for the specified
-- fleet.
--
-- 'httpStatus', 'describeFleetEventsResponse_httpStatus' - The response's http status code.
newDescribeFleetEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeFleetEventsResponse
newDescribeFleetEventsResponse pHttpStatus_ =
  DescribeFleetEventsResponse'
    { nextToken =
        Core.Nothing,
      events = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeFleetEventsResponse_nextToken :: Lens.Lens' DescribeFleetEventsResponse (Core.Maybe Core.Text)
describeFleetEventsResponse_nextToken = Lens.lens (\DescribeFleetEventsResponse' {nextToken} -> nextToken) (\s@DescribeFleetEventsResponse' {} a -> s {nextToken = a} :: DescribeFleetEventsResponse)

-- | A collection of objects containing event log entries for the specified
-- fleet.
describeFleetEventsResponse_events :: Lens.Lens' DescribeFleetEventsResponse (Core.Maybe [Event])
describeFleetEventsResponse_events = Lens.lens (\DescribeFleetEventsResponse' {events} -> events) (\s@DescribeFleetEventsResponse' {} a -> s {events = a} :: DescribeFleetEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFleetEventsResponse_httpStatus :: Lens.Lens' DescribeFleetEventsResponse Core.Int
describeFleetEventsResponse_httpStatus = Lens.lens (\DescribeFleetEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetEventsResponse' {} a -> s {httpStatus = a} :: DescribeFleetEventsResponse)

instance Core.NFData DescribeFleetEventsResponse
