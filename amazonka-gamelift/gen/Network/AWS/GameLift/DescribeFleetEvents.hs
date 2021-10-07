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
-- Retrieves entries from a fleet\'s event log. Fleet events are initiated
-- by changes in status, such as during fleet creation and termination,
-- changes in capacity, etc. If a fleet has multiple locations, events are
-- also initiated by changes to status and capacity in remote locations.
--
-- You can specify a time range to limit the result set. Use the pagination
-- parameters to retrieve results as a set of sequential pages.
--
-- If successful, a collection of event log entries matching the request
-- are returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
--
-- __Related actions__
--
-- ListFleets | DescribeEC2InstanceLimits | DescribeFleetAttributes |
-- DescribeFleetCapacity | DescribeFleetEvents |
-- DescribeFleetLocationAttributes | DescribeFleetPortSettings |
-- DescribeFleetUtilization | DescribeRuntimeConfiguration |
-- DescribeScalingPolicies |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeFleetEvents' smart constructor.
data DescribeFleetEvents = DescribeFleetEvents'
  { -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The earliest date to retrieve event logs for. If no start time is
    -- specified, this call returns entries starting from when the fleet was
    -- created to the specified end time. Format is a number expressed in Unix
    -- time as milliseconds (ex: \"1469498468.057\").
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The most recent date to retrieve event logs for. If no end time is
    -- specified, this call returns entries from the specified start time up to
    -- the present. Format is a number expressed in Unix time as milliseconds
    -- (ex: \"1469498468.057\").
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for the fleet to get event logs for. You can use
    -- either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetEvents_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'startTime', 'describeFleetEvents_startTime' - The earliest date to retrieve event logs for. If no start time is
-- specified, this call returns entries starting from when the fleet was
-- created to the specified end time. Format is a number expressed in Unix
-- time as milliseconds (ex: \"1469498468.057\").
--
-- 'endTime', 'describeFleetEvents_endTime' - The most recent date to retrieve event logs for. If no end time is
-- specified, this call returns entries from the specified start time up to
-- the present. Format is a number expressed in Unix time as milliseconds
-- (ex: \"1469498468.057\").
--
-- 'limit', 'describeFleetEvents_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'fleetId', 'describeFleetEvents_fleetId' - A unique identifier for the fleet to get event logs for. You can use
-- either the fleet ID or ARN value.
newDescribeFleetEvents ::
  -- | 'fleetId'
  Prelude.Text ->
  DescribeFleetEvents
newDescribeFleetEvents pFleetId_ =
  DescribeFleetEvents'
    { nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      limit = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeFleetEvents_nextToken :: Lens.Lens' DescribeFleetEvents (Prelude.Maybe Prelude.Text)
describeFleetEvents_nextToken = Lens.lens (\DescribeFleetEvents' {nextToken} -> nextToken) (\s@DescribeFleetEvents' {} a -> s {nextToken = a} :: DescribeFleetEvents)

-- | The earliest date to retrieve event logs for. If no start time is
-- specified, this call returns entries starting from when the fleet was
-- created to the specified end time. Format is a number expressed in Unix
-- time as milliseconds (ex: \"1469498468.057\").
describeFleetEvents_startTime :: Lens.Lens' DescribeFleetEvents (Prelude.Maybe Prelude.UTCTime)
describeFleetEvents_startTime = Lens.lens (\DescribeFleetEvents' {startTime} -> startTime) (\s@DescribeFleetEvents' {} a -> s {startTime = a} :: DescribeFleetEvents) Prelude.. Lens.mapping Core._Time

-- | The most recent date to retrieve event logs for. If no end time is
-- specified, this call returns entries from the specified start time up to
-- the present. Format is a number expressed in Unix time as milliseconds
-- (ex: \"1469498468.057\").
describeFleetEvents_endTime :: Lens.Lens' DescribeFleetEvents (Prelude.Maybe Prelude.UTCTime)
describeFleetEvents_endTime = Lens.lens (\DescribeFleetEvents' {endTime} -> endTime) (\s@DescribeFleetEvents' {} a -> s {endTime = a} :: DescribeFleetEvents) Prelude.. Lens.mapping Core._Time

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeFleetEvents_limit :: Lens.Lens' DescribeFleetEvents (Prelude.Maybe Prelude.Natural)
describeFleetEvents_limit = Lens.lens (\DescribeFleetEvents' {limit} -> limit) (\s@DescribeFleetEvents' {} a -> s {limit = a} :: DescribeFleetEvents)

-- | A unique identifier for the fleet to get event logs for. You can use
-- either the fleet ID or ARN value.
describeFleetEvents_fleetId :: Lens.Lens' DescribeFleetEvents Prelude.Text
describeFleetEvents_fleetId = Lens.lens (\DescribeFleetEvents' {fleetId} -> fleetId) (\s@DescribeFleetEvents' {} a -> s {fleetId = a} :: DescribeFleetEvents)

instance Core.AWSPager DescribeFleetEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFleetEventsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFleetEventsResponse_events
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeFleetEvents_nextToken
          Lens..~ rs
          Lens.^? describeFleetEventsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeFleetEvents where
  type
    AWSResponse DescribeFleetEvents =
      DescribeFleetEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetEventsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Events" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetEvents

instance Prelude.NFData DescribeFleetEvents

instance Core.ToHeaders DescribeFleetEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeFleetEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFleetEvents where
  toJSON DescribeFleetEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("StartTime" Core..=) Prelude.<$> startTime,
            ("EndTime" Core..=) Prelude.<$> endTime,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("FleetId" Core..= fleetId)
          ]
      )

instance Core.ToPath DescribeFleetEvents where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFleetEvents where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeFleetEventsResponse' smart constructor.
data DescribeFleetEventsResponse = DescribeFleetEventsResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects containing event log entries for the specified
    -- fleet.
    events :: Prelude.Maybe [Event],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetEventsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'events', 'describeFleetEventsResponse_events' - A collection of objects containing event log entries for the specified
-- fleet.
--
-- 'httpStatus', 'describeFleetEventsResponse_httpStatus' - The response's http status code.
newDescribeFleetEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetEventsResponse
newDescribeFleetEventsResponse pHttpStatus_ =
  DescribeFleetEventsResponse'
    { nextToken =
        Prelude.Nothing,
      events = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeFleetEventsResponse_nextToken :: Lens.Lens' DescribeFleetEventsResponse (Prelude.Maybe Prelude.Text)
describeFleetEventsResponse_nextToken = Lens.lens (\DescribeFleetEventsResponse' {nextToken} -> nextToken) (\s@DescribeFleetEventsResponse' {} a -> s {nextToken = a} :: DescribeFleetEventsResponse)

-- | A collection of objects containing event log entries for the specified
-- fleet.
describeFleetEventsResponse_events :: Lens.Lens' DescribeFleetEventsResponse (Prelude.Maybe [Event])
describeFleetEventsResponse_events = Lens.lens (\DescribeFleetEventsResponse' {events} -> events) (\s@DescribeFleetEventsResponse' {} a -> s {events = a} :: DescribeFleetEventsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFleetEventsResponse_httpStatus :: Lens.Lens' DescribeFleetEventsResponse Prelude.Int
describeFleetEventsResponse_httpStatus = Lens.lens (\DescribeFleetEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetEventsResponse' {} a -> s {httpStatus = a} :: DescribeFleetEventsResponse)

instance Prelude.NFData DescribeFleetEventsResponse
