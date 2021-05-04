{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeFleetEvents' smart constructor.
data DescribeFleetEvents = DescribeFleetEvents'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Earliest date to retrieve event logs for. If no start time is specified,
    -- this call returns entries starting from when the fleet was created to
    -- the specified end time. Format is a number expressed in Unix time as
    -- milliseconds (ex: \"1469498468.057\").
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | Most recent date to retrieve event logs for. If no end time is
    -- specified, this call returns entries from the specified start time up to
    -- the present. Format is a number expressed in Unix time as milliseconds
    -- (ex: \"1469498468.057\").
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for a fleet to get event logs for. You can use
    -- either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeFleetEvents_nextToken :: Lens.Lens' DescribeFleetEvents (Prelude.Maybe Prelude.Text)
describeFleetEvents_nextToken = Lens.lens (\DescribeFleetEvents' {nextToken} -> nextToken) (\s@DescribeFleetEvents' {} a -> s {nextToken = a} :: DescribeFleetEvents)

-- | Earliest date to retrieve event logs for. If no start time is specified,
-- this call returns entries starting from when the fleet was created to
-- the specified end time. Format is a number expressed in Unix time as
-- milliseconds (ex: \"1469498468.057\").
describeFleetEvents_startTime :: Lens.Lens' DescribeFleetEvents (Prelude.Maybe Prelude.UTCTime)
describeFleetEvents_startTime = Lens.lens (\DescribeFleetEvents' {startTime} -> startTime) (\s@DescribeFleetEvents' {} a -> s {startTime = a} :: DescribeFleetEvents) Prelude.. Lens.mapping Prelude._Time

-- | Most recent date to retrieve event logs for. If no end time is
-- specified, this call returns entries from the specified start time up to
-- the present. Format is a number expressed in Unix time as milliseconds
-- (ex: \"1469498468.057\").
describeFleetEvents_endTime :: Lens.Lens' DescribeFleetEvents (Prelude.Maybe Prelude.UTCTime)
describeFleetEvents_endTime = Lens.lens (\DescribeFleetEvents' {endTime} -> endTime) (\s@DescribeFleetEvents' {} a -> s {endTime = a} :: DescribeFleetEvents) Prelude.. Lens.mapping Prelude._Time

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeFleetEvents_limit :: Lens.Lens' DescribeFleetEvents (Prelude.Maybe Prelude.Natural)
describeFleetEvents_limit = Lens.lens (\DescribeFleetEvents' {limit} -> limit) (\s@DescribeFleetEvents' {} a -> s {limit = a} :: DescribeFleetEvents)

-- | A unique identifier for a fleet to get event logs for. You can use
-- either the fleet ID or ARN value.
describeFleetEvents_fleetId :: Lens.Lens' DescribeFleetEvents Prelude.Text
describeFleetEvents_fleetId = Lens.lens (\DescribeFleetEvents' {fleetId} -> fleetId) (\s@DescribeFleetEvents' {} a -> s {fleetId = a} :: DescribeFleetEvents)

instance Pager.AWSPager DescribeFleetEvents where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeFleetEventsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeFleetEventsResponse_events
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeFleetEvents_nextToken
          Lens..~ rs
          Lens.^? describeFleetEventsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeFleetEvents where
  type
    Rs DescribeFleetEvents =
      DescribeFleetEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetEventsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Events" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetEvents

instance Prelude.NFData DescribeFleetEvents

instance Prelude.ToHeaders DescribeFleetEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.DescribeFleetEvents" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeFleetEvents where
  toJSON DescribeFleetEvents' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("StartTime" Prelude..=) Prelude.<$> startTime,
            ("EndTime" Prelude..=) Prelude.<$> endTime,
            ("Limit" Prelude..=) Prelude.<$> limit,
            Prelude.Just ("FleetId" Prelude..= fleetId)
          ]
      )

instance Prelude.ToPath DescribeFleetEvents where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeFleetEvents where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeFleetEventsResponse' smart constructor.
data DescribeFleetEventsResponse = DescribeFleetEventsResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects containing event log entries for the specified
    -- fleet.
    events :: Prelude.Maybe [Event],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeFleetEventsResponse
newDescribeFleetEventsResponse pHttpStatus_ =
  DescribeFleetEventsResponse'
    { nextToken =
        Prelude.Nothing,
      events = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
describeFleetEventsResponse_nextToken :: Lens.Lens' DescribeFleetEventsResponse (Prelude.Maybe Prelude.Text)
describeFleetEventsResponse_nextToken = Lens.lens (\DescribeFleetEventsResponse' {nextToken} -> nextToken) (\s@DescribeFleetEventsResponse' {} a -> s {nextToken = a} :: DescribeFleetEventsResponse)

-- | A collection of objects containing event log entries for the specified
-- fleet.
describeFleetEventsResponse_events :: Lens.Lens' DescribeFleetEventsResponse (Prelude.Maybe [Event])
describeFleetEventsResponse_events = Lens.lens (\DescribeFleetEventsResponse' {events} -> events) (\s@DescribeFleetEventsResponse' {} a -> s {events = a} :: DescribeFleetEventsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeFleetEventsResponse_httpStatus :: Lens.Lens' DescribeFleetEventsResponse Prelude.Int
describeFleetEventsResponse_httpStatus = Lens.lens (\DescribeFleetEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetEventsResponse' {} a -> s {httpStatus = a} :: DescribeFleetEventsResponse)

instance Prelude.NFData DescribeFleetEventsResponse
