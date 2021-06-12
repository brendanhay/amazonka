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
-- Module      : Network.AWS.Redshift.DescribeScheduledActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes properties of scheduled actions.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeScheduledActions
  ( -- * Creating a Request
    DescribeScheduledActions (..),
    newDescribeScheduledActions,

    -- * Request Lenses
    describeScheduledActions_startTime,
    describeScheduledActions_endTime,
    describeScheduledActions_targetActionType,
    describeScheduledActions_active,
    describeScheduledActions_filters,
    describeScheduledActions_scheduledActionName,
    describeScheduledActions_marker,
    describeScheduledActions_maxRecords,

    -- * Destructuring the Response
    DescribeScheduledActionsResponse (..),
    newDescribeScheduledActionsResponse,

    -- * Response Lenses
    describeScheduledActionsResponse_scheduledActions,
    describeScheduledActionsResponse_marker,
    describeScheduledActionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { -- | The start time in UTC of the scheduled actions to retrieve. Only active
    -- scheduled actions that have invocations after this time are retrieved.
    startTime :: Core.Maybe Core.ISO8601,
    -- | The end time in UTC of the scheduled action to retrieve. Only active
    -- scheduled actions that have invocations before this time are retrieved.
    endTime :: Core.Maybe Core.ISO8601,
    -- | The type of the scheduled actions to retrieve.
    targetActionType :: Core.Maybe ScheduledActionTypeValues,
    -- | If true, retrieve only active scheduled actions. If false, retrieve only
    -- disabled scheduled actions.
    active :: Core.Maybe Core.Bool,
    -- | List of scheduled action filters.
    filters :: Core.Maybe [ScheduledActionFilter],
    -- | The name of the scheduled action to retrieve.
    scheduledActionName :: Core.Maybe Core.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeScheduledActions
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeScheduledActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'describeScheduledActions_startTime' - The start time in UTC of the scheduled actions to retrieve. Only active
-- scheduled actions that have invocations after this time are retrieved.
--
-- 'endTime', 'describeScheduledActions_endTime' - The end time in UTC of the scheduled action to retrieve. Only active
-- scheduled actions that have invocations before this time are retrieved.
--
-- 'targetActionType', 'describeScheduledActions_targetActionType' - The type of the scheduled actions to retrieve.
--
-- 'active', 'describeScheduledActions_active' - If true, retrieve only active scheduled actions. If false, retrieve only
-- disabled scheduled actions.
--
-- 'filters', 'describeScheduledActions_filters' - List of scheduled action filters.
--
-- 'scheduledActionName', 'describeScheduledActions_scheduledActionName' - The name of the scheduled action to retrieve.
--
-- 'marker', 'describeScheduledActions_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeScheduledActions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- 'maxRecords', 'describeScheduledActions_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeScheduledActions ::
  DescribeScheduledActions
newDescribeScheduledActions =
  DescribeScheduledActions'
    { startTime = Core.Nothing,
      endTime = Core.Nothing,
      targetActionType = Core.Nothing,
      active = Core.Nothing,
      filters = Core.Nothing,
      scheduledActionName = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The start time in UTC of the scheduled actions to retrieve. Only active
-- scheduled actions that have invocations after this time are retrieved.
describeScheduledActions_startTime :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.UTCTime)
describeScheduledActions_startTime = Lens.lens (\DescribeScheduledActions' {startTime} -> startTime) (\s@DescribeScheduledActions' {} a -> s {startTime = a} :: DescribeScheduledActions) Core.. Lens.mapping Core._Time

-- | The end time in UTC of the scheduled action to retrieve. Only active
-- scheduled actions that have invocations before this time are retrieved.
describeScheduledActions_endTime :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.UTCTime)
describeScheduledActions_endTime = Lens.lens (\DescribeScheduledActions' {endTime} -> endTime) (\s@DescribeScheduledActions' {} a -> s {endTime = a} :: DescribeScheduledActions) Core.. Lens.mapping Core._Time

-- | The type of the scheduled actions to retrieve.
describeScheduledActions_targetActionType :: Lens.Lens' DescribeScheduledActions (Core.Maybe ScheduledActionTypeValues)
describeScheduledActions_targetActionType = Lens.lens (\DescribeScheduledActions' {targetActionType} -> targetActionType) (\s@DescribeScheduledActions' {} a -> s {targetActionType = a} :: DescribeScheduledActions)

-- | If true, retrieve only active scheduled actions. If false, retrieve only
-- disabled scheduled actions.
describeScheduledActions_active :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.Bool)
describeScheduledActions_active = Lens.lens (\DescribeScheduledActions' {active} -> active) (\s@DescribeScheduledActions' {} a -> s {active = a} :: DescribeScheduledActions)

-- | List of scheduled action filters.
describeScheduledActions_filters :: Lens.Lens' DescribeScheduledActions (Core.Maybe [ScheduledActionFilter])
describeScheduledActions_filters = Lens.lens (\DescribeScheduledActions' {filters} -> filters) (\s@DescribeScheduledActions' {} a -> s {filters = a} :: DescribeScheduledActions) Core.. Lens.mapping Lens._Coerce

-- | The name of the scheduled action to retrieve.
describeScheduledActions_scheduledActionName :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.Text)
describeScheduledActions_scheduledActionName = Lens.lens (\DescribeScheduledActions' {scheduledActionName} -> scheduledActionName) (\s@DescribeScheduledActions' {} a -> s {scheduledActionName = a} :: DescribeScheduledActions)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeScheduledActions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeScheduledActions_marker :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.Text)
describeScheduledActions_marker = Lens.lens (\DescribeScheduledActions' {marker} -> marker) (\s@DescribeScheduledActions' {} a -> s {marker = a} :: DescribeScheduledActions)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeScheduledActions_maxRecords :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.Int)
describeScheduledActions_maxRecords = Lens.lens (\DescribeScheduledActions' {maxRecords} -> maxRecords) (\s@DescribeScheduledActions' {} a -> s {maxRecords = a} :: DescribeScheduledActions)

instance Core.AWSPager DescribeScheduledActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScheduledActionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScheduledActionsResponse_scheduledActions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeScheduledActions_marker
          Lens..~ rs
          Lens.^? describeScheduledActionsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeScheduledActions where
  type
    AWSResponse DescribeScheduledActions =
      DescribeScheduledActionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeScheduledActionsResult"
      ( \s h x ->
          DescribeScheduledActionsResponse'
            Core.<$> ( x Core..@? "ScheduledActions" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "ScheduledAction")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeScheduledActions

instance Core.NFData DescribeScheduledActions

instance Core.ToHeaders DescribeScheduledActions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeScheduledActions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeScheduledActions where
  toQuery DescribeScheduledActions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeScheduledActions" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "StartTime" Core.=: startTime,
        "EndTime" Core.=: endTime,
        "TargetActionType" Core.=: targetActionType,
        "Active" Core.=: active,
        "Filters"
          Core.=: Core.toQuery
            ( Core.toQueryList "ScheduledActionFilter"
                Core.<$> filters
            ),
        "ScheduledActionName" Core.=: scheduledActionName,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { -- | List of retrieved scheduled actions.
    scheduledActions :: Core.Maybe [ScheduledAction],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeScheduledActions
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeScheduledActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledActions', 'describeScheduledActionsResponse_scheduledActions' - List of retrieved scheduled actions.
--
-- 'marker', 'describeScheduledActionsResponse_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeScheduledActions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- 'httpStatus', 'describeScheduledActionsResponse_httpStatus' - The response's http status code.
newDescribeScheduledActionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeScheduledActionsResponse
newDescribeScheduledActionsResponse pHttpStatus_ =
  DescribeScheduledActionsResponse'
    { scheduledActions =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of retrieved scheduled actions.
describeScheduledActionsResponse_scheduledActions :: Lens.Lens' DescribeScheduledActionsResponse (Core.Maybe [ScheduledAction])
describeScheduledActionsResponse_scheduledActions = Lens.lens (\DescribeScheduledActionsResponse' {scheduledActions} -> scheduledActions) (\s@DescribeScheduledActionsResponse' {} a -> s {scheduledActions = a} :: DescribeScheduledActionsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeScheduledActions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeScheduledActionsResponse_marker :: Lens.Lens' DescribeScheduledActionsResponse (Core.Maybe Core.Text)
describeScheduledActionsResponse_marker = Lens.lens (\DescribeScheduledActionsResponse' {marker} -> marker) (\s@DescribeScheduledActionsResponse' {} a -> s {marker = a} :: DescribeScheduledActionsResponse)

-- | The response's http status code.
describeScheduledActionsResponse_httpStatus :: Lens.Lens' DescribeScheduledActionsResponse Core.Int
describeScheduledActionsResponse_httpStatus = Lens.lens (\DescribeScheduledActionsResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduledActionsResponse' {} a -> s {httpStatus = a} :: DescribeScheduledActionsResponse)

instance Core.NFData DescribeScheduledActionsResponse
