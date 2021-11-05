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
-- Module      : Amazonka.Redshift.DescribeScheduledActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes properties of scheduled actions.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeScheduledActions
  ( -- * Creating a Request
    DescribeScheduledActions (..),
    newDescribeScheduledActions,

    -- * Request Lenses
    describeScheduledActions_startTime,
    describeScheduledActions_scheduledActionName,
    describeScheduledActions_filters,
    describeScheduledActions_active,
    describeScheduledActions_targetActionType,
    describeScheduledActions_marker,
    describeScheduledActions_maxRecords,
    describeScheduledActions_endTime,

    -- * Destructuring the Response
    DescribeScheduledActionsResponse (..),
    newDescribeScheduledActionsResponse,

    -- * Response Lenses
    describeScheduledActionsResponse_scheduledActions,
    describeScheduledActionsResponse_marker,
    describeScheduledActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { -- | The start time in UTC of the scheduled actions to retrieve. Only active
    -- scheduled actions that have invocations after this time are retrieved.
    startTime :: Prelude.Maybe Core.ISO8601,
    -- | The name of the scheduled action to retrieve.
    scheduledActionName :: Prelude.Maybe Prelude.Text,
    -- | List of scheduled action filters.
    filters :: Prelude.Maybe [ScheduledActionFilter],
    -- | If true, retrieve only active scheduled actions. If false, retrieve only
    -- disabled scheduled actions.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The type of the scheduled actions to retrieve.
    targetActionType :: Prelude.Maybe ScheduledActionTypeValues,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeScheduledActions
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The end time in UTC of the scheduled action to retrieve. Only active
    -- scheduled actions that have invocations before this time are retrieved.
    endTime :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'scheduledActionName', 'describeScheduledActions_scheduledActionName' - The name of the scheduled action to retrieve.
--
-- 'filters', 'describeScheduledActions_filters' - List of scheduled action filters.
--
-- 'active', 'describeScheduledActions_active' - If true, retrieve only active scheduled actions. If false, retrieve only
-- disabled scheduled actions.
--
-- 'targetActionType', 'describeScheduledActions_targetActionType' - The type of the scheduled actions to retrieve.
--
-- 'marker', 'describeScheduledActions_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeScheduledActions
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
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
--
-- 'endTime', 'describeScheduledActions_endTime' - The end time in UTC of the scheduled action to retrieve. Only active
-- scheduled actions that have invocations before this time are retrieved.
newDescribeScheduledActions ::
  DescribeScheduledActions
newDescribeScheduledActions =
  DescribeScheduledActions'
    { startTime =
        Prelude.Nothing,
      scheduledActionName = Prelude.Nothing,
      filters = Prelude.Nothing,
      active = Prelude.Nothing,
      targetActionType = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      endTime = Prelude.Nothing
    }

-- | The start time in UTC of the scheduled actions to retrieve. Only active
-- scheduled actions that have invocations after this time are retrieved.
describeScheduledActions_startTime :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.UTCTime)
describeScheduledActions_startTime = Lens.lens (\DescribeScheduledActions' {startTime} -> startTime) (\s@DescribeScheduledActions' {} a -> s {startTime = a} :: DescribeScheduledActions) Prelude.. Lens.mapping Core._Time

-- | The name of the scheduled action to retrieve.
describeScheduledActions_scheduledActionName :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.Text)
describeScheduledActions_scheduledActionName = Lens.lens (\DescribeScheduledActions' {scheduledActionName} -> scheduledActionName) (\s@DescribeScheduledActions' {} a -> s {scheduledActionName = a} :: DescribeScheduledActions)

-- | List of scheduled action filters.
describeScheduledActions_filters :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe [ScheduledActionFilter])
describeScheduledActions_filters = Lens.lens (\DescribeScheduledActions' {filters} -> filters) (\s@DescribeScheduledActions' {} a -> s {filters = a} :: DescribeScheduledActions) Prelude.. Lens.mapping Lens.coerced

-- | If true, retrieve only active scheduled actions. If false, retrieve only
-- disabled scheduled actions.
describeScheduledActions_active :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.Bool)
describeScheduledActions_active = Lens.lens (\DescribeScheduledActions' {active} -> active) (\s@DescribeScheduledActions' {} a -> s {active = a} :: DescribeScheduledActions)

-- | The type of the scheduled actions to retrieve.
describeScheduledActions_targetActionType :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe ScheduledActionTypeValues)
describeScheduledActions_targetActionType = Lens.lens (\DescribeScheduledActions' {targetActionType} -> targetActionType) (\s@DescribeScheduledActions' {} a -> s {targetActionType = a} :: DescribeScheduledActions)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeScheduledActions
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeScheduledActions_marker :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.Text)
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
describeScheduledActions_maxRecords :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.Int)
describeScheduledActions_maxRecords = Lens.lens (\DescribeScheduledActions' {maxRecords} -> maxRecords) (\s@DescribeScheduledActions' {} a -> s {maxRecords = a} :: DescribeScheduledActions)

-- | The end time in UTC of the scheduled action to retrieve. Only active
-- scheduled actions that have invocations before this time are retrieved.
describeScheduledActions_endTime :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.UTCTime)
describeScheduledActions_endTime = Lens.lens (\DescribeScheduledActions' {endTime} -> endTime) (\s@DescribeScheduledActions' {} a -> s {endTime = a} :: DescribeScheduledActions) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager DescribeScheduledActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScheduledActionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScheduledActionsResponse_scheduledActions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeScheduledActions_marker
          Lens..~ rs
          Lens.^? describeScheduledActionsResponse_marker
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..@? "ScheduledActions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "ScheduledAction")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeScheduledActions

instance Prelude.NFData DescribeScheduledActions

instance Core.ToHeaders DescribeScheduledActions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeScheduledActions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeScheduledActions where
  toQuery DescribeScheduledActions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeScheduledActions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "StartTime" Core.=: startTime,
        "ScheduledActionName" Core.=: scheduledActionName,
        "Filters"
          Core.=: Core.toQuery
            ( Core.toQueryList "ScheduledActionFilter"
                Prelude.<$> filters
            ),
        "Active" Core.=: active,
        "TargetActionType" Core.=: targetActionType,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "EndTime" Core.=: endTime
      ]

-- | /See:/ 'newDescribeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { -- | List of retrieved scheduled actions.
    scheduledActions :: Prelude.Maybe [ScheduledAction],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeScheduledActions
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'httpStatus', 'describeScheduledActionsResponse_httpStatus' - The response's http status code.
newDescribeScheduledActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeScheduledActionsResponse
newDescribeScheduledActionsResponse pHttpStatus_ =
  DescribeScheduledActionsResponse'
    { scheduledActions =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of retrieved scheduled actions.
describeScheduledActionsResponse_scheduledActions :: Lens.Lens' DescribeScheduledActionsResponse (Prelude.Maybe [ScheduledAction])
describeScheduledActionsResponse_scheduledActions = Lens.lens (\DescribeScheduledActionsResponse' {scheduledActions} -> scheduledActions) (\s@DescribeScheduledActionsResponse' {} a -> s {scheduledActions = a} :: DescribeScheduledActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeScheduledActions
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeScheduledActionsResponse_marker :: Lens.Lens' DescribeScheduledActionsResponse (Prelude.Maybe Prelude.Text)
describeScheduledActionsResponse_marker = Lens.lens (\DescribeScheduledActionsResponse' {marker} -> marker) (\s@DescribeScheduledActionsResponse' {} a -> s {marker = a} :: DescribeScheduledActionsResponse)

-- | The response's http status code.
describeScheduledActionsResponse_httpStatus :: Lens.Lens' DescribeScheduledActionsResponse Prelude.Int
describeScheduledActionsResponse_httpStatus = Lens.lens (\DescribeScheduledActionsResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduledActionsResponse' {} a -> s {httpStatus = a} :: DescribeScheduledActionsResponse)

instance
  Prelude.NFData
    DescribeScheduledActionsResponse
