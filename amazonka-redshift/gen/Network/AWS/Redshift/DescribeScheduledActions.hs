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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { -- | The start time in UTC of the scheduled actions to retrieve. Only active
    -- scheduled actions that have invocations after this time are retrieved.
    startTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The end time in UTC of the scheduled action to retrieve. Only active
    -- scheduled actions that have invocations before this time are retrieved.
    endTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The type of the scheduled actions to retrieve.
    targetActionType :: Prelude.Maybe ScheduledActionTypeValues,
    -- | If true, retrieve only active scheduled actions. If false, retrieve only
    -- disabled scheduled actions.
    active :: Prelude.Maybe Prelude.Bool,
    -- | List of scheduled action filters.
    filters :: Prelude.Maybe [ScheduledActionFilter],
    -- | The name of the scheduled action to retrieve.
    scheduledActionName :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeScheduledActions
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
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
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { startTime =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      targetActionType = Prelude.Nothing,
      active = Prelude.Nothing,
      filters = Prelude.Nothing,
      scheduledActionName = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The start time in UTC of the scheduled actions to retrieve. Only active
-- scheduled actions that have invocations after this time are retrieved.
describeScheduledActions_startTime :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.UTCTime)
describeScheduledActions_startTime = Lens.lens (\DescribeScheduledActions' {startTime} -> startTime) (\s@DescribeScheduledActions' {} a -> s {startTime = a} :: DescribeScheduledActions) Prelude.. Lens.mapping Prelude._Time

-- | The end time in UTC of the scheduled action to retrieve. Only active
-- scheduled actions that have invocations before this time are retrieved.
describeScheduledActions_endTime :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.UTCTime)
describeScheduledActions_endTime = Lens.lens (\DescribeScheduledActions' {endTime} -> endTime) (\s@DescribeScheduledActions' {} a -> s {endTime = a} :: DescribeScheduledActions) Prelude.. Lens.mapping Prelude._Time

-- | The type of the scheduled actions to retrieve.
describeScheduledActions_targetActionType :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe ScheduledActionTypeValues)
describeScheduledActions_targetActionType = Lens.lens (\DescribeScheduledActions' {targetActionType} -> targetActionType) (\s@DescribeScheduledActions' {} a -> s {targetActionType = a} :: DescribeScheduledActions)

-- | If true, retrieve only active scheduled actions. If false, retrieve only
-- disabled scheduled actions.
describeScheduledActions_active :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.Bool)
describeScheduledActions_active = Lens.lens (\DescribeScheduledActions' {active} -> active) (\s@DescribeScheduledActions' {} a -> s {active = a} :: DescribeScheduledActions)

-- | List of scheduled action filters.
describeScheduledActions_filters :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe [ScheduledActionFilter])
describeScheduledActions_filters = Lens.lens (\DescribeScheduledActions' {filters} -> filters) (\s@DescribeScheduledActions' {} a -> s {filters = a} :: DescribeScheduledActions) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the scheduled action to retrieve.
describeScheduledActions_scheduledActionName :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.Text)
describeScheduledActions_scheduledActionName = Lens.lens (\DescribeScheduledActions' {scheduledActionName} -> scheduledActionName) (\s@DescribeScheduledActions' {} a -> s {scheduledActionName = a} :: DescribeScheduledActions)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeScheduledActions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
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

instance Pager.AWSPager DescribeScheduledActions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeScheduledActionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeScheduledActionsResponse_scheduledActions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeScheduledActions_marker
          Lens..~ rs
          Lens.^? describeScheduledActionsResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeScheduledActions where
  type
    Rs DescribeScheduledActions =
      DescribeScheduledActionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeScheduledActionsResult"
      ( \s h x ->
          DescribeScheduledActionsResponse'
            Prelude.<$> ( x Prelude..@? "ScheduledActions"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "ScheduledAction")
                        )
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeScheduledActions

instance Prelude.NFData DescribeScheduledActions

instance Prelude.ToHeaders DescribeScheduledActions where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeScheduledActions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeScheduledActions where
  toQuery DescribeScheduledActions' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeScheduledActions" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "StartTime" Prelude.=: startTime,
        "EndTime" Prelude.=: endTime,
        "TargetActionType" Prelude.=: targetActionType,
        "Active" Prelude.=: active,
        "Filters"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "ScheduledActionFilter"
                Prelude.<$> filters
            ),
        "ScheduledActionName" Prelude.=: scheduledActionName,
        "Marker" Prelude.=: marker,
        "MaxRecords" Prelude.=: maxRecords
      ]

-- | /See:/ 'newDescribeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { -- | List of retrieved scheduled actions.
    scheduledActions :: Prelude.Maybe [ScheduledAction],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeScheduledActions
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeScheduledActionsResponse_scheduledActions = Lens.lens (\DescribeScheduledActionsResponse' {scheduledActions} -> scheduledActions) (\s@DescribeScheduledActionsResponse' {} a -> s {scheduledActions = a} :: DescribeScheduledActionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeScheduledActions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeScheduledActionsResponse_marker :: Lens.Lens' DescribeScheduledActionsResponse (Prelude.Maybe Prelude.Text)
describeScheduledActionsResponse_marker = Lens.lens (\DescribeScheduledActionsResponse' {marker} -> marker) (\s@DescribeScheduledActionsResponse' {} a -> s {marker = a} :: DescribeScheduledActionsResponse)

-- | The response's http status code.
describeScheduledActionsResponse_httpStatus :: Lens.Lens' DescribeScheduledActionsResponse Prelude.Int
describeScheduledActionsResponse_httpStatus = Lens.lens (\DescribeScheduledActionsResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduledActionsResponse' {} a -> s {httpStatus = a} :: DescribeScheduledActionsResponse)

instance
  Prelude.NFData
    DescribeScheduledActionsResponse
