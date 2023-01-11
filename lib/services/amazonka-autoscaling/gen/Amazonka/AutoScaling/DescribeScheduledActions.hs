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
-- Module      : Amazonka.AutoScaling.DescribeScheduledActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the scheduled actions that haven\'t run or that
-- have not reached their end time.
--
-- To describe the scaling activities for scheduled actions that have
-- already run, call the DescribeScalingActivities API.
--
-- This operation returns paginated results.
module Amazonka.AutoScaling.DescribeScheduledActions
  ( -- * Creating a Request
    DescribeScheduledActions (..),
    newDescribeScheduledActions,

    -- * Request Lenses
    describeScheduledActions_autoScalingGroupName,
    describeScheduledActions_endTime,
    describeScheduledActions_maxRecords,
    describeScheduledActions_nextToken,
    describeScheduledActions_scheduledActionNames,
    describeScheduledActions_startTime,

    -- * Destructuring the Response
    DescribeScheduledActionsResponse (..),
    newDescribeScheduledActionsResponse,

    -- * Response Lenses
    describeScheduledActionsResponse_nextToken,
    describeScheduledActionsResponse_scheduledUpdateGroupActions,
    describeScheduledActionsResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The latest scheduled start time to return. If scheduled action names are
    -- provided, this property is ignored.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of one or more scheduled actions. If you omit this property,
    -- all scheduled actions are described. If you specify an unknown scheduled
    -- action, it is ignored with no error.
    --
    -- Array Members: Maximum number of 50 actions.
    scheduledActionNames :: Prelude.Maybe [Prelude.Text],
    -- | The earliest scheduled start time to return. If scheduled action names
    -- are provided, this property is ignored.
    startTime :: Prelude.Maybe Data.ISO8601
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
-- 'autoScalingGroupName', 'describeScheduledActions_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'endTime', 'describeScheduledActions_endTime' - The latest scheduled start time to return. If scheduled action names are
-- provided, this property is ignored.
--
-- 'maxRecords', 'describeScheduledActions_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
--
-- 'nextToken', 'describeScheduledActions_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'scheduledActionNames', 'describeScheduledActions_scheduledActionNames' - The names of one or more scheduled actions. If you omit this property,
-- all scheduled actions are described. If you specify an unknown scheduled
-- action, it is ignored with no error.
--
-- Array Members: Maximum number of 50 actions.
--
-- 'startTime', 'describeScheduledActions_startTime' - The earliest scheduled start time to return. If scheduled action names
-- are provided, this property is ignored.
newDescribeScheduledActions ::
  DescribeScheduledActions
newDescribeScheduledActions =
  DescribeScheduledActions'
    { autoScalingGroupName =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      scheduledActionNames = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The name of the Auto Scaling group.
describeScheduledActions_autoScalingGroupName :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.Text)
describeScheduledActions_autoScalingGroupName = Lens.lens (\DescribeScheduledActions' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeScheduledActions' {} a -> s {autoScalingGroupName = a} :: DescribeScheduledActions)

-- | The latest scheduled start time to return. If scheduled action names are
-- provided, this property is ignored.
describeScheduledActions_endTime :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.UTCTime)
describeScheduledActions_endTime = Lens.lens (\DescribeScheduledActions' {endTime} -> endTime) (\s@DescribeScheduledActions' {} a -> s {endTime = a} :: DescribeScheduledActions) Prelude.. Lens.mapping Data._Time

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeScheduledActions_maxRecords :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.Int)
describeScheduledActions_maxRecords = Lens.lens (\DescribeScheduledActions' {maxRecords} -> maxRecords) (\s@DescribeScheduledActions' {} a -> s {maxRecords = a} :: DescribeScheduledActions)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeScheduledActions_nextToken :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.Text)
describeScheduledActions_nextToken = Lens.lens (\DescribeScheduledActions' {nextToken} -> nextToken) (\s@DescribeScheduledActions' {} a -> s {nextToken = a} :: DescribeScheduledActions)

-- | The names of one or more scheduled actions. If you omit this property,
-- all scheduled actions are described. If you specify an unknown scheduled
-- action, it is ignored with no error.
--
-- Array Members: Maximum number of 50 actions.
describeScheduledActions_scheduledActionNames :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe [Prelude.Text])
describeScheduledActions_scheduledActionNames = Lens.lens (\DescribeScheduledActions' {scheduledActionNames} -> scheduledActionNames) (\s@DescribeScheduledActions' {} a -> s {scheduledActionNames = a} :: DescribeScheduledActions) Prelude.. Lens.mapping Lens.coerced

-- | The earliest scheduled start time to return. If scheduled action names
-- are provided, this property is ignored.
describeScheduledActions_startTime :: Lens.Lens' DescribeScheduledActions (Prelude.Maybe Prelude.UTCTime)
describeScheduledActions_startTime = Lens.lens (\DescribeScheduledActions' {startTime} -> startTime) (\s@DescribeScheduledActions' {} a -> s {startTime = a} :: DescribeScheduledActions) Prelude.. Lens.mapping Data._Time

instance Core.AWSPager DescribeScheduledActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScheduledActionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScheduledActionsResponse_scheduledUpdateGroupActions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeScheduledActions_nextToken
          Lens..~ rs
          Lens.^? describeScheduledActionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeScheduledActions where
  type
    AWSResponse DescribeScheduledActions =
      DescribeScheduledActionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeScheduledActionsResult"
      ( \s h x ->
          DescribeScheduledActionsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "ScheduledUpdateGroupActions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeScheduledActions where
  hashWithSalt _salt DescribeScheduledActions' {..} =
    _salt `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` scheduledActionNames
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData DescribeScheduledActions where
  rnf DescribeScheduledActions' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scheduledActionNames
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToHeaders DescribeScheduledActions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeScheduledActions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeScheduledActions where
  toQuery DescribeScheduledActions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeScheduledActions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "EndTime" Data.=: endTime,
        "MaxRecords" Data.=: maxRecords,
        "NextToken" Data.=: nextToken,
        "ScheduledActionNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> scheduledActionNames
            ),
        "StartTime" Data.=: startTime
      ]

-- | /See:/ 'newDescribeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The scheduled actions.
    scheduledUpdateGroupActions :: Prelude.Maybe [ScheduledUpdateGroupAction],
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
-- 'nextToken', 'describeScheduledActionsResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'scheduledUpdateGroupActions', 'describeScheduledActionsResponse_scheduledUpdateGroupActions' - The scheduled actions.
--
-- 'httpStatus', 'describeScheduledActionsResponse_httpStatus' - The response's http status code.
newDescribeScheduledActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeScheduledActionsResponse
newDescribeScheduledActionsResponse pHttpStatus_ =
  DescribeScheduledActionsResponse'
    { nextToken =
        Prelude.Nothing,
      scheduledUpdateGroupActions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeScheduledActionsResponse_nextToken :: Lens.Lens' DescribeScheduledActionsResponse (Prelude.Maybe Prelude.Text)
describeScheduledActionsResponse_nextToken = Lens.lens (\DescribeScheduledActionsResponse' {nextToken} -> nextToken) (\s@DescribeScheduledActionsResponse' {} a -> s {nextToken = a} :: DescribeScheduledActionsResponse)

-- | The scheduled actions.
describeScheduledActionsResponse_scheduledUpdateGroupActions :: Lens.Lens' DescribeScheduledActionsResponse (Prelude.Maybe [ScheduledUpdateGroupAction])
describeScheduledActionsResponse_scheduledUpdateGroupActions = Lens.lens (\DescribeScheduledActionsResponse' {scheduledUpdateGroupActions} -> scheduledUpdateGroupActions) (\s@DescribeScheduledActionsResponse' {} a -> s {scheduledUpdateGroupActions = a} :: DescribeScheduledActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeScheduledActionsResponse_httpStatus :: Lens.Lens' DescribeScheduledActionsResponse Prelude.Int
describeScheduledActionsResponse_httpStatus = Lens.lens (\DescribeScheduledActionsResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduledActionsResponse' {} a -> s {httpStatus = a} :: DescribeScheduledActionsResponse)

instance
  Prelude.NFData
    DescribeScheduledActionsResponse
  where
  rnf DescribeScheduledActionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scheduledUpdateGroupActions
      `Prelude.seq` Prelude.rnf httpStatus
