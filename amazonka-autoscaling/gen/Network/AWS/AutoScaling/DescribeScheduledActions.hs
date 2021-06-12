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
-- Module      : Network.AWS.AutoScaling.DescribeScheduledActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the actions scheduled for your Auto Scaling group that
-- haven\'t run or that have not reached their end time. To describe the
-- actions that have already run, call the DescribeScalingActivities API.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeScheduledActions
  ( -- * Creating a Request
    DescribeScheduledActions (..),
    newDescribeScheduledActions,

    -- * Request Lenses
    describeScheduledActions_nextToken,
    describeScheduledActions_startTime,
    describeScheduledActions_endTime,
    describeScheduledActions_scheduledActionNames,
    describeScheduledActions_autoScalingGroupName,
    describeScheduledActions_maxRecords,

    -- * Destructuring the Response
    DescribeScheduledActionsResponse (..),
    newDescribeScheduledActionsResponse,

    -- * Response Lenses
    describeScheduledActionsResponse_nextToken,
    describeScheduledActionsResponse_scheduledUpdateGroupActions,
    describeScheduledActionsResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The earliest scheduled start time to return. If scheduled action names
    -- are provided, this parameter is ignored.
    startTime :: Core.Maybe Core.ISO8601,
    -- | The latest scheduled start time to return. If scheduled action names are
    -- provided, this parameter is ignored.
    endTime :: Core.Maybe Core.ISO8601,
    -- | The names of one or more scheduled actions. You can specify up to 50
    -- actions. If you omit this parameter, all scheduled actions are
    -- described. If you specify an unknown scheduled action, it is ignored
    -- with no error.
    scheduledActionNames :: Core.Maybe [Core.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Maybe Core.Text,
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
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
-- 'nextToken', 'describeScheduledActions_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'startTime', 'describeScheduledActions_startTime' - The earliest scheduled start time to return. If scheduled action names
-- are provided, this parameter is ignored.
--
-- 'endTime', 'describeScheduledActions_endTime' - The latest scheduled start time to return. If scheduled action names are
-- provided, this parameter is ignored.
--
-- 'scheduledActionNames', 'describeScheduledActions_scheduledActionNames' - The names of one or more scheduled actions. You can specify up to 50
-- actions. If you omit this parameter, all scheduled actions are
-- described. If you specify an unknown scheduled action, it is ignored
-- with no error.
--
-- 'autoScalingGroupName', 'describeScheduledActions_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'maxRecords', 'describeScheduledActions_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
newDescribeScheduledActions ::
  DescribeScheduledActions
newDescribeScheduledActions =
  DescribeScheduledActions'
    { nextToken = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      scheduledActionNames = Core.Nothing,
      autoScalingGroupName = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeScheduledActions_nextToken :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.Text)
describeScheduledActions_nextToken = Lens.lens (\DescribeScheduledActions' {nextToken} -> nextToken) (\s@DescribeScheduledActions' {} a -> s {nextToken = a} :: DescribeScheduledActions)

-- | The earliest scheduled start time to return. If scheduled action names
-- are provided, this parameter is ignored.
describeScheduledActions_startTime :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.UTCTime)
describeScheduledActions_startTime = Lens.lens (\DescribeScheduledActions' {startTime} -> startTime) (\s@DescribeScheduledActions' {} a -> s {startTime = a} :: DescribeScheduledActions) Core.. Lens.mapping Core._Time

-- | The latest scheduled start time to return. If scheduled action names are
-- provided, this parameter is ignored.
describeScheduledActions_endTime :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.UTCTime)
describeScheduledActions_endTime = Lens.lens (\DescribeScheduledActions' {endTime} -> endTime) (\s@DescribeScheduledActions' {} a -> s {endTime = a} :: DescribeScheduledActions) Core.. Lens.mapping Core._Time

-- | The names of one or more scheduled actions. You can specify up to 50
-- actions. If you omit this parameter, all scheduled actions are
-- described. If you specify an unknown scheduled action, it is ignored
-- with no error.
describeScheduledActions_scheduledActionNames :: Lens.Lens' DescribeScheduledActions (Core.Maybe [Core.Text])
describeScheduledActions_scheduledActionNames = Lens.lens (\DescribeScheduledActions' {scheduledActionNames} -> scheduledActionNames) (\s@DescribeScheduledActions' {} a -> s {scheduledActionNames = a} :: DescribeScheduledActions) Core.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
describeScheduledActions_autoScalingGroupName :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.Text)
describeScheduledActions_autoScalingGroupName = Lens.lens (\DescribeScheduledActions' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeScheduledActions' {} a -> s {autoScalingGroupName = a} :: DescribeScheduledActions)

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeScheduledActions_maxRecords :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.Int)
describeScheduledActions_maxRecords = Lens.lens (\DescribeScheduledActions' {maxRecords} -> maxRecords) (\s@DescribeScheduledActions' {} a -> s {maxRecords = a} :: DescribeScheduledActions)

instance Core.AWSPager DescribeScheduledActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScheduledActionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScheduledActionsResponse_scheduledUpdateGroupActions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeScheduledActions_nextToken
          Lens..~ rs
          Lens.^? describeScheduledActionsResponse_nextToken
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
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "ScheduledUpdateGroupActions"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
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
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "StartTime" Core.=: startTime,
        "EndTime" Core.=: endTime,
        "ScheduledActionNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> scheduledActionNames
            ),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The scheduled actions.
    scheduledUpdateGroupActions :: Core.Maybe [ScheduledUpdateGroupAction],
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
  Core.Int ->
  DescribeScheduledActionsResponse
newDescribeScheduledActionsResponse pHttpStatus_ =
  DescribeScheduledActionsResponse'
    { nextToken =
        Core.Nothing,
      scheduledUpdateGroupActions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeScheduledActionsResponse_nextToken :: Lens.Lens' DescribeScheduledActionsResponse (Core.Maybe Core.Text)
describeScheduledActionsResponse_nextToken = Lens.lens (\DescribeScheduledActionsResponse' {nextToken} -> nextToken) (\s@DescribeScheduledActionsResponse' {} a -> s {nextToken = a} :: DescribeScheduledActionsResponse)

-- | The scheduled actions.
describeScheduledActionsResponse_scheduledUpdateGroupActions :: Lens.Lens' DescribeScheduledActionsResponse (Core.Maybe [ScheduledUpdateGroupAction])
describeScheduledActionsResponse_scheduledUpdateGroupActions = Lens.lens (\DescribeScheduledActionsResponse' {scheduledUpdateGroupActions} -> scheduledUpdateGroupActions) (\s@DescribeScheduledActionsResponse' {} a -> s {scheduledUpdateGroupActions = a} :: DescribeScheduledActionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeScheduledActionsResponse_httpStatus :: Lens.Lens' DescribeScheduledActionsResponse Core.Int
describeScheduledActionsResponse_httpStatus = Lens.lens (\DescribeScheduledActionsResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduledActionsResponse' {} a -> s {httpStatus = a} :: DescribeScheduledActionsResponse)

instance Core.NFData DescribeScheduledActionsResponse
