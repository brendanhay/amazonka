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
-- Module      : Network.AWS.CloudWatch.DescribeAlarms
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified alarms. You can filter the results by specifying
-- a prefix for the alarm name, the alarm state, or a prefix for any
-- action.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.DescribeAlarms
  ( -- * Creating a Request
    DescribeAlarms (..),
    newDescribeAlarms,

    -- * Request Lenses
    describeAlarms_nextToken,
    describeAlarms_alarmTypes,
    describeAlarms_alarmNames,
    describeAlarms_alarmNamePrefix,
    describeAlarms_stateValue,
    describeAlarms_actionPrefix,
    describeAlarms_childrenOfAlarmName,
    describeAlarms_parentsOfAlarmName,
    describeAlarms_maxRecords,

    -- * Destructuring the Response
    DescribeAlarmsResponse (..),
    newDescribeAlarmsResponse,

    -- * Response Lenses
    describeAlarmsResponse_metricAlarms,
    describeAlarmsResponse_nextToken,
    describeAlarmsResponse_compositeAlarms,
    describeAlarmsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAlarms' smart constructor.
data DescribeAlarms = DescribeAlarms'
  { -- | The token returned by a previous call to indicate that there is more
    -- data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to specify whether you want the operation to return
    -- metric alarms or composite alarms. If you omit this parameter, only
    -- metric alarms are returned.
    alarmTypes :: Prelude.Maybe [AlarmType],
    -- | The names of the alarms to retrieve information about.
    alarmNames :: Prelude.Maybe [Prelude.Text],
    -- | An alarm name prefix. If you specify this parameter, you receive
    -- information about all alarms that have names that start with this
    -- prefix.
    --
    -- If this parameter is specified, you cannot specify @AlarmNames@.
    alarmNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | Specify this parameter to receive information only about alarms that are
    -- currently in the state that you specify.
    stateValue :: Prelude.Maybe StateValue,
    -- | Use this parameter to filter the results of the operation to only those
    -- alarms that use a certain alarm action. For example, you could specify
    -- the ARN of an SNS topic to find all alarms that send notifications to
    -- that topic.
    actionPrefix :: Prelude.Maybe Prelude.Text,
    -- | If you use this parameter and specify the name of a composite alarm, the
    -- operation returns information about the \"children\" alarms of the alarm
    -- you specify. These are the metric alarms and composite alarms referenced
    -- in the @AlarmRule@ field of the composite alarm that you specify in
    -- @ChildrenOfAlarmName@. Information about the composite alarm that you
    -- name in @ChildrenOfAlarmName@ is not returned.
    --
    -- If you specify @ChildrenOfAlarmName@, you cannot specify any other
    -- parameters in the request except for @MaxRecords@ and @NextToken@. If
    -- you do so, you receive a validation error.
    --
    -- Only the @Alarm Name@, @ARN@, @StateValue@
    -- (OK\/ALARM\/INSUFFICIENT_DATA), and @StateUpdatedTimestamp@ information
    -- are returned by this operation when you use this parameter. To get
    -- complete information about these alarms, perform another
    -- @DescribeAlarms@ operation and specify the parent alarm names in the
    -- @AlarmNames@ parameter.
    childrenOfAlarmName :: Prelude.Maybe Prelude.Text,
    -- | If you use this parameter and specify the name of a metric or composite
    -- alarm, the operation returns information about the \"parent\" alarms of
    -- the alarm you specify. These are the composite alarms that have
    -- @AlarmRule@ parameters that reference the alarm named in
    -- @ParentsOfAlarmName@. Information about the alarm that you specify in
    -- @ParentsOfAlarmName@ is not returned.
    --
    -- If you specify @ParentsOfAlarmName@, you cannot specify any other
    -- parameters in the request except for @MaxRecords@ and @NextToken@. If
    -- you do so, you receive a validation error.
    --
    -- Only the Alarm Name and ARN are returned by this operation when you use
    -- this parameter. To get complete information about these alarms, perform
    -- another @DescribeAlarms@ operation and specify the parent alarm names in
    -- the @AlarmNames@ parameter.
    parentsOfAlarmName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of alarm descriptions to retrieve.
    maxRecords :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAlarms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAlarms_nextToken' - The token returned by a previous call to indicate that there is more
-- data available.
--
-- 'alarmTypes', 'describeAlarms_alarmTypes' - Use this parameter to specify whether you want the operation to return
-- metric alarms or composite alarms. If you omit this parameter, only
-- metric alarms are returned.
--
-- 'alarmNames', 'describeAlarms_alarmNames' - The names of the alarms to retrieve information about.
--
-- 'alarmNamePrefix', 'describeAlarms_alarmNamePrefix' - An alarm name prefix. If you specify this parameter, you receive
-- information about all alarms that have names that start with this
-- prefix.
--
-- If this parameter is specified, you cannot specify @AlarmNames@.
--
-- 'stateValue', 'describeAlarms_stateValue' - Specify this parameter to receive information only about alarms that are
-- currently in the state that you specify.
--
-- 'actionPrefix', 'describeAlarms_actionPrefix' - Use this parameter to filter the results of the operation to only those
-- alarms that use a certain alarm action. For example, you could specify
-- the ARN of an SNS topic to find all alarms that send notifications to
-- that topic.
--
-- 'childrenOfAlarmName', 'describeAlarms_childrenOfAlarmName' - If you use this parameter and specify the name of a composite alarm, the
-- operation returns information about the \"children\" alarms of the alarm
-- you specify. These are the metric alarms and composite alarms referenced
-- in the @AlarmRule@ field of the composite alarm that you specify in
-- @ChildrenOfAlarmName@. Information about the composite alarm that you
-- name in @ChildrenOfAlarmName@ is not returned.
--
-- If you specify @ChildrenOfAlarmName@, you cannot specify any other
-- parameters in the request except for @MaxRecords@ and @NextToken@. If
-- you do so, you receive a validation error.
--
-- Only the @Alarm Name@, @ARN@, @StateValue@
-- (OK\/ALARM\/INSUFFICIENT_DATA), and @StateUpdatedTimestamp@ information
-- are returned by this operation when you use this parameter. To get
-- complete information about these alarms, perform another
-- @DescribeAlarms@ operation and specify the parent alarm names in the
-- @AlarmNames@ parameter.
--
-- 'parentsOfAlarmName', 'describeAlarms_parentsOfAlarmName' - If you use this parameter and specify the name of a metric or composite
-- alarm, the operation returns information about the \"parent\" alarms of
-- the alarm you specify. These are the composite alarms that have
-- @AlarmRule@ parameters that reference the alarm named in
-- @ParentsOfAlarmName@. Information about the alarm that you specify in
-- @ParentsOfAlarmName@ is not returned.
--
-- If you specify @ParentsOfAlarmName@, you cannot specify any other
-- parameters in the request except for @MaxRecords@ and @NextToken@. If
-- you do so, you receive a validation error.
--
-- Only the Alarm Name and ARN are returned by this operation when you use
-- this parameter. To get complete information about these alarms, perform
-- another @DescribeAlarms@ operation and specify the parent alarm names in
-- the @AlarmNames@ parameter.
--
-- 'maxRecords', 'describeAlarms_maxRecords' - The maximum number of alarm descriptions to retrieve.
newDescribeAlarms ::
  DescribeAlarms
newDescribeAlarms =
  DescribeAlarms'
    { nextToken = Prelude.Nothing,
      alarmTypes = Prelude.Nothing,
      alarmNames = Prelude.Nothing,
      alarmNamePrefix = Prelude.Nothing,
      stateValue = Prelude.Nothing,
      actionPrefix = Prelude.Nothing,
      childrenOfAlarmName = Prelude.Nothing,
      parentsOfAlarmName = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The token returned by a previous call to indicate that there is more
-- data available.
describeAlarms_nextToken :: Lens.Lens' DescribeAlarms (Prelude.Maybe Prelude.Text)
describeAlarms_nextToken = Lens.lens (\DescribeAlarms' {nextToken} -> nextToken) (\s@DescribeAlarms' {} a -> s {nextToken = a} :: DescribeAlarms)

-- | Use this parameter to specify whether you want the operation to return
-- metric alarms or composite alarms. If you omit this parameter, only
-- metric alarms are returned.
describeAlarms_alarmTypes :: Lens.Lens' DescribeAlarms (Prelude.Maybe [AlarmType])
describeAlarms_alarmTypes = Lens.lens (\DescribeAlarms' {alarmTypes} -> alarmTypes) (\s@DescribeAlarms' {} a -> s {alarmTypes = a} :: DescribeAlarms) Prelude.. Lens.mapping Lens._Coerce

-- | The names of the alarms to retrieve information about.
describeAlarms_alarmNames :: Lens.Lens' DescribeAlarms (Prelude.Maybe [Prelude.Text])
describeAlarms_alarmNames = Lens.lens (\DescribeAlarms' {alarmNames} -> alarmNames) (\s@DescribeAlarms' {} a -> s {alarmNames = a} :: DescribeAlarms) Prelude.. Lens.mapping Lens._Coerce

-- | An alarm name prefix. If you specify this parameter, you receive
-- information about all alarms that have names that start with this
-- prefix.
--
-- If this parameter is specified, you cannot specify @AlarmNames@.
describeAlarms_alarmNamePrefix :: Lens.Lens' DescribeAlarms (Prelude.Maybe Prelude.Text)
describeAlarms_alarmNamePrefix = Lens.lens (\DescribeAlarms' {alarmNamePrefix} -> alarmNamePrefix) (\s@DescribeAlarms' {} a -> s {alarmNamePrefix = a} :: DescribeAlarms)

-- | Specify this parameter to receive information only about alarms that are
-- currently in the state that you specify.
describeAlarms_stateValue :: Lens.Lens' DescribeAlarms (Prelude.Maybe StateValue)
describeAlarms_stateValue = Lens.lens (\DescribeAlarms' {stateValue} -> stateValue) (\s@DescribeAlarms' {} a -> s {stateValue = a} :: DescribeAlarms)

-- | Use this parameter to filter the results of the operation to only those
-- alarms that use a certain alarm action. For example, you could specify
-- the ARN of an SNS topic to find all alarms that send notifications to
-- that topic.
describeAlarms_actionPrefix :: Lens.Lens' DescribeAlarms (Prelude.Maybe Prelude.Text)
describeAlarms_actionPrefix = Lens.lens (\DescribeAlarms' {actionPrefix} -> actionPrefix) (\s@DescribeAlarms' {} a -> s {actionPrefix = a} :: DescribeAlarms)

-- | If you use this parameter and specify the name of a composite alarm, the
-- operation returns information about the \"children\" alarms of the alarm
-- you specify. These are the metric alarms and composite alarms referenced
-- in the @AlarmRule@ field of the composite alarm that you specify in
-- @ChildrenOfAlarmName@. Information about the composite alarm that you
-- name in @ChildrenOfAlarmName@ is not returned.
--
-- If you specify @ChildrenOfAlarmName@, you cannot specify any other
-- parameters in the request except for @MaxRecords@ and @NextToken@. If
-- you do so, you receive a validation error.
--
-- Only the @Alarm Name@, @ARN@, @StateValue@
-- (OK\/ALARM\/INSUFFICIENT_DATA), and @StateUpdatedTimestamp@ information
-- are returned by this operation when you use this parameter. To get
-- complete information about these alarms, perform another
-- @DescribeAlarms@ operation and specify the parent alarm names in the
-- @AlarmNames@ parameter.
describeAlarms_childrenOfAlarmName :: Lens.Lens' DescribeAlarms (Prelude.Maybe Prelude.Text)
describeAlarms_childrenOfAlarmName = Lens.lens (\DescribeAlarms' {childrenOfAlarmName} -> childrenOfAlarmName) (\s@DescribeAlarms' {} a -> s {childrenOfAlarmName = a} :: DescribeAlarms)

-- | If you use this parameter and specify the name of a metric or composite
-- alarm, the operation returns information about the \"parent\" alarms of
-- the alarm you specify. These are the composite alarms that have
-- @AlarmRule@ parameters that reference the alarm named in
-- @ParentsOfAlarmName@. Information about the alarm that you specify in
-- @ParentsOfAlarmName@ is not returned.
--
-- If you specify @ParentsOfAlarmName@, you cannot specify any other
-- parameters in the request except for @MaxRecords@ and @NextToken@. If
-- you do so, you receive a validation error.
--
-- Only the Alarm Name and ARN are returned by this operation when you use
-- this parameter. To get complete information about these alarms, perform
-- another @DescribeAlarms@ operation and specify the parent alarm names in
-- the @AlarmNames@ parameter.
describeAlarms_parentsOfAlarmName :: Lens.Lens' DescribeAlarms (Prelude.Maybe Prelude.Text)
describeAlarms_parentsOfAlarmName = Lens.lens (\DescribeAlarms' {parentsOfAlarmName} -> parentsOfAlarmName) (\s@DescribeAlarms' {} a -> s {parentsOfAlarmName = a} :: DescribeAlarms)

-- | The maximum number of alarm descriptions to retrieve.
describeAlarms_maxRecords :: Lens.Lens' DescribeAlarms (Prelude.Maybe Prelude.Natural)
describeAlarms_maxRecords = Lens.lens (\DescribeAlarms' {maxRecords} -> maxRecords) (\s@DescribeAlarms' {} a -> s {maxRecords = a} :: DescribeAlarms)

instance Core.AWSPager DescribeAlarms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAlarmsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAlarmsResponse_metricAlarms
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAlarmsResponse_compositeAlarms
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAlarms_nextToken
          Lens..~ rs
          Lens.^? describeAlarmsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeAlarms where
  type
    AWSResponse DescribeAlarms =
      DescribeAlarmsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAlarmsResult"
      ( \s h x ->
          DescribeAlarmsResponse'
            Prelude.<$> ( x Core..@? "MetricAlarms" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "CompositeAlarms" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAlarms

instance Prelude.NFData DescribeAlarms

instance Core.ToHeaders DescribeAlarms where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAlarms where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAlarms where
  toQuery DescribeAlarms' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeAlarms" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-08-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "AlarmTypes"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> alarmTypes),
        "AlarmNames"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> alarmNames),
        "AlarmNamePrefix" Core.=: alarmNamePrefix,
        "StateValue" Core.=: stateValue,
        "ActionPrefix" Core.=: actionPrefix,
        "ChildrenOfAlarmName" Core.=: childrenOfAlarmName,
        "ParentsOfAlarmName" Core.=: parentsOfAlarmName,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeAlarmsResponse' smart constructor.
data DescribeAlarmsResponse = DescribeAlarmsResponse'
  { -- | The information about any metric alarms returned by the operation.
    metricAlarms :: Prelude.Maybe [MetricAlarm],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The information about any composite alarms returned by the operation.
    compositeAlarms :: Prelude.Maybe [CompositeAlarm],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAlarmsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricAlarms', 'describeAlarmsResponse_metricAlarms' - The information about any metric alarms returned by the operation.
--
-- 'nextToken', 'describeAlarmsResponse_nextToken' - The token that marks the start of the next batch of returned results.
--
-- 'compositeAlarms', 'describeAlarmsResponse_compositeAlarms' - The information about any composite alarms returned by the operation.
--
-- 'httpStatus', 'describeAlarmsResponse_httpStatus' - The response's http status code.
newDescribeAlarmsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAlarmsResponse
newDescribeAlarmsResponse pHttpStatus_ =
  DescribeAlarmsResponse'
    { metricAlarms =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      compositeAlarms = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The information about any metric alarms returned by the operation.
describeAlarmsResponse_metricAlarms :: Lens.Lens' DescribeAlarmsResponse (Prelude.Maybe [MetricAlarm])
describeAlarmsResponse_metricAlarms = Lens.lens (\DescribeAlarmsResponse' {metricAlarms} -> metricAlarms) (\s@DescribeAlarmsResponse' {} a -> s {metricAlarms = a} :: DescribeAlarmsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The token that marks the start of the next batch of returned results.
describeAlarmsResponse_nextToken :: Lens.Lens' DescribeAlarmsResponse (Prelude.Maybe Prelude.Text)
describeAlarmsResponse_nextToken = Lens.lens (\DescribeAlarmsResponse' {nextToken} -> nextToken) (\s@DescribeAlarmsResponse' {} a -> s {nextToken = a} :: DescribeAlarmsResponse)

-- | The information about any composite alarms returned by the operation.
describeAlarmsResponse_compositeAlarms :: Lens.Lens' DescribeAlarmsResponse (Prelude.Maybe [CompositeAlarm])
describeAlarmsResponse_compositeAlarms = Lens.lens (\DescribeAlarmsResponse' {compositeAlarms} -> compositeAlarms) (\s@DescribeAlarmsResponse' {} a -> s {compositeAlarms = a} :: DescribeAlarmsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAlarmsResponse_httpStatus :: Lens.Lens' DescribeAlarmsResponse Prelude.Int
describeAlarmsResponse_httpStatus = Lens.lens (\DescribeAlarmsResponse' {httpStatus} -> httpStatus) (\s@DescribeAlarmsResponse' {} a -> s {httpStatus = a} :: DescribeAlarmsResponse)

instance Prelude.NFData DescribeAlarmsResponse
