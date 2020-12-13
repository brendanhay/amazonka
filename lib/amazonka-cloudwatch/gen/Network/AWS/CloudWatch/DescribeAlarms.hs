{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAlarms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified alarms. You can filter the results by specifying a a prefix for the alarm name, the alarm state, or a prefix for any action.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.DescribeAlarms
  ( -- * Creating a request
    DescribeAlarms (..),
    mkDescribeAlarms,

    -- ** Request lenses
    dAlarmNamePrefix,
    dAlarmTypes,
    dActionPrefix,
    dNextToken,
    dStateValue,
    dAlarmNames,
    dMaxRecords,
    dParentsOfAlarmName,
    dChildrenOfAlarmName,

    -- * Destructuring the response
    DescribeAlarmsResponse (..),
    mkDescribeAlarmsResponse,

    -- ** Response lenses
    darsMetricAlarms,
    darsCompositeAlarms,
    darsNextToken,
    darsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAlarms' smart constructor.
data DescribeAlarms = DescribeAlarms'
  { -- | An alarm name prefix. If you specify this parameter, you receive information about all alarms that have names that start with this prefix.
    --
    -- If this parameter is specified, you cannot specify @AlarmNames@ .
    alarmNamePrefix :: Lude.Maybe Lude.Text,
    -- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
    alarmTypes :: Lude.Maybe [AlarmType],
    -- | Use this parameter to filter the results of the operation to only those alarms that use a certain alarm action. For example, you could specify the ARN of an SNS topic to find all alarms that send notifications to that topic.
    actionPrefix :: Lude.Maybe Lude.Text,
    -- | The token returned by a previous call to indicate that there is more data available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specify this parameter to receive information only about alarms that are currently in the state that you specify.
    stateValue :: Lude.Maybe StateValue,
    -- | The names of the alarms to retrieve information about.
    alarmNames :: Lude.Maybe [Lude.Text],
    -- | The maximum number of alarm descriptions to retrieve.
    maxRecords :: Lude.Maybe Lude.Natural,
    -- | If you use this parameter and specify the name of a metric or composite alarm, the operation returns information about the "parent" alarms of the alarm you specify. These are the composite alarms that have @AlarmRule@ parameters that reference the alarm named in @ParentsOfAlarmName@ . Information about the alarm that you specify in @ParentsOfAlarmName@ is not returned.
    --
    -- If you specify @ParentsOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
    parentsOfAlarmName :: Lude.Maybe Lude.Text,
    -- | If you use this parameter and specify the name of a composite alarm, the operation returns information about the "children" alarms of the alarm you specify. These are the metric alarms and composite alarms referenced in the @AlarmRule@ field of the composite alarm that you specify in @ChildrenOfAlarmName@ . Information about the composite alarm that you name in @ChildrenOfAlarmName@ is not returned.
    --
    -- If you specify @ChildrenOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
    childrenOfAlarmName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlarms' with the minimum fields required to make a request.
--
-- * 'alarmNamePrefix' - An alarm name prefix. If you specify this parameter, you receive information about all alarms that have names that start with this prefix.
--
-- If this parameter is specified, you cannot specify @AlarmNames@ .
-- * 'alarmTypes' - Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
-- * 'actionPrefix' - Use this parameter to filter the results of the operation to only those alarms that use a certain alarm action. For example, you could specify the ARN of an SNS topic to find all alarms that send notifications to that topic.
-- * 'nextToken' - The token returned by a previous call to indicate that there is more data available.
-- * 'stateValue' - Specify this parameter to receive information only about alarms that are currently in the state that you specify.
-- * 'alarmNames' - The names of the alarms to retrieve information about.
-- * 'maxRecords' - The maximum number of alarm descriptions to retrieve.
-- * 'parentsOfAlarmName' - If you use this parameter and specify the name of a metric or composite alarm, the operation returns information about the "parent" alarms of the alarm you specify. These are the composite alarms that have @AlarmRule@ parameters that reference the alarm named in @ParentsOfAlarmName@ . Information about the alarm that you specify in @ParentsOfAlarmName@ is not returned.
--
-- If you specify @ParentsOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
-- * 'childrenOfAlarmName' - If you use this parameter and specify the name of a composite alarm, the operation returns information about the "children" alarms of the alarm you specify. These are the metric alarms and composite alarms referenced in the @AlarmRule@ field of the composite alarm that you specify in @ChildrenOfAlarmName@ . Information about the composite alarm that you name in @ChildrenOfAlarmName@ is not returned.
--
-- If you specify @ChildrenOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
mkDescribeAlarms ::
  DescribeAlarms
mkDescribeAlarms =
  DescribeAlarms'
    { alarmNamePrefix = Lude.Nothing,
      alarmTypes = Lude.Nothing,
      actionPrefix = Lude.Nothing,
      nextToken = Lude.Nothing,
      stateValue = Lude.Nothing,
      alarmNames = Lude.Nothing,
      maxRecords = Lude.Nothing,
      parentsOfAlarmName = Lude.Nothing,
      childrenOfAlarmName = Lude.Nothing
    }

-- | An alarm name prefix. If you specify this parameter, you receive information about all alarms that have names that start with this prefix.
--
-- If this parameter is specified, you cannot specify @AlarmNames@ .
--
-- /Note:/ Consider using 'alarmNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAlarmNamePrefix :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Text)
dAlarmNamePrefix = Lens.lens (alarmNamePrefix :: DescribeAlarms -> Lude.Maybe Lude.Text) (\s a -> s {alarmNamePrefix = a} :: DescribeAlarms)
{-# DEPRECATED dAlarmNamePrefix "Use generic-lens or generic-optics with 'alarmNamePrefix' instead." #-}

-- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
--
-- /Note:/ Consider using 'alarmTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAlarmTypes :: Lens.Lens' DescribeAlarms (Lude.Maybe [AlarmType])
dAlarmTypes = Lens.lens (alarmTypes :: DescribeAlarms -> Lude.Maybe [AlarmType]) (\s a -> s {alarmTypes = a} :: DescribeAlarms)
{-# DEPRECATED dAlarmTypes "Use generic-lens or generic-optics with 'alarmTypes' instead." #-}

-- | Use this parameter to filter the results of the operation to only those alarms that use a certain alarm action. For example, you could specify the ARN of an SNS topic to find all alarms that send notifications to that topic.
--
-- /Note:/ Consider using 'actionPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActionPrefix :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Text)
dActionPrefix = Lens.lens (actionPrefix :: DescribeAlarms -> Lude.Maybe Lude.Text) (\s a -> s {actionPrefix = a} :: DescribeAlarms)
{-# DEPRECATED dActionPrefix "Use generic-lens or generic-optics with 'actionPrefix' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Text)
dNextToken = Lens.lens (nextToken :: DescribeAlarms -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAlarms)
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify this parameter to receive information only about alarms that are currently in the state that you specify.
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStateValue :: Lens.Lens' DescribeAlarms (Lude.Maybe StateValue)
dStateValue = Lens.lens (stateValue :: DescribeAlarms -> Lude.Maybe StateValue) (\s a -> s {stateValue = a} :: DescribeAlarms)
{-# DEPRECATED dStateValue "Use generic-lens or generic-optics with 'stateValue' instead." #-}

-- | The names of the alarms to retrieve information about.
--
-- /Note:/ Consider using 'alarmNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAlarmNames :: Lens.Lens' DescribeAlarms (Lude.Maybe [Lude.Text])
dAlarmNames = Lens.lens (alarmNames :: DescribeAlarms -> Lude.Maybe [Lude.Text]) (\s a -> s {alarmNames = a} :: DescribeAlarms)
{-# DEPRECATED dAlarmNames "Use generic-lens or generic-optics with 'alarmNames' instead." #-}

-- | The maximum number of alarm descriptions to retrieve.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxRecords :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Natural)
dMaxRecords = Lens.lens (maxRecords :: DescribeAlarms -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeAlarms)
{-# DEPRECATED dMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | If you use this parameter and specify the name of a metric or composite alarm, the operation returns information about the "parent" alarms of the alarm you specify. These are the composite alarms that have @AlarmRule@ parameters that reference the alarm named in @ParentsOfAlarmName@ . Information about the alarm that you specify in @ParentsOfAlarmName@ is not returned.
--
-- If you specify @ParentsOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
--
-- /Note:/ Consider using 'parentsOfAlarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dParentsOfAlarmName :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Text)
dParentsOfAlarmName = Lens.lens (parentsOfAlarmName :: DescribeAlarms -> Lude.Maybe Lude.Text) (\s a -> s {parentsOfAlarmName = a} :: DescribeAlarms)
{-# DEPRECATED dParentsOfAlarmName "Use generic-lens or generic-optics with 'parentsOfAlarmName' instead." #-}

-- | If you use this parameter and specify the name of a composite alarm, the operation returns information about the "children" alarms of the alarm you specify. These are the metric alarms and composite alarms referenced in the @AlarmRule@ field of the composite alarm that you specify in @ChildrenOfAlarmName@ . Information about the composite alarm that you name in @ChildrenOfAlarmName@ is not returned.
--
-- If you specify @ChildrenOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
--
-- /Note:/ Consider using 'childrenOfAlarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChildrenOfAlarmName :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Text)
dChildrenOfAlarmName = Lens.lens (childrenOfAlarmName :: DescribeAlarms -> Lude.Maybe Lude.Text) (\s a -> s {childrenOfAlarmName = a} :: DescribeAlarms)
{-# DEPRECATED dChildrenOfAlarmName "Use generic-lens or generic-optics with 'childrenOfAlarmName' instead." #-}

instance Page.AWSPager DescribeAlarms where
  page rq rs
    | Page.stop (rs Lens.^. darsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. darsMetricAlarms) = Lude.Nothing
    | Page.stop (rs Lens.^. darsCompositeAlarms) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dNextToken Lens..~ rs Lens.^. darsNextToken

instance Lude.AWSRequest DescribeAlarms where
  type Rs DescribeAlarms = DescribeAlarmsResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "DescribeAlarmsResult"
      ( \s h x ->
          DescribeAlarmsResponse'
            Lude.<$> ( x Lude..@? "MetricAlarms" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "CompositeAlarms" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAlarms where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAlarms where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAlarms where
  toQuery DescribeAlarms' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAlarms" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "AlarmNamePrefix" Lude.=: alarmNamePrefix,
        "AlarmTypes"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> alarmTypes),
        "ActionPrefix" Lude.=: actionPrefix,
        "NextToken" Lude.=: nextToken,
        "StateValue" Lude.=: stateValue,
        "AlarmNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> alarmNames),
        "MaxRecords" Lude.=: maxRecords,
        "ParentsOfAlarmName" Lude.=: parentsOfAlarmName,
        "ChildrenOfAlarmName" Lude.=: childrenOfAlarmName
      ]

-- | /See:/ 'mkDescribeAlarmsResponse' smart constructor.
data DescribeAlarmsResponse = DescribeAlarmsResponse'
  { -- | The information about any metric alarms returned by the operation.
    metricAlarms :: Lude.Maybe [MetricAlarm],
    -- | The information about any composite alarms returned by the operation.
    compositeAlarms :: Lude.Maybe [CompositeAlarm],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlarmsResponse' with the minimum fields required to make a request.
--
-- * 'metricAlarms' - The information about any metric alarms returned by the operation.
-- * 'compositeAlarms' - The information about any composite alarms returned by the operation.
-- * 'nextToken' - The token that marks the start of the next batch of returned results.
-- * 'responseStatus' - The response status code.
mkDescribeAlarmsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAlarmsResponse
mkDescribeAlarmsResponse pResponseStatus_ =
  DescribeAlarmsResponse'
    { metricAlarms = Lude.Nothing,
      compositeAlarms = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The information about any metric alarms returned by the operation.
--
-- /Note:/ Consider using 'metricAlarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsMetricAlarms :: Lens.Lens' DescribeAlarmsResponse (Lude.Maybe [MetricAlarm])
darsMetricAlarms = Lens.lens (metricAlarms :: DescribeAlarmsResponse -> Lude.Maybe [MetricAlarm]) (\s a -> s {metricAlarms = a} :: DescribeAlarmsResponse)
{-# DEPRECATED darsMetricAlarms "Use generic-lens or generic-optics with 'metricAlarms' instead." #-}

-- | The information about any composite alarms returned by the operation.
--
-- /Note:/ Consider using 'compositeAlarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsCompositeAlarms :: Lens.Lens' DescribeAlarmsResponse (Lude.Maybe [CompositeAlarm])
darsCompositeAlarms = Lens.lens (compositeAlarms :: DescribeAlarmsResponse -> Lude.Maybe [CompositeAlarm]) (\s a -> s {compositeAlarms = a} :: DescribeAlarmsResponse)
{-# DEPRECATED darsCompositeAlarms "Use generic-lens or generic-optics with 'compositeAlarms' instead." #-}

-- | The token that marks the start of the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsNextToken :: Lens.Lens' DescribeAlarmsResponse (Lude.Maybe Lude.Text)
darsNextToken = Lens.lens (nextToken :: DescribeAlarmsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAlarmsResponse)
{-# DEPRECATED darsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeAlarmsResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeAlarmsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAlarmsResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
