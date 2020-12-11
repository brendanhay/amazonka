{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    daAlarmNamePrefix,
    daAlarmTypes,
    daActionPrefix,
    daNextToken,
    daStateValue,
    daAlarmNames,
    daMaxRecords,
    daParentsOfAlarmName,
    daChildrenOfAlarmName,

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
  { alarmNamePrefix ::
      Lude.Maybe Lude.Text,
    alarmTypes :: Lude.Maybe [AlarmType],
    actionPrefix :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    stateValue :: Lude.Maybe StateValue,
    alarmNames :: Lude.Maybe [Lude.Text],
    maxRecords :: Lude.Maybe Lude.Natural,
    parentsOfAlarmName :: Lude.Maybe Lude.Text,
    childrenOfAlarmName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlarms' with the minimum fields required to make a request.
--
-- * 'actionPrefix' - Use this parameter to filter the results of the operation to only those alarms that use a certain alarm action. For example, you could specify the ARN of an SNS topic to find all alarms that send notifications to that topic.
-- * 'alarmNamePrefix' - An alarm name prefix. If you specify this parameter, you receive information about all alarms that have names that start with this prefix.
--
-- If this parameter is specified, you cannot specify @AlarmNames@ .
-- * 'alarmNames' - The names of the alarms to retrieve information about.
-- * 'alarmTypes' - Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
-- * 'childrenOfAlarmName' - If you use this parameter and specify the name of a composite alarm, the operation returns information about the "children" alarms of the alarm you specify. These are the metric alarms and composite alarms referenced in the @AlarmRule@ field of the composite alarm that you specify in @ChildrenOfAlarmName@ . Information about the composite alarm that you name in @ChildrenOfAlarmName@ is not returned.
--
-- If you specify @ChildrenOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
-- * 'maxRecords' - The maximum number of alarm descriptions to retrieve.
-- * 'nextToken' - The token returned by a previous call to indicate that there is more data available.
-- * 'parentsOfAlarmName' - If you use this parameter and specify the name of a metric or composite alarm, the operation returns information about the "parent" alarms of the alarm you specify. These are the composite alarms that have @AlarmRule@ parameters that reference the alarm named in @ParentsOfAlarmName@ . Information about the alarm that you specify in @ParentsOfAlarmName@ is not returned.
--
-- If you specify @ParentsOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
-- * 'stateValue' - Specify this parameter to receive information only about alarms that are currently in the state that you specify.
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
daAlarmNamePrefix :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Text)
daAlarmNamePrefix = Lens.lens (alarmNamePrefix :: DescribeAlarms -> Lude.Maybe Lude.Text) (\s a -> s {alarmNamePrefix = a} :: DescribeAlarms)
{-# DEPRECATED daAlarmNamePrefix "Use generic-lens or generic-optics with 'alarmNamePrefix' instead." #-}

-- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
--
-- /Note:/ Consider using 'alarmTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlarmTypes :: Lens.Lens' DescribeAlarms (Lude.Maybe [AlarmType])
daAlarmTypes = Lens.lens (alarmTypes :: DescribeAlarms -> Lude.Maybe [AlarmType]) (\s a -> s {alarmTypes = a} :: DescribeAlarms)
{-# DEPRECATED daAlarmTypes "Use generic-lens or generic-optics with 'alarmTypes' instead." #-}

-- | Use this parameter to filter the results of the operation to only those alarms that use a certain alarm action. For example, you could specify the ARN of an SNS topic to find all alarms that send notifications to that topic.
--
-- /Note:/ Consider using 'actionPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daActionPrefix :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Text)
daActionPrefix = Lens.lens (actionPrefix :: DescribeAlarms -> Lude.Maybe Lude.Text) (\s a -> s {actionPrefix = a} :: DescribeAlarms)
{-# DEPRECATED daActionPrefix "Use generic-lens or generic-optics with 'actionPrefix' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daNextToken :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Text)
daNextToken = Lens.lens (nextToken :: DescribeAlarms -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAlarms)
{-# DEPRECATED daNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify this parameter to receive information only about alarms that are currently in the state that you specify.
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daStateValue :: Lens.Lens' DescribeAlarms (Lude.Maybe StateValue)
daStateValue = Lens.lens (stateValue :: DescribeAlarms -> Lude.Maybe StateValue) (\s a -> s {stateValue = a} :: DescribeAlarms)
{-# DEPRECATED daStateValue "Use generic-lens or generic-optics with 'stateValue' instead." #-}

-- | The names of the alarms to retrieve information about.
--
-- /Note:/ Consider using 'alarmNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlarmNames :: Lens.Lens' DescribeAlarms (Lude.Maybe [Lude.Text])
daAlarmNames = Lens.lens (alarmNames :: DescribeAlarms -> Lude.Maybe [Lude.Text]) (\s a -> s {alarmNames = a} :: DescribeAlarms)
{-# DEPRECATED daAlarmNames "Use generic-lens or generic-optics with 'alarmNames' instead." #-}

-- | The maximum number of alarm descriptions to retrieve.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMaxRecords :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Natural)
daMaxRecords = Lens.lens (maxRecords :: DescribeAlarms -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeAlarms)
{-# DEPRECATED daMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | If you use this parameter and specify the name of a metric or composite alarm, the operation returns information about the "parent" alarms of the alarm you specify. These are the composite alarms that have @AlarmRule@ parameters that reference the alarm named in @ParentsOfAlarmName@ . Information about the alarm that you specify in @ParentsOfAlarmName@ is not returned.
--
-- If you specify @ParentsOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
--
-- /Note:/ Consider using 'parentsOfAlarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daParentsOfAlarmName :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Text)
daParentsOfAlarmName = Lens.lens (parentsOfAlarmName :: DescribeAlarms -> Lude.Maybe Lude.Text) (\s a -> s {parentsOfAlarmName = a} :: DescribeAlarms)
{-# DEPRECATED daParentsOfAlarmName "Use generic-lens or generic-optics with 'parentsOfAlarmName' instead." #-}

-- | If you use this parameter and specify the name of a composite alarm, the operation returns information about the "children" alarms of the alarm you specify. These are the metric alarms and composite alarms referenced in the @AlarmRule@ field of the composite alarm that you specify in @ChildrenOfAlarmName@ . Information about the composite alarm that you name in @ChildrenOfAlarmName@ is not returned.
--
-- If you specify @ChildrenOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
--
-- /Note:/ Consider using 'childrenOfAlarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daChildrenOfAlarmName :: Lens.Lens' DescribeAlarms (Lude.Maybe Lude.Text)
daChildrenOfAlarmName = Lens.lens (childrenOfAlarmName :: DescribeAlarms -> Lude.Maybe Lude.Text) (\s a -> s {childrenOfAlarmName = a} :: DescribeAlarms)
{-# DEPRECATED daChildrenOfAlarmName "Use generic-lens or generic-optics with 'childrenOfAlarmName' instead." #-}

instance Page.AWSPager DescribeAlarms where
  page rq rs
    | Page.stop (rs Lens.^. darsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. darsMetricAlarms) = Lude.Nothing
    | Page.stop (rs Lens.^. darsCompositeAlarms) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daNextToken Lens..~ rs Lens.^. darsNextToken

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
  { metricAlarms ::
      Lude.Maybe [MetricAlarm],
    compositeAlarms ::
      Lude.Maybe [CompositeAlarm],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlarmsResponse' with the minimum fields required to make a request.
--
-- * 'compositeAlarms' - The information about any composite alarms returned by the operation.
-- * 'metricAlarms' - The information about any metric alarms returned by the operation.
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
