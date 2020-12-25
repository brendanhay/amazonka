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
    daActionPrefix,
    daAlarmNamePrefix,
    daAlarmNames,
    daAlarmTypes,
    daChildrenOfAlarmName,
    daMaxRecords,
    daNextToken,
    daParentsOfAlarmName,
    daStateValue,

    -- * Destructuring the response
    DescribeAlarmsResponse (..),
    mkDescribeAlarmsResponse,

    -- ** Response lenses
    darrsCompositeAlarms,
    darrsMetricAlarms,
    darrsNextToken,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAlarms' smart constructor.
data DescribeAlarms = DescribeAlarms'
  { -- | Use this parameter to filter the results of the operation to only those alarms that use a certain alarm action. For example, you could specify the ARN of an SNS topic to find all alarms that send notifications to that topic.
    actionPrefix :: Core.Maybe Types.ActionPrefix,
    -- | An alarm name prefix. If you specify this parameter, you receive information about all alarms that have names that start with this prefix.
    --
    -- If this parameter is specified, you cannot specify @AlarmNames@ .
    alarmNamePrefix :: Core.Maybe Types.AlarmNamePrefix,
    -- | The names of the alarms to retrieve information about.
    alarmNames :: Core.Maybe [Types.AlarmName],
    -- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
    alarmTypes :: Core.Maybe [Types.AlarmType],
    -- | If you use this parameter and specify the name of a composite alarm, the operation returns information about the "children" alarms of the alarm you specify. These are the metric alarms and composite alarms referenced in the @AlarmRule@ field of the composite alarm that you specify in @ChildrenOfAlarmName@ . Information about the composite alarm that you name in @ChildrenOfAlarmName@ is not returned.
    --
    -- If you specify @ChildrenOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
    childrenOfAlarmName :: Core.Maybe Types.AlarmName,
    -- | The maximum number of alarm descriptions to retrieve.
    maxRecords :: Core.Maybe Core.Natural,
    -- | The token returned by a previous call to indicate that there is more data available.
    nextToken :: Core.Maybe Types.NextToken,
    -- | If you use this parameter and specify the name of a metric or composite alarm, the operation returns information about the "parent" alarms of the alarm you specify. These are the composite alarms that have @AlarmRule@ parameters that reference the alarm named in @ParentsOfAlarmName@ . Information about the alarm that you specify in @ParentsOfAlarmName@ is not returned.
    --
    -- If you specify @ParentsOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
    parentsOfAlarmName :: Core.Maybe Types.AlarmName,
    -- | Specify this parameter to receive information only about alarms that are currently in the state that you specify.
    stateValue :: Core.Maybe Types.StateValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAlarms' value with any optional fields omitted.
mkDescribeAlarms ::
  DescribeAlarms
mkDescribeAlarms =
  DescribeAlarms'
    { actionPrefix = Core.Nothing,
      alarmNamePrefix = Core.Nothing,
      alarmNames = Core.Nothing,
      alarmTypes = Core.Nothing,
      childrenOfAlarmName = Core.Nothing,
      maxRecords = Core.Nothing,
      nextToken = Core.Nothing,
      parentsOfAlarmName = Core.Nothing,
      stateValue = Core.Nothing
    }

-- | Use this parameter to filter the results of the operation to only those alarms that use a certain alarm action. For example, you could specify the ARN of an SNS topic to find all alarms that send notifications to that topic.
--
-- /Note:/ Consider using 'actionPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daActionPrefix :: Lens.Lens' DescribeAlarms (Core.Maybe Types.ActionPrefix)
daActionPrefix = Lens.field @"actionPrefix"
{-# DEPRECATED daActionPrefix "Use generic-lens or generic-optics with 'actionPrefix' instead." #-}

-- | An alarm name prefix. If you specify this parameter, you receive information about all alarms that have names that start with this prefix.
--
-- If this parameter is specified, you cannot specify @AlarmNames@ .
--
-- /Note:/ Consider using 'alarmNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlarmNamePrefix :: Lens.Lens' DescribeAlarms (Core.Maybe Types.AlarmNamePrefix)
daAlarmNamePrefix = Lens.field @"alarmNamePrefix"
{-# DEPRECATED daAlarmNamePrefix "Use generic-lens or generic-optics with 'alarmNamePrefix' instead." #-}

-- | The names of the alarms to retrieve information about.
--
-- /Note:/ Consider using 'alarmNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlarmNames :: Lens.Lens' DescribeAlarms (Core.Maybe [Types.AlarmName])
daAlarmNames = Lens.field @"alarmNames"
{-# DEPRECATED daAlarmNames "Use generic-lens or generic-optics with 'alarmNames' instead." #-}

-- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
--
-- /Note:/ Consider using 'alarmTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlarmTypes :: Lens.Lens' DescribeAlarms (Core.Maybe [Types.AlarmType])
daAlarmTypes = Lens.field @"alarmTypes"
{-# DEPRECATED daAlarmTypes "Use generic-lens or generic-optics with 'alarmTypes' instead." #-}

-- | If you use this parameter and specify the name of a composite alarm, the operation returns information about the "children" alarms of the alarm you specify. These are the metric alarms and composite alarms referenced in the @AlarmRule@ field of the composite alarm that you specify in @ChildrenOfAlarmName@ . Information about the composite alarm that you name in @ChildrenOfAlarmName@ is not returned.
--
-- If you specify @ChildrenOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
--
-- /Note:/ Consider using 'childrenOfAlarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daChildrenOfAlarmName :: Lens.Lens' DescribeAlarms (Core.Maybe Types.AlarmName)
daChildrenOfAlarmName = Lens.field @"childrenOfAlarmName"
{-# DEPRECATED daChildrenOfAlarmName "Use generic-lens or generic-optics with 'childrenOfAlarmName' instead." #-}

-- | The maximum number of alarm descriptions to retrieve.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMaxRecords :: Lens.Lens' DescribeAlarms (Core.Maybe Core.Natural)
daMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED daMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daNextToken :: Lens.Lens' DescribeAlarms (Core.Maybe Types.NextToken)
daNextToken = Lens.field @"nextToken"
{-# DEPRECATED daNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | If you use this parameter and specify the name of a metric or composite alarm, the operation returns information about the "parent" alarms of the alarm you specify. These are the composite alarms that have @AlarmRule@ parameters that reference the alarm named in @ParentsOfAlarmName@ . Information about the alarm that you specify in @ParentsOfAlarmName@ is not returned.
--
-- If you specify @ParentsOfAlarmName@ , you cannot specify any other parameters in the request except for @MaxRecords@ and @NextToken@ . If you do so, you receive a validation error.
--
-- /Note:/ Consider using 'parentsOfAlarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daParentsOfAlarmName :: Lens.Lens' DescribeAlarms (Core.Maybe Types.AlarmName)
daParentsOfAlarmName = Lens.field @"parentsOfAlarmName"
{-# DEPRECATED daParentsOfAlarmName "Use generic-lens or generic-optics with 'parentsOfAlarmName' instead." #-}

-- | Specify this parameter to receive information only about alarms that are currently in the state that you specify.
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daStateValue :: Lens.Lens' DescribeAlarms (Core.Maybe Types.StateValue)
daStateValue = Lens.field @"stateValue"
{-# DEPRECATED daStateValue "Use generic-lens or generic-optics with 'stateValue' instead." #-}

instance Core.AWSRequest DescribeAlarms where
  type Rs DescribeAlarms = DescribeAlarmsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeAlarms")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> (Core.toQueryValue "ActionPrefix" Core.<$> actionPrefix)
                Core.<> (Core.toQueryValue "AlarmNamePrefix" Core.<$> alarmNamePrefix)
                Core.<> ( Core.toQueryValue
                            "AlarmNames"
                            (Core.toQueryList "member" Core.<$> alarmNames)
                        )
                Core.<> ( Core.toQueryValue
                            "AlarmTypes"
                            (Core.toQueryList "member" Core.<$> alarmTypes)
                        )
                Core.<> ( Core.toQueryValue "ChildrenOfAlarmName"
                            Core.<$> childrenOfAlarmName
                        )
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryValue "ParentsOfAlarmName"
                            Core.<$> parentsOfAlarmName
                        )
                Core.<> (Core.toQueryValue "StateValue" Core.<$> stateValue)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeAlarmsResult"
      ( \s h x ->
          DescribeAlarmsResponse'
            Core.<$> (x Core..@? "CompositeAlarms" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "MetricAlarms" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAlarms where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"metricAlarms" Core.. Lens._Just) =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"compositeAlarms" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAlarmsResponse' smart constructor.
data DescribeAlarmsResponse = DescribeAlarmsResponse'
  { -- | The information about any composite alarms returned by the operation.
    compositeAlarms :: Core.Maybe [Types.CompositeAlarm],
    -- | The information about any metric alarms returned by the operation.
    metricAlarms :: Core.Maybe [Types.MetricAlarm],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAlarmsResponse' value with any optional fields omitted.
mkDescribeAlarmsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAlarmsResponse
mkDescribeAlarmsResponse responseStatus =
  DescribeAlarmsResponse'
    { compositeAlarms = Core.Nothing,
      metricAlarms = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The information about any composite alarms returned by the operation.
--
-- /Note:/ Consider using 'compositeAlarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsCompositeAlarms :: Lens.Lens' DescribeAlarmsResponse (Core.Maybe [Types.CompositeAlarm])
darrsCompositeAlarms = Lens.field @"compositeAlarms"
{-# DEPRECATED darrsCompositeAlarms "Use generic-lens or generic-optics with 'compositeAlarms' instead." #-}

-- | The information about any metric alarms returned by the operation.
--
-- /Note:/ Consider using 'metricAlarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsMetricAlarms :: Lens.Lens' DescribeAlarmsResponse (Core.Maybe [Types.MetricAlarm])
darrsMetricAlarms = Lens.field @"metricAlarms"
{-# DEPRECATED darrsMetricAlarms "Use generic-lens or generic-optics with 'metricAlarms' instead." #-}

-- | The token that marks the start of the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsNextToken :: Lens.Lens' DescribeAlarmsResponse (Core.Maybe Types.NextToken)
darrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED darrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAlarmsResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
