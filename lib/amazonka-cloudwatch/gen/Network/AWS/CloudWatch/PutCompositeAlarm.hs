{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutCompositeAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a /composite alarm/ . When you create a composite alarm, you specify a rule expression for the alarm that takes into account the alarm states of other alarms that you have created. The composite alarm goes into ALARM state only if all conditions of the rule are met.
--
-- The alarms specified in a composite alarm's rule expression can include metric alarms and other composite alarms.
-- Using composite alarms can reduce alarm noise. You can create multiple metric alarms, and also create a composite alarm and set up alerts only for the composite alarm. For example, you could create a composite alarm that goes into ALARM state only when more than one of the underlying metric alarms are in ALARM state.
-- Currently, the only alarm actions that can be taken by composite alarms are notifying SNS topics.
-- When this operation creates an alarm, the alarm state is immediately set to @INSUFFICIENT_DATA@ . The alarm is then evaluated and its state is set appropriately. Any actions associated with the new state are then executed. For a composite alarm, this initial time after creation is the only time that the alarm can be in @INSUFFICIENT_DATA@ state.
-- When you update an existing alarm, its state is left unchanged, but the update completely overwrites the previous configuration of the alarm.
module Network.AWS.CloudWatch.PutCompositeAlarm
  ( -- * Creating a request
    PutCompositeAlarm (..),
    mkPutCompositeAlarm,

    -- ** Request lenses
    pcaAlarmName,
    pcaAlarmRule,
    pcaActionsEnabled,
    pcaAlarmActions,
    pcaAlarmDescription,
    pcaInsufficientDataActions,
    pcaOKActions,
    pcaTags,

    -- * Destructuring the response
    PutCompositeAlarmResponse (..),
    mkPutCompositeAlarmResponse,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutCompositeAlarm' smart constructor.
data PutCompositeAlarm = PutCompositeAlarm'
  { -- | The name for the composite alarm. This name must be unique within the Region.
    alarmName :: Types.AlarmName,
    -- | An expression that specifies which other alarms are to be evaluated to determine this composite alarm's state. For each alarm that you reference, you designate a function that specifies whether that alarm needs to be in ALARM state, OK state, or INSUFFICIENT_DATA state. You can use operators (AND, OR and NOT) to combine multiple functions in a single expression. You can use parenthesis to logically group the functions in your expression.
    --
    -- You can use either alarm names or ARNs to reference the other alarms that are to be evaluated.
    -- Functions can include the following:
    --
    --     * @ALARM("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in ALARM state.
    --
    --
    --     * @OK("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in OK state.
    --
    --
    --     * @INSUFFICIENT_DATA("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in INSUFFICIENT_DATA state.
    --
    --
    --     * @TRUE@ always evaluates to TRUE.
    --
    --
    --     * @FALSE@ always evaluates to FALSE.
    --
    --
    -- TRUE and FALSE are useful for testing a complex @AlarmRule@ structure, and for testing your alarm actions.
    -- Alarm names specified in @AlarmRule@ can be surrounded with double-quotes ("), but do not have to be.
    -- The following are some examples of @AlarmRule@ :
    --
    --     * @ALARM(CPUUtilizationTooHigh) AND ALARM(DiskReadOpsTooHigh)@ specifies that the composite alarm goes into ALARM state only if both CPUUtilizationTooHigh and DiskReadOpsTooHigh alarms are in ALARM state.
    --
    --
    --     * @ALARM(CPUUtilizationTooHigh) AND NOT ALARM(DeploymentInProgress)@ specifies that the alarm goes to ALARM state if CPUUtilizationTooHigh is in ALARM state and DeploymentInProgress is not in ALARM state. This example reduces alarm noise during a known deployment window.
    --
    --
    --     * @(ALARM(CPUUtilizationTooHigh) OR ALARM(DiskReadOpsTooHigh)) AND OK(NetworkOutTooHigh)@ goes into ALARM state if CPUUtilizationTooHigh OR DiskReadOpsTooHigh is in ALARM state, and if NetworkOutTooHigh is in OK state. This provides another example of using a composite alarm to prevent noise. This rule ensures that you are not notified with an alarm action on high CPU or disk usage if a known network problem is also occurring.
    --
    --
    -- The @AlarmRule@ can specify as many as 100 "children" alarms. The @AlarmRule@ expression can have as many as 500 elements. Elements are child alarms, TRUE or FALSE statements, and parentheses.
    alarmRule :: Types.AlarmRule,
    -- | Indicates whether actions should be executed during any changes to the alarm state of the composite alarm. The default is @TRUE@ .
    actionsEnabled :: Core.Maybe Core.Bool,
    -- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
    --
    -- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
    alarmActions :: Core.Maybe [Types.ResourceName],
    -- | The description for the composite alarm.
    alarmDescription :: Core.Maybe Types.AlarmDescription,
    -- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
    --
    -- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
    insufficientDataActions :: Core.Maybe [Types.ResourceName],
    -- | The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
    --
    -- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
    oKActions :: Core.Maybe [Types.ResourceName],
    -- | A list of key-value pairs to associate with the composite alarm. You can associate as many as 50 tags with an alarm.
    --
    -- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only resources with certain tag values.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutCompositeAlarm' value with any optional fields omitted.
mkPutCompositeAlarm ::
  -- | 'alarmName'
  Types.AlarmName ->
  -- | 'alarmRule'
  Types.AlarmRule ->
  PutCompositeAlarm
mkPutCompositeAlarm alarmName alarmRule =
  PutCompositeAlarm'
    { alarmName,
      alarmRule,
      actionsEnabled = Core.Nothing,
      alarmActions = Core.Nothing,
      alarmDescription = Core.Nothing,
      insufficientDataActions = Core.Nothing,
      oKActions = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name for the composite alarm. This name must be unique within the Region.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaAlarmName :: Lens.Lens' PutCompositeAlarm Types.AlarmName
pcaAlarmName = Lens.field @"alarmName"
{-# DEPRECATED pcaAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | An expression that specifies which other alarms are to be evaluated to determine this composite alarm's state. For each alarm that you reference, you designate a function that specifies whether that alarm needs to be in ALARM state, OK state, or INSUFFICIENT_DATA state. You can use operators (AND, OR and NOT) to combine multiple functions in a single expression. You can use parenthesis to logically group the functions in your expression.
--
-- You can use either alarm names or ARNs to reference the other alarms that are to be evaluated.
-- Functions can include the following:
--
--     * @ALARM("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in ALARM state.
--
--
--     * @OK("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in OK state.
--
--
--     * @INSUFFICIENT_DATA("/alarm-name/ or /alarm-ARN/ ")@ is TRUE if the named alarm is in INSUFFICIENT_DATA state.
--
--
--     * @TRUE@ always evaluates to TRUE.
--
--
--     * @FALSE@ always evaluates to FALSE.
--
--
-- TRUE and FALSE are useful for testing a complex @AlarmRule@ structure, and for testing your alarm actions.
-- Alarm names specified in @AlarmRule@ can be surrounded with double-quotes ("), but do not have to be.
-- The following are some examples of @AlarmRule@ :
--
--     * @ALARM(CPUUtilizationTooHigh) AND ALARM(DiskReadOpsTooHigh)@ specifies that the composite alarm goes into ALARM state only if both CPUUtilizationTooHigh and DiskReadOpsTooHigh alarms are in ALARM state.
--
--
--     * @ALARM(CPUUtilizationTooHigh) AND NOT ALARM(DeploymentInProgress)@ specifies that the alarm goes to ALARM state if CPUUtilizationTooHigh is in ALARM state and DeploymentInProgress is not in ALARM state. This example reduces alarm noise during a known deployment window.
--
--
--     * @(ALARM(CPUUtilizationTooHigh) OR ALARM(DiskReadOpsTooHigh)) AND OK(NetworkOutTooHigh)@ goes into ALARM state if CPUUtilizationTooHigh OR DiskReadOpsTooHigh is in ALARM state, and if NetworkOutTooHigh is in OK state. This provides another example of using a composite alarm to prevent noise. This rule ensures that you are not notified with an alarm action on high CPU or disk usage if a known network problem is also occurring.
--
--
-- The @AlarmRule@ can specify as many as 100 "children" alarms. The @AlarmRule@ expression can have as many as 500 elements. Elements are child alarms, TRUE or FALSE statements, and parentheses.
--
-- /Note:/ Consider using 'alarmRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaAlarmRule :: Lens.Lens' PutCompositeAlarm Types.AlarmRule
pcaAlarmRule = Lens.field @"alarmRule"
{-# DEPRECATED pcaAlarmRule "Use generic-lens or generic-optics with 'alarmRule' instead." #-}

-- | Indicates whether actions should be executed during any changes to the alarm state of the composite alarm. The default is @TRUE@ .
--
-- /Note:/ Consider using 'actionsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaActionsEnabled :: Lens.Lens' PutCompositeAlarm (Core.Maybe Core.Bool)
pcaActionsEnabled = Lens.field @"actionsEnabled"
{-# DEPRECATED pcaActionsEnabled "Use generic-lens or generic-optics with 'actionsEnabled' instead." #-}

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
--
-- /Note:/ Consider using 'alarmActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaAlarmActions :: Lens.Lens' PutCompositeAlarm (Core.Maybe [Types.ResourceName])
pcaAlarmActions = Lens.field @"alarmActions"
{-# DEPRECATED pcaAlarmActions "Use generic-lens or generic-optics with 'alarmActions' instead." #-}

-- | The description for the composite alarm.
--
-- /Note:/ Consider using 'alarmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaAlarmDescription :: Lens.Lens' PutCompositeAlarm (Core.Maybe Types.AlarmDescription)
pcaAlarmDescription = Lens.field @"alarmDescription"
{-# DEPRECATED pcaAlarmDescription "Use generic-lens or generic-optics with 'alarmDescription' instead." #-}

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
--
-- /Note:/ Consider using 'insufficientDataActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaInsufficientDataActions :: Lens.Lens' PutCompositeAlarm (Core.Maybe [Types.ResourceName])
pcaInsufficientDataActions = Lens.field @"insufficientDataActions"
{-# DEPRECATED pcaInsufficientDataActions "Use generic-lens or generic-optics with 'insufficientDataActions' instead." #-}

-- | The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
--
-- /Note:/ Consider using 'oKActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaOKActions :: Lens.Lens' PutCompositeAlarm (Core.Maybe [Types.ResourceName])
pcaOKActions = Lens.field @"oKActions"
{-# DEPRECATED pcaOKActions "Use generic-lens or generic-optics with 'oKActions' instead." #-}

-- | A list of key-value pairs to associate with the composite alarm. You can associate as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only resources with certain tag values.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaTags :: Lens.Lens' PutCompositeAlarm (Core.Maybe [Types.Tag])
pcaTags = Lens.field @"tags"
{-# DEPRECATED pcaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest PutCompositeAlarm where
  type Rs PutCompositeAlarm = PutCompositeAlarmResponse
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
            ( Core.pure ("Action", "PutCompositeAlarm")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> (Core.toQueryValue "AlarmName" alarmName)
                Core.<> (Core.toQueryValue "AlarmRule" alarmRule)
                Core.<> (Core.toQueryValue "ActionsEnabled" Core.<$> actionsEnabled)
                Core.<> ( Core.toQueryValue
                            "AlarmActions"
                            (Core.toQueryList "member" Core.<$> alarmActions)
                        )
                Core.<> (Core.toQueryValue "AlarmDescription" Core.<$> alarmDescription)
                Core.<> ( Core.toQueryValue
                            "InsufficientDataActions"
                            (Core.toQueryList "member" Core.<$> insufficientDataActions)
                        )
                Core.<> ( Core.toQueryValue
                            "OKActions"
                            (Core.toQueryList "member" Core.<$> oKActions)
                        )
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
            )
      }
  response = Response.receiveNull PutCompositeAlarmResponse'

-- | /See:/ 'mkPutCompositeAlarmResponse' smart constructor.
data PutCompositeAlarmResponse = PutCompositeAlarmResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutCompositeAlarmResponse' value with any optional fields omitted.
mkPutCompositeAlarmResponse ::
  PutCompositeAlarmResponse
mkPutCompositeAlarmResponse = PutCompositeAlarmResponse'
