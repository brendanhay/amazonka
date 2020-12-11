{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    pcaAlarmDescription,
    pcaOKActions,
    pcaActionsEnabled,
    pcaInsufficientDataActions,
    pcaAlarmActions,
    pcaTags,
    pcaAlarmName,
    pcaAlarmRule,

    -- * Destructuring the response
    PutCompositeAlarmResponse (..),
    mkPutCompositeAlarmResponse,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutCompositeAlarm' smart constructor.
data PutCompositeAlarm = PutCompositeAlarm'
  { alarmDescription ::
      Lude.Maybe Lude.Text,
    okActions :: Lude.Maybe [Lude.Text],
    actionsEnabled :: Lude.Maybe Lude.Bool,
    insufficientDataActions :: Lude.Maybe [Lude.Text],
    alarmActions :: Lude.Maybe [Lude.Text],
    tags :: Lude.Maybe [Tag],
    alarmName :: Lude.Text,
    alarmRule :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutCompositeAlarm' with the minimum fields required to make a request.
--
-- * 'actionsEnabled' - Indicates whether actions should be executed during any changes to the alarm state of the composite alarm. The default is @TRUE@ .
-- * 'alarmActions' - The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
-- * 'alarmDescription' - The description for the composite alarm.
-- * 'alarmName' - The name for the composite alarm. This name must be unique within the Region.
-- * 'alarmRule' - An expression that specifies which other alarms are to be evaluated to determine this composite alarm's state. For each alarm that you reference, you designate a function that specifies whether that alarm needs to be in ALARM state, OK state, or INSUFFICIENT_DATA state. You can use operators (AND, OR and NOT) to combine multiple functions in a single expression. You can use parenthesis to logically group the functions in your expression.
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
-- * 'insufficientDataActions' - The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
-- * 'okActions' - The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
-- * 'tags' - A list of key-value pairs to associate with the composite alarm. You can associate as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only resources with certain tag values.
mkPutCompositeAlarm ::
  -- | 'alarmName'
  Lude.Text ->
  -- | 'alarmRule'
  Lude.Text ->
  PutCompositeAlarm
mkPutCompositeAlarm pAlarmName_ pAlarmRule_ =
  PutCompositeAlarm'
    { alarmDescription = Lude.Nothing,
      okActions = Lude.Nothing,
      actionsEnabled = Lude.Nothing,
      insufficientDataActions = Lude.Nothing,
      alarmActions = Lude.Nothing,
      tags = Lude.Nothing,
      alarmName = pAlarmName_,
      alarmRule = pAlarmRule_
    }

-- | The description for the composite alarm.
--
-- /Note:/ Consider using 'alarmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaAlarmDescription :: Lens.Lens' PutCompositeAlarm (Lude.Maybe Lude.Text)
pcaAlarmDescription = Lens.lens (alarmDescription :: PutCompositeAlarm -> Lude.Maybe Lude.Text) (\s a -> s {alarmDescription = a} :: PutCompositeAlarm)
{-# DEPRECATED pcaAlarmDescription "Use generic-lens or generic-optics with 'alarmDescription' instead." #-}

-- | The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
--
-- /Note:/ Consider using 'okActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaOKActions :: Lens.Lens' PutCompositeAlarm (Lude.Maybe [Lude.Text])
pcaOKActions = Lens.lens (okActions :: PutCompositeAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {okActions = a} :: PutCompositeAlarm)
{-# DEPRECATED pcaOKActions "Use generic-lens or generic-optics with 'okActions' instead." #-}

-- | Indicates whether actions should be executed during any changes to the alarm state of the composite alarm. The default is @TRUE@ .
--
-- /Note:/ Consider using 'actionsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaActionsEnabled :: Lens.Lens' PutCompositeAlarm (Lude.Maybe Lude.Bool)
pcaActionsEnabled = Lens.lens (actionsEnabled :: PutCompositeAlarm -> Lude.Maybe Lude.Bool) (\s a -> s {actionsEnabled = a} :: PutCompositeAlarm)
{-# DEPRECATED pcaActionsEnabled "Use generic-lens or generic-optics with 'actionsEnabled' instead." #-}

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
--
-- /Note:/ Consider using 'insufficientDataActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaInsufficientDataActions :: Lens.Lens' PutCompositeAlarm (Lude.Maybe [Lude.Text])
pcaInsufficientDataActions = Lens.lens (insufficientDataActions :: PutCompositeAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {insufficientDataActions = a} :: PutCompositeAlarm)
{-# DEPRECATED pcaInsufficientDataActions "Use generic-lens or generic-optics with 'insufficientDataActions' instead." #-}

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @
--
-- /Note:/ Consider using 'alarmActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaAlarmActions :: Lens.Lens' PutCompositeAlarm (Lude.Maybe [Lude.Text])
pcaAlarmActions = Lens.lens (alarmActions :: PutCompositeAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {alarmActions = a} :: PutCompositeAlarm)
{-# DEPRECATED pcaAlarmActions "Use generic-lens or generic-optics with 'alarmActions' instead." #-}

-- | A list of key-value pairs to associate with the composite alarm. You can associate as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only resources with certain tag values.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaTags :: Lens.Lens' PutCompositeAlarm (Lude.Maybe [Tag])
pcaTags = Lens.lens (tags :: PutCompositeAlarm -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PutCompositeAlarm)
{-# DEPRECATED pcaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name for the composite alarm. This name must be unique within the Region.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaAlarmName :: Lens.Lens' PutCompositeAlarm Lude.Text
pcaAlarmName = Lens.lens (alarmName :: PutCompositeAlarm -> Lude.Text) (\s a -> s {alarmName = a} :: PutCompositeAlarm)
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
pcaAlarmRule :: Lens.Lens' PutCompositeAlarm Lude.Text
pcaAlarmRule = Lens.lens (alarmRule :: PutCompositeAlarm -> Lude.Text) (\s a -> s {alarmRule = a} :: PutCompositeAlarm)
{-# DEPRECATED pcaAlarmRule "Use generic-lens or generic-optics with 'alarmRule' instead." #-}

instance Lude.AWSRequest PutCompositeAlarm where
  type Rs PutCompositeAlarm = PutCompositeAlarmResponse
  request = Req.postQuery cloudWatchService
  response = Res.receiveNull PutCompositeAlarmResponse'

instance Lude.ToHeaders PutCompositeAlarm where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutCompositeAlarm where
  toPath = Lude.const "/"

instance Lude.ToQuery PutCompositeAlarm where
  toQuery PutCompositeAlarm' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutCompositeAlarm" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "AlarmDescription" Lude.=: alarmDescription,
        "OKActions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> okActions),
        "ActionsEnabled" Lude.=: actionsEnabled,
        "InsufficientDataActions"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> insufficientDataActions),
        "AlarmActions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> alarmActions),
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "AlarmName" Lude.=: alarmName,
        "AlarmRule" Lude.=: alarmRule
      ]

-- | /See:/ 'mkPutCompositeAlarmResponse' smart constructor.
data PutCompositeAlarmResponse = PutCompositeAlarmResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutCompositeAlarmResponse' with the minimum fields required to make a request.
mkPutCompositeAlarmResponse ::
  PutCompositeAlarmResponse
mkPutCompositeAlarmResponse = PutCompositeAlarmResponse'
