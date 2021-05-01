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
-- Module      : Network.AWS.CloudWatch.PutCompositeAlarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a /composite alarm/. When you create a composite
-- alarm, you specify a rule expression for the alarm that takes into
-- account the alarm states of other alarms that you have created. The
-- composite alarm goes into ALARM state only if all conditions of the rule
-- are met.
--
-- The alarms specified in a composite alarm\'s rule expression can include
-- metric alarms and other composite alarms.
--
-- Using composite alarms can reduce alarm noise. You can create multiple
-- metric alarms, and also create a composite alarm and set up alerts only
-- for the composite alarm. For example, you could create a composite alarm
-- that goes into ALARM state only when more than one of the underlying
-- metric alarms are in ALARM state.
--
-- Currently, the only alarm actions that can be taken by composite alarms
-- are notifying SNS topics.
--
-- It is possible to create a loop or cycle of composite alarms, where
-- composite alarm A depends on composite alarm B, and composite alarm B
-- also depends on composite alarm A. In this scenario, you can\'t delete
-- any composite alarm that is part of the cycle because there is always
-- still a composite alarm that depends on that alarm that you want to
-- delete.
--
-- To get out of such a situation, you must break the cycle by changing the
-- rule of one of the composite alarms in the cycle to remove a dependency
-- that creates the cycle. The simplest change to make to break a cycle is
-- to change the @AlarmRule@ of one of the alarms to @False@.
--
-- Additionally, the evaluation of composite alarms stops if CloudWatch
-- detects a cycle in the evaluation path.
--
-- When this operation creates an alarm, the alarm state is immediately set
-- to @INSUFFICIENT_DATA@. The alarm is then evaluated and its state is set
-- appropriately. Any actions associated with the new state are then
-- executed. For a composite alarm, this initial time after creation is the
-- only time that the alarm can be in @INSUFFICIENT_DATA@ state.
--
-- When you update an existing alarm, its state is left unchanged, but the
-- update completely overwrites the previous configuration of the alarm.
--
-- If you are an IAM user, you must have @iam:CreateServiceLinkedRole@ to
-- create a composite alarm that has Systems Manager OpsItem actions.
module Network.AWS.CloudWatch.PutCompositeAlarm
  ( -- * Creating a Request
    PutCompositeAlarm (..),
    newPutCompositeAlarm,

    -- * Request Lenses
    putCompositeAlarm_alarmActions,
    putCompositeAlarm_insufficientDataActions,
    putCompositeAlarm_tags,
    putCompositeAlarm_oKActions,
    putCompositeAlarm_actionsEnabled,
    putCompositeAlarm_alarmDescription,
    putCompositeAlarm_alarmName,
    putCompositeAlarm_alarmRule,

    -- * Destructuring the Response
    PutCompositeAlarmResponse (..),
    newPutCompositeAlarmResponse,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutCompositeAlarm' smart constructor.
data PutCompositeAlarm = PutCompositeAlarm'
  { -- | The actions to execute when this alarm transitions to the @ALARM@ state
    -- from any other state. Each action is specified as an Amazon Resource
    -- Name (ARN).
    --
    -- Valid Values: @arn:aws:sns:region:account-id:sns-topic-name @ |
    -- @arn:aws:ssm:region:account-id:opsitem:severity @
    alarmActions :: Prelude.Maybe [Prelude.Text],
    -- | The actions to execute when this alarm transitions to the
    -- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
    -- as an Amazon Resource Name (ARN).
    --
    -- Valid Values: @arn:aws:sns:region:account-id:sns-topic-name @
    insufficientDataActions :: Prelude.Maybe [Prelude.Text],
    -- | A list of key-value pairs to associate with the composite alarm. You can
    -- associate as many as 50 tags with an alarm.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions, by granting a user permission to
    -- access or change only resources with certain tag values.
    tags :: Prelude.Maybe [Tag],
    -- | The actions to execute when this alarm transitions to an @OK@ state from
    -- any other state. Each action is specified as an Amazon Resource Name
    -- (ARN).
    --
    -- Valid Values: @arn:aws:sns:region:account-id:sns-topic-name @
    oKActions :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether actions should be executed during any changes to the
    -- alarm state of the composite alarm. The default is @TRUE@.
    actionsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The description for the composite alarm.
    alarmDescription :: Prelude.Maybe Prelude.Text,
    -- | The name for the composite alarm. This name must be unique within the
    -- Region.
    alarmName :: Prelude.Text,
    -- | An expression that specifies which other alarms are to be evaluated to
    -- determine this composite alarm\'s state. For each alarm that you
    -- reference, you designate a function that specifies whether that alarm
    -- needs to be in ALARM state, OK state, or INSUFFICIENT_DATA state. You
    -- can use operators (AND, OR and NOT) to combine multiple functions in a
    -- single expression. You can use parenthesis to logically group the
    -- functions in your expression.
    --
    -- You can use either alarm names or ARNs to reference the other alarms
    -- that are to be evaluated.
    --
    -- Functions can include the following:
    --
    -- -   @ALARM(\"alarm-name or alarm-ARN\")@ is TRUE if the named alarm is
    --     in ALARM state.
    --
    -- -   @OK(\"alarm-name or alarm-ARN\")@ is TRUE if the named alarm is in
    --     OK state.
    --
    -- -   @INSUFFICIENT_DATA(\"alarm-name or alarm-ARN\")@ is TRUE if the
    --     named alarm is in INSUFFICIENT_DATA state.
    --
    -- -   @TRUE@ always evaluates to TRUE.
    --
    -- -   @FALSE@ always evaluates to FALSE.
    --
    -- TRUE and FALSE are useful for testing a complex @AlarmRule@ structure,
    -- and for testing your alarm actions.
    --
    -- Alarm names specified in @AlarmRule@ can be surrounded with
    -- double-quotes (\"), but do not have to be.
    --
    -- The following are some examples of @AlarmRule@:
    --
    -- -   @ALARM(CPUUtilizationTooHigh) AND ALARM(DiskReadOpsTooHigh)@
    --     specifies that the composite alarm goes into ALARM state only if
    --     both CPUUtilizationTooHigh and DiskReadOpsTooHigh alarms are in
    --     ALARM state.
    --
    -- -   @ALARM(CPUUtilizationTooHigh) AND NOT ALARM(DeploymentInProgress)@
    --     specifies that the alarm goes to ALARM state if
    --     CPUUtilizationTooHigh is in ALARM state and DeploymentInProgress is
    --     not in ALARM state. This example reduces alarm noise during a known
    --     deployment window.
    --
    -- -   @(ALARM(CPUUtilizationTooHigh) OR ALARM(DiskReadOpsTooHigh)) AND OK(NetworkOutTooHigh)@
    --     goes into ALARM state if CPUUtilizationTooHigh OR DiskReadOpsTooHigh
    --     is in ALARM state, and if NetworkOutTooHigh is in OK state. This
    --     provides another example of using a composite alarm to prevent
    --     noise. This rule ensures that you are not notified with an alarm
    --     action on high CPU or disk usage if a known network problem is also
    --     occurring.
    --
    -- The @AlarmRule@ can specify as many as 100 \"children\" alarms. The
    -- @AlarmRule@ expression can have as many as 500 elements. Elements are
    -- child alarms, TRUE or FALSE statements, and parentheses.
    alarmRule :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutCompositeAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmActions', 'putCompositeAlarm_alarmActions' - The actions to execute when this alarm transitions to the @ALARM@ state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
--
-- Valid Values: @arn:aws:sns:region:account-id:sns-topic-name @ |
-- @arn:aws:ssm:region:account-id:opsitem:severity @
--
-- 'insufficientDataActions', 'putCompositeAlarm_insufficientDataActions' - The actions to execute when this alarm transitions to the
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:region:account-id:sns-topic-name @
--
-- 'tags', 'putCompositeAlarm_tags' - A list of key-value pairs to associate with the composite alarm. You can
-- associate as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions, by granting a user permission to
-- access or change only resources with certain tag values.
--
-- 'oKActions', 'putCompositeAlarm_oKActions' - The actions to execute when this alarm transitions to an @OK@ state from
-- any other state. Each action is specified as an Amazon Resource Name
-- (ARN).
--
-- Valid Values: @arn:aws:sns:region:account-id:sns-topic-name @
--
-- 'actionsEnabled', 'putCompositeAlarm_actionsEnabled' - Indicates whether actions should be executed during any changes to the
-- alarm state of the composite alarm. The default is @TRUE@.
--
-- 'alarmDescription', 'putCompositeAlarm_alarmDescription' - The description for the composite alarm.
--
-- 'alarmName', 'putCompositeAlarm_alarmName' - The name for the composite alarm. This name must be unique within the
-- Region.
--
-- 'alarmRule', 'putCompositeAlarm_alarmRule' - An expression that specifies which other alarms are to be evaluated to
-- determine this composite alarm\'s state. For each alarm that you
-- reference, you designate a function that specifies whether that alarm
-- needs to be in ALARM state, OK state, or INSUFFICIENT_DATA state. You
-- can use operators (AND, OR and NOT) to combine multiple functions in a
-- single expression. You can use parenthesis to logically group the
-- functions in your expression.
--
-- You can use either alarm names or ARNs to reference the other alarms
-- that are to be evaluated.
--
-- Functions can include the following:
--
-- -   @ALARM(\"alarm-name or alarm-ARN\")@ is TRUE if the named alarm is
--     in ALARM state.
--
-- -   @OK(\"alarm-name or alarm-ARN\")@ is TRUE if the named alarm is in
--     OK state.
--
-- -   @INSUFFICIENT_DATA(\"alarm-name or alarm-ARN\")@ is TRUE if the
--     named alarm is in INSUFFICIENT_DATA state.
--
-- -   @TRUE@ always evaluates to TRUE.
--
-- -   @FALSE@ always evaluates to FALSE.
--
-- TRUE and FALSE are useful for testing a complex @AlarmRule@ structure,
-- and for testing your alarm actions.
--
-- Alarm names specified in @AlarmRule@ can be surrounded with
-- double-quotes (\"), but do not have to be.
--
-- The following are some examples of @AlarmRule@:
--
-- -   @ALARM(CPUUtilizationTooHigh) AND ALARM(DiskReadOpsTooHigh)@
--     specifies that the composite alarm goes into ALARM state only if
--     both CPUUtilizationTooHigh and DiskReadOpsTooHigh alarms are in
--     ALARM state.
--
-- -   @ALARM(CPUUtilizationTooHigh) AND NOT ALARM(DeploymentInProgress)@
--     specifies that the alarm goes to ALARM state if
--     CPUUtilizationTooHigh is in ALARM state and DeploymentInProgress is
--     not in ALARM state. This example reduces alarm noise during a known
--     deployment window.
--
-- -   @(ALARM(CPUUtilizationTooHigh) OR ALARM(DiskReadOpsTooHigh)) AND OK(NetworkOutTooHigh)@
--     goes into ALARM state if CPUUtilizationTooHigh OR DiskReadOpsTooHigh
--     is in ALARM state, and if NetworkOutTooHigh is in OK state. This
--     provides another example of using a composite alarm to prevent
--     noise. This rule ensures that you are not notified with an alarm
--     action on high CPU or disk usage if a known network problem is also
--     occurring.
--
-- The @AlarmRule@ can specify as many as 100 \"children\" alarms. The
-- @AlarmRule@ expression can have as many as 500 elements. Elements are
-- child alarms, TRUE or FALSE statements, and parentheses.
newPutCompositeAlarm ::
  -- | 'alarmName'
  Prelude.Text ->
  -- | 'alarmRule'
  Prelude.Text ->
  PutCompositeAlarm
newPutCompositeAlarm pAlarmName_ pAlarmRule_ =
  PutCompositeAlarm'
    { alarmActions = Prelude.Nothing,
      insufficientDataActions = Prelude.Nothing,
      tags = Prelude.Nothing,
      oKActions = Prelude.Nothing,
      actionsEnabled = Prelude.Nothing,
      alarmDescription = Prelude.Nothing,
      alarmName = pAlarmName_,
      alarmRule = pAlarmRule_
    }

-- | The actions to execute when this alarm transitions to the @ALARM@ state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
--
-- Valid Values: @arn:aws:sns:region:account-id:sns-topic-name @ |
-- @arn:aws:ssm:region:account-id:opsitem:severity @
putCompositeAlarm_alarmActions :: Lens.Lens' PutCompositeAlarm (Prelude.Maybe [Prelude.Text])
putCompositeAlarm_alarmActions = Lens.lens (\PutCompositeAlarm' {alarmActions} -> alarmActions) (\s@PutCompositeAlarm' {} a -> s {alarmActions = a} :: PutCompositeAlarm) Prelude.. Lens.mapping Prelude._Coerce

-- | The actions to execute when this alarm transitions to the
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:sns:region:account-id:sns-topic-name @
putCompositeAlarm_insufficientDataActions :: Lens.Lens' PutCompositeAlarm (Prelude.Maybe [Prelude.Text])
putCompositeAlarm_insufficientDataActions = Lens.lens (\PutCompositeAlarm' {insufficientDataActions} -> insufficientDataActions) (\s@PutCompositeAlarm' {} a -> s {insufficientDataActions = a} :: PutCompositeAlarm) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of key-value pairs to associate with the composite alarm. You can
-- associate as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions, by granting a user permission to
-- access or change only resources with certain tag values.
putCompositeAlarm_tags :: Lens.Lens' PutCompositeAlarm (Prelude.Maybe [Tag])
putCompositeAlarm_tags = Lens.lens (\PutCompositeAlarm' {tags} -> tags) (\s@PutCompositeAlarm' {} a -> s {tags = a} :: PutCompositeAlarm) Prelude.. Lens.mapping Prelude._Coerce

-- | The actions to execute when this alarm transitions to an @OK@ state from
-- any other state. Each action is specified as an Amazon Resource Name
-- (ARN).
--
-- Valid Values: @arn:aws:sns:region:account-id:sns-topic-name @
putCompositeAlarm_oKActions :: Lens.Lens' PutCompositeAlarm (Prelude.Maybe [Prelude.Text])
putCompositeAlarm_oKActions = Lens.lens (\PutCompositeAlarm' {oKActions} -> oKActions) (\s@PutCompositeAlarm' {} a -> s {oKActions = a} :: PutCompositeAlarm) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether actions should be executed during any changes to the
-- alarm state of the composite alarm. The default is @TRUE@.
putCompositeAlarm_actionsEnabled :: Lens.Lens' PutCompositeAlarm (Prelude.Maybe Prelude.Bool)
putCompositeAlarm_actionsEnabled = Lens.lens (\PutCompositeAlarm' {actionsEnabled} -> actionsEnabled) (\s@PutCompositeAlarm' {} a -> s {actionsEnabled = a} :: PutCompositeAlarm)

-- | The description for the composite alarm.
putCompositeAlarm_alarmDescription :: Lens.Lens' PutCompositeAlarm (Prelude.Maybe Prelude.Text)
putCompositeAlarm_alarmDescription = Lens.lens (\PutCompositeAlarm' {alarmDescription} -> alarmDescription) (\s@PutCompositeAlarm' {} a -> s {alarmDescription = a} :: PutCompositeAlarm)

-- | The name for the composite alarm. This name must be unique within the
-- Region.
putCompositeAlarm_alarmName :: Lens.Lens' PutCompositeAlarm Prelude.Text
putCompositeAlarm_alarmName = Lens.lens (\PutCompositeAlarm' {alarmName} -> alarmName) (\s@PutCompositeAlarm' {} a -> s {alarmName = a} :: PutCompositeAlarm)

-- | An expression that specifies which other alarms are to be evaluated to
-- determine this composite alarm\'s state. For each alarm that you
-- reference, you designate a function that specifies whether that alarm
-- needs to be in ALARM state, OK state, or INSUFFICIENT_DATA state. You
-- can use operators (AND, OR and NOT) to combine multiple functions in a
-- single expression. You can use parenthesis to logically group the
-- functions in your expression.
--
-- You can use either alarm names or ARNs to reference the other alarms
-- that are to be evaluated.
--
-- Functions can include the following:
--
-- -   @ALARM(\"alarm-name or alarm-ARN\")@ is TRUE if the named alarm is
--     in ALARM state.
--
-- -   @OK(\"alarm-name or alarm-ARN\")@ is TRUE if the named alarm is in
--     OK state.
--
-- -   @INSUFFICIENT_DATA(\"alarm-name or alarm-ARN\")@ is TRUE if the
--     named alarm is in INSUFFICIENT_DATA state.
--
-- -   @TRUE@ always evaluates to TRUE.
--
-- -   @FALSE@ always evaluates to FALSE.
--
-- TRUE and FALSE are useful for testing a complex @AlarmRule@ structure,
-- and for testing your alarm actions.
--
-- Alarm names specified in @AlarmRule@ can be surrounded with
-- double-quotes (\"), but do not have to be.
--
-- The following are some examples of @AlarmRule@:
--
-- -   @ALARM(CPUUtilizationTooHigh) AND ALARM(DiskReadOpsTooHigh)@
--     specifies that the composite alarm goes into ALARM state only if
--     both CPUUtilizationTooHigh and DiskReadOpsTooHigh alarms are in
--     ALARM state.
--
-- -   @ALARM(CPUUtilizationTooHigh) AND NOT ALARM(DeploymentInProgress)@
--     specifies that the alarm goes to ALARM state if
--     CPUUtilizationTooHigh is in ALARM state and DeploymentInProgress is
--     not in ALARM state. This example reduces alarm noise during a known
--     deployment window.
--
-- -   @(ALARM(CPUUtilizationTooHigh) OR ALARM(DiskReadOpsTooHigh)) AND OK(NetworkOutTooHigh)@
--     goes into ALARM state if CPUUtilizationTooHigh OR DiskReadOpsTooHigh
--     is in ALARM state, and if NetworkOutTooHigh is in OK state. This
--     provides another example of using a composite alarm to prevent
--     noise. This rule ensures that you are not notified with an alarm
--     action on high CPU or disk usage if a known network problem is also
--     occurring.
--
-- The @AlarmRule@ can specify as many as 100 \"children\" alarms. The
-- @AlarmRule@ expression can have as many as 500 elements. Elements are
-- child alarms, TRUE or FALSE statements, and parentheses.
putCompositeAlarm_alarmRule :: Lens.Lens' PutCompositeAlarm Prelude.Text
putCompositeAlarm_alarmRule = Lens.lens (\PutCompositeAlarm' {alarmRule} -> alarmRule) (\s@PutCompositeAlarm' {} a -> s {alarmRule = a} :: PutCompositeAlarm)

instance Prelude.AWSRequest PutCompositeAlarm where
  type Rs PutCompositeAlarm = PutCompositeAlarmResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull PutCompositeAlarmResponse'

instance Prelude.Hashable PutCompositeAlarm

instance Prelude.NFData PutCompositeAlarm

instance Prelude.ToHeaders PutCompositeAlarm where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PutCompositeAlarm where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutCompositeAlarm where
  toQuery PutCompositeAlarm' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PutCompositeAlarm" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-08-01" :: Prelude.ByteString),
        "AlarmActions"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> alarmActions
            ),
        "InsufficientDataActions"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> insufficientDataActions
            ),
        "Tags"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> tags),
        "OKActions"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> oKActions),
        "ActionsEnabled" Prelude.=: actionsEnabled,
        "AlarmDescription" Prelude.=: alarmDescription,
        "AlarmName" Prelude.=: alarmName,
        "AlarmRule" Prelude.=: alarmRule
      ]

-- | /See:/ 'newPutCompositeAlarmResponse' smart constructor.
data PutCompositeAlarmResponse = PutCompositeAlarmResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutCompositeAlarmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutCompositeAlarmResponse ::
  PutCompositeAlarmResponse
newPutCompositeAlarmResponse =
  PutCompositeAlarmResponse'

instance Prelude.NFData PutCompositeAlarmResponse
