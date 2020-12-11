{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled action. A scheduled action contains a schedule and an Amazon Redshift API action. For example, you can create a schedule of when to run the @ResizeCluster@ API operation.
module Network.AWS.Redshift.CreateScheduledAction
  ( -- * Creating a request
    CreateScheduledAction (..),
    mkCreateScheduledAction,

    -- ** Request lenses
    csaStartTime,
    csaScheduledActionDescription,
    csaEnable,
    csaEndTime,
    csaScheduledActionName,
    csaTargetAction,
    csaSchedule,
    csaIAMRole,

    -- * Destructuring the response
    ScheduledAction (..),
    mkScheduledAction,

    -- ** Response lenses
    saState,
    saTargetAction,
    saStartTime,
    saSchedule,
    saScheduledActionName,
    saScheduledActionDescription,
    saNextInvocations,
    saEndTime,
    saIAMRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateScheduledAction' smart constructor.
data CreateScheduledAction = CreateScheduledAction'
  { startTime ::
      Lude.Maybe Lude.ISO8601,
    scheduledActionDescription ::
      Lude.Maybe Lude.Text,
    enable :: Lude.Maybe Lude.Bool,
    endTime :: Lude.Maybe Lude.ISO8601,
    scheduledActionName :: Lude.Text,
    targetAction :: ScheduledActionType,
    schedule :: Lude.Text,
    iamRole :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateScheduledAction' with the minimum fields required to make a request.
--
-- * 'enable' - If true, the schedule is enabled. If false, the scheduled action does not trigger. For more information about @state@ of the scheduled action, see 'ScheduledAction' .
-- * 'endTime' - The end time in UTC of the scheduled action. After this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
-- * 'iamRole' - The IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
-- * 'schedule' - The schedule in @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
-- * 'scheduledActionDescription' - The description of the scheduled action.
-- * 'scheduledActionName' - The name of the scheduled action. The name must be unique within an account. For more information about this parameter, see 'ScheduledAction' .
-- * 'startTime' - The start time in UTC of the scheduled action. Before this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
-- * 'targetAction' - A JSON format string of the Amazon Redshift API operation with input parameters. For more information about this parameter, see 'ScheduledAction' .
mkCreateScheduledAction ::
  -- | 'scheduledActionName'
  Lude.Text ->
  -- | 'targetAction'
  ScheduledActionType ->
  -- | 'schedule'
  Lude.Text ->
  -- | 'iamRole'
  Lude.Text ->
  CreateScheduledAction
mkCreateScheduledAction
  pScheduledActionName_
  pTargetAction_
  pSchedule_
  pIAMRole_ =
    CreateScheduledAction'
      { startTime = Lude.Nothing,
        scheduledActionDescription = Lude.Nothing,
        enable = Lude.Nothing,
        endTime = Lude.Nothing,
        scheduledActionName = pScheduledActionName_,
        targetAction = pTargetAction_,
        schedule = pSchedule_,
        iamRole = pIAMRole_
      }

-- | The start time in UTC of the scheduled action. Before this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaStartTime :: Lens.Lens' CreateScheduledAction (Lude.Maybe Lude.ISO8601)
csaStartTime = Lens.lens (startTime :: CreateScheduledAction -> Lude.Maybe Lude.ISO8601) (\s a -> s {startTime = a} :: CreateScheduledAction)
{-# DEPRECATED csaStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The description of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaScheduledActionDescription :: Lens.Lens' CreateScheduledAction (Lude.Maybe Lude.Text)
csaScheduledActionDescription = Lens.lens (scheduledActionDescription :: CreateScheduledAction -> Lude.Maybe Lude.Text) (\s a -> s {scheduledActionDescription = a} :: CreateScheduledAction)
{-# DEPRECATED csaScheduledActionDescription "Use generic-lens or generic-optics with 'scheduledActionDescription' instead." #-}

-- | If true, the schedule is enabled. If false, the scheduled action does not trigger. For more information about @state@ of the scheduled action, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaEnable :: Lens.Lens' CreateScheduledAction (Lude.Maybe Lude.Bool)
csaEnable = Lens.lens (enable :: CreateScheduledAction -> Lude.Maybe Lude.Bool) (\s a -> s {enable = a} :: CreateScheduledAction)
{-# DEPRECATED csaEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | The end time in UTC of the scheduled action. After this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaEndTime :: Lens.Lens' CreateScheduledAction (Lude.Maybe Lude.ISO8601)
csaEndTime = Lens.lens (endTime :: CreateScheduledAction -> Lude.Maybe Lude.ISO8601) (\s a -> s {endTime = a} :: CreateScheduledAction)
{-# DEPRECATED csaEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the scheduled action. The name must be unique within an account. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaScheduledActionName :: Lens.Lens' CreateScheduledAction Lude.Text
csaScheduledActionName = Lens.lens (scheduledActionName :: CreateScheduledAction -> Lude.Text) (\s a -> s {scheduledActionName = a} :: CreateScheduledAction)
{-# DEPRECATED csaScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | A JSON format string of the Amazon Redshift API operation with input parameters. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'targetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaTargetAction :: Lens.Lens' CreateScheduledAction ScheduledActionType
csaTargetAction = Lens.lens (targetAction :: CreateScheduledAction -> ScheduledActionType) (\s a -> s {targetAction = a} :: CreateScheduledAction)
{-# DEPRECATED csaTargetAction "Use generic-lens or generic-optics with 'targetAction' instead." #-}

-- | The schedule in @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaSchedule :: Lens.Lens' CreateScheduledAction Lude.Text
csaSchedule = Lens.lens (schedule :: CreateScheduledAction -> Lude.Text) (\s a -> s {schedule = a} :: CreateScheduledAction)
{-# DEPRECATED csaSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaIAMRole :: Lens.Lens' CreateScheduledAction Lude.Text
csaIAMRole = Lens.lens (iamRole :: CreateScheduledAction -> Lude.Text) (\s a -> s {iamRole = a} :: CreateScheduledAction)
{-# DEPRECATED csaIAMRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

instance Lude.AWSRequest CreateScheduledAction where
  type Rs CreateScheduledAction = ScheduledAction
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CreateScheduledActionResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateScheduledAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateScheduledAction where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateScheduledAction where
  toQuery CreateScheduledAction' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateScheduledAction" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "StartTime" Lude.=: startTime,
        "ScheduledActionDescription" Lude.=: scheduledActionDescription,
        "Enable" Lude.=: enable,
        "EndTime" Lude.=: endTime,
        "ScheduledActionName" Lude.=: scheduledActionName,
        "TargetAction" Lude.=: targetAction,
        "Schedule" Lude.=: schedule,
        "IamRole" Lude.=: iamRole
      ]
