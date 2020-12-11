{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a scheduled action.
module Network.AWS.Redshift.ModifyScheduledAction
  ( -- * Creating a request
    ModifyScheduledAction (..),
    mkModifyScheduledAction,

    -- ** Request lenses
    msaTargetAction,
    msaStartTime,
    msaSchedule,
    msaScheduledActionDescription,
    msaEnable,
    msaEndTime,
    msaIAMRole,
    msaScheduledActionName,

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

-- | /See:/ 'mkModifyScheduledAction' smart constructor.
data ModifyScheduledAction = ModifyScheduledAction'
  { targetAction ::
      Lude.Maybe ScheduledActionType,
    startTime :: Lude.Maybe Lude.ISO8601,
    schedule :: Lude.Maybe Lude.Text,
    scheduledActionDescription ::
      Lude.Maybe Lude.Text,
    enable :: Lude.Maybe Lude.Bool,
    endTime :: Lude.Maybe Lude.ISO8601,
    iamRole :: Lude.Maybe Lude.Text,
    scheduledActionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyScheduledAction' with the minimum fields required to make a request.
--
-- * 'enable' - A modified enable flag of the scheduled action. If true, the scheduled action is active. If false, the scheduled action is disabled.
-- * 'endTime' - A modified end time of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
-- * 'iamRole' - A different IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
-- * 'schedule' - A modified schedule in either @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
-- * 'scheduledActionDescription' - A modified description of the scheduled action.
-- * 'scheduledActionName' - The name of the scheduled action to modify.
-- * 'startTime' - A modified start time of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
-- * 'targetAction' - A modified JSON format of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
mkModifyScheduledAction ::
  -- | 'scheduledActionName'
  Lude.Text ->
  ModifyScheduledAction
mkModifyScheduledAction pScheduledActionName_ =
  ModifyScheduledAction'
    { targetAction = Lude.Nothing,
      startTime = Lude.Nothing,
      schedule = Lude.Nothing,
      scheduledActionDescription = Lude.Nothing,
      enable = Lude.Nothing,
      endTime = Lude.Nothing,
      iamRole = Lude.Nothing,
      scheduledActionName = pScheduledActionName_
    }

-- | A modified JSON format of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'targetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaTargetAction :: Lens.Lens' ModifyScheduledAction (Lude.Maybe ScheduledActionType)
msaTargetAction = Lens.lens (targetAction :: ModifyScheduledAction -> Lude.Maybe ScheduledActionType) (\s a -> s {targetAction = a} :: ModifyScheduledAction)
{-# DEPRECATED msaTargetAction "Use generic-lens or generic-optics with 'targetAction' instead." #-}

-- | A modified start time of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaStartTime :: Lens.Lens' ModifyScheduledAction (Lude.Maybe Lude.ISO8601)
msaStartTime = Lens.lens (startTime :: ModifyScheduledAction -> Lude.Maybe Lude.ISO8601) (\s a -> s {startTime = a} :: ModifyScheduledAction)
{-# DEPRECATED msaStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A modified schedule in either @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaSchedule :: Lens.Lens' ModifyScheduledAction (Lude.Maybe Lude.Text)
msaSchedule = Lens.lens (schedule :: ModifyScheduledAction -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: ModifyScheduledAction)
{-# DEPRECATED msaSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | A modified description of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaScheduledActionDescription :: Lens.Lens' ModifyScheduledAction (Lude.Maybe Lude.Text)
msaScheduledActionDescription = Lens.lens (scheduledActionDescription :: ModifyScheduledAction -> Lude.Maybe Lude.Text) (\s a -> s {scheduledActionDescription = a} :: ModifyScheduledAction)
{-# DEPRECATED msaScheduledActionDescription "Use generic-lens or generic-optics with 'scheduledActionDescription' instead." #-}

-- | A modified enable flag of the scheduled action. If true, the scheduled action is active. If false, the scheduled action is disabled.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaEnable :: Lens.Lens' ModifyScheduledAction (Lude.Maybe Lude.Bool)
msaEnable = Lens.lens (enable :: ModifyScheduledAction -> Lude.Maybe Lude.Bool) (\s a -> s {enable = a} :: ModifyScheduledAction)
{-# DEPRECATED msaEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | A modified end time of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaEndTime :: Lens.Lens' ModifyScheduledAction (Lude.Maybe Lude.ISO8601)
msaEndTime = Lens.lens (endTime :: ModifyScheduledAction -> Lude.Maybe Lude.ISO8601) (\s a -> s {endTime = a} :: ModifyScheduledAction)
{-# DEPRECATED msaEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | A different IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaIAMRole :: Lens.Lens' ModifyScheduledAction (Lude.Maybe Lude.Text)
msaIAMRole = Lens.lens (iamRole :: ModifyScheduledAction -> Lude.Maybe Lude.Text) (\s a -> s {iamRole = a} :: ModifyScheduledAction)
{-# DEPRECATED msaIAMRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

-- | The name of the scheduled action to modify.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaScheduledActionName :: Lens.Lens' ModifyScheduledAction Lude.Text
msaScheduledActionName = Lens.lens (scheduledActionName :: ModifyScheduledAction -> Lude.Text) (\s a -> s {scheduledActionName = a} :: ModifyScheduledAction)
{-# DEPRECATED msaScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

instance Lude.AWSRequest ModifyScheduledAction where
  type Rs ModifyScheduledAction = ScheduledAction
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifyScheduledActionResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ModifyScheduledAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyScheduledAction where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyScheduledAction where
  toQuery ModifyScheduledAction' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyScheduledAction" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TargetAction" Lude.=: targetAction,
        "StartTime" Lude.=: startTime,
        "Schedule" Lude.=: schedule,
        "ScheduledActionDescription" Lude.=: scheduledActionDescription,
        "Enable" Lude.=: enable,
        "EndTime" Lude.=: endTime,
        "IamRole" Lude.=: iamRole,
        "ScheduledActionName" Lude.=: scheduledActionName
      ]
