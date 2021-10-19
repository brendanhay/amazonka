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
-- Module      : Network.AWS.Redshift.CreateScheduledAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled action. A scheduled action contains a schedule and
-- an Amazon Redshift API action. For example, you can create a schedule of
-- when to run the @ResizeCluster@ API operation.
module Network.AWS.Redshift.CreateScheduledAction
  ( -- * Creating a Request
    CreateScheduledAction (..),
    newCreateScheduledAction,

    -- * Request Lenses
    createScheduledAction_startTime,
    createScheduledAction_scheduledActionDescription,
    createScheduledAction_enable,
    createScheduledAction_endTime,
    createScheduledAction_scheduledActionName,
    createScheduledAction_targetAction,
    createScheduledAction_schedule,
    createScheduledAction_iamRole,

    -- * Destructuring the Response
    ScheduledAction (..),
    newScheduledAction,

    -- * Response Lenses
    scheduledAction_state,
    scheduledAction_targetAction,
    scheduledAction_startTime,
    scheduledAction_schedule,
    scheduledAction_scheduledActionName,
    scheduledAction_scheduledActionDescription,
    scheduledAction_nextInvocations,
    scheduledAction_endTime,
    scheduledAction_iamRole,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateScheduledAction' smart constructor.
data CreateScheduledAction = CreateScheduledAction'
  { -- | The start time in UTC of the scheduled action. Before this time, the
    -- scheduled action does not trigger. For more information about this
    -- parameter, see ScheduledAction.
    startTime :: Prelude.Maybe Core.ISO8601,
    -- | The description of the scheduled action.
    scheduledActionDescription :: Prelude.Maybe Prelude.Text,
    -- | If true, the schedule is enabled. If false, the scheduled action does
    -- not trigger. For more information about @state@ of the scheduled action,
    -- see ScheduledAction.
    enable :: Prelude.Maybe Prelude.Bool,
    -- | The end time in UTC of the scheduled action. After this time, the
    -- scheduled action does not trigger. For more information about this
    -- parameter, see ScheduledAction.
    endTime :: Prelude.Maybe Core.ISO8601,
    -- | The name of the scheduled action. The name must be unique within an
    -- account. For more information about this parameter, see ScheduledAction.
    scheduledActionName :: Prelude.Text,
    -- | A JSON format string of the Amazon Redshift API operation with input
    -- parameters. For more information about this parameter, see
    -- ScheduledAction.
    targetAction :: ScheduledActionType,
    -- | The schedule in @at( )@ or @cron( )@ format. For more information about
    -- this parameter, see ScheduledAction.
    schedule :: Prelude.Text,
    -- | The IAM role to assume to run the target action. For more information
    -- about this parameter, see ScheduledAction.
    iamRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScheduledAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'createScheduledAction_startTime' - The start time in UTC of the scheduled action. Before this time, the
-- scheduled action does not trigger. For more information about this
-- parameter, see ScheduledAction.
--
-- 'scheduledActionDescription', 'createScheduledAction_scheduledActionDescription' - The description of the scheduled action.
--
-- 'enable', 'createScheduledAction_enable' - If true, the schedule is enabled. If false, the scheduled action does
-- not trigger. For more information about @state@ of the scheduled action,
-- see ScheduledAction.
--
-- 'endTime', 'createScheduledAction_endTime' - The end time in UTC of the scheduled action. After this time, the
-- scheduled action does not trigger. For more information about this
-- parameter, see ScheduledAction.
--
-- 'scheduledActionName', 'createScheduledAction_scheduledActionName' - The name of the scheduled action. The name must be unique within an
-- account. For more information about this parameter, see ScheduledAction.
--
-- 'targetAction', 'createScheduledAction_targetAction' - A JSON format string of the Amazon Redshift API operation with input
-- parameters. For more information about this parameter, see
-- ScheduledAction.
--
-- 'schedule', 'createScheduledAction_schedule' - The schedule in @at( )@ or @cron( )@ format. For more information about
-- this parameter, see ScheduledAction.
--
-- 'iamRole', 'createScheduledAction_iamRole' - The IAM role to assume to run the target action. For more information
-- about this parameter, see ScheduledAction.
newCreateScheduledAction ::
  -- | 'scheduledActionName'
  Prelude.Text ->
  -- | 'targetAction'
  ScheduledActionType ->
  -- | 'schedule'
  Prelude.Text ->
  -- | 'iamRole'
  Prelude.Text ->
  CreateScheduledAction
newCreateScheduledAction
  pScheduledActionName_
  pTargetAction_
  pSchedule_
  pIamRole_ =
    CreateScheduledAction'
      { startTime = Prelude.Nothing,
        scheduledActionDescription = Prelude.Nothing,
        enable = Prelude.Nothing,
        endTime = Prelude.Nothing,
        scheduledActionName = pScheduledActionName_,
        targetAction = pTargetAction_,
        schedule = pSchedule_,
        iamRole = pIamRole_
      }

-- | The start time in UTC of the scheduled action. Before this time, the
-- scheduled action does not trigger. For more information about this
-- parameter, see ScheduledAction.
createScheduledAction_startTime :: Lens.Lens' CreateScheduledAction (Prelude.Maybe Prelude.UTCTime)
createScheduledAction_startTime = Lens.lens (\CreateScheduledAction' {startTime} -> startTime) (\s@CreateScheduledAction' {} a -> s {startTime = a} :: CreateScheduledAction) Prelude.. Lens.mapping Core._Time

-- | The description of the scheduled action.
createScheduledAction_scheduledActionDescription :: Lens.Lens' CreateScheduledAction (Prelude.Maybe Prelude.Text)
createScheduledAction_scheduledActionDescription = Lens.lens (\CreateScheduledAction' {scheduledActionDescription} -> scheduledActionDescription) (\s@CreateScheduledAction' {} a -> s {scheduledActionDescription = a} :: CreateScheduledAction)

-- | If true, the schedule is enabled. If false, the scheduled action does
-- not trigger. For more information about @state@ of the scheduled action,
-- see ScheduledAction.
createScheduledAction_enable :: Lens.Lens' CreateScheduledAction (Prelude.Maybe Prelude.Bool)
createScheduledAction_enable = Lens.lens (\CreateScheduledAction' {enable} -> enable) (\s@CreateScheduledAction' {} a -> s {enable = a} :: CreateScheduledAction)

-- | The end time in UTC of the scheduled action. After this time, the
-- scheduled action does not trigger. For more information about this
-- parameter, see ScheduledAction.
createScheduledAction_endTime :: Lens.Lens' CreateScheduledAction (Prelude.Maybe Prelude.UTCTime)
createScheduledAction_endTime = Lens.lens (\CreateScheduledAction' {endTime} -> endTime) (\s@CreateScheduledAction' {} a -> s {endTime = a} :: CreateScheduledAction) Prelude.. Lens.mapping Core._Time

-- | The name of the scheduled action. The name must be unique within an
-- account. For more information about this parameter, see ScheduledAction.
createScheduledAction_scheduledActionName :: Lens.Lens' CreateScheduledAction Prelude.Text
createScheduledAction_scheduledActionName = Lens.lens (\CreateScheduledAction' {scheduledActionName} -> scheduledActionName) (\s@CreateScheduledAction' {} a -> s {scheduledActionName = a} :: CreateScheduledAction)

-- | A JSON format string of the Amazon Redshift API operation with input
-- parameters. For more information about this parameter, see
-- ScheduledAction.
createScheduledAction_targetAction :: Lens.Lens' CreateScheduledAction ScheduledActionType
createScheduledAction_targetAction = Lens.lens (\CreateScheduledAction' {targetAction} -> targetAction) (\s@CreateScheduledAction' {} a -> s {targetAction = a} :: CreateScheduledAction)

-- | The schedule in @at( )@ or @cron( )@ format. For more information about
-- this parameter, see ScheduledAction.
createScheduledAction_schedule :: Lens.Lens' CreateScheduledAction Prelude.Text
createScheduledAction_schedule = Lens.lens (\CreateScheduledAction' {schedule} -> schedule) (\s@CreateScheduledAction' {} a -> s {schedule = a} :: CreateScheduledAction)

-- | The IAM role to assume to run the target action. For more information
-- about this parameter, see ScheduledAction.
createScheduledAction_iamRole :: Lens.Lens' CreateScheduledAction Prelude.Text
createScheduledAction_iamRole = Lens.lens (\CreateScheduledAction' {iamRole} -> iamRole) (\s@CreateScheduledAction' {} a -> s {iamRole = a} :: CreateScheduledAction)

instance Core.AWSRequest CreateScheduledAction where
  type
    AWSResponse CreateScheduledAction =
      ScheduledAction
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateScheduledActionResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable CreateScheduledAction

instance Prelude.NFData CreateScheduledAction

instance Core.ToHeaders CreateScheduledAction where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateScheduledAction where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateScheduledAction where
  toQuery CreateScheduledAction' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateScheduledAction" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "StartTime" Core.=: startTime,
        "ScheduledActionDescription"
          Core.=: scheduledActionDescription,
        "Enable" Core.=: enable,
        "EndTime" Core.=: endTime,
        "ScheduledActionName" Core.=: scheduledActionName,
        "TargetAction" Core.=: targetAction,
        "Schedule" Core.=: schedule,
        "IamRole" Core.=: iamRole
      ]
