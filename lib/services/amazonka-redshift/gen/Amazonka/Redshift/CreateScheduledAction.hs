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
-- Module      : Amazonka.Redshift.CreateScheduledAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled action. A scheduled action contains a schedule and
-- an Amazon Redshift API action. For example, you can create a schedule of
-- when to run the @ResizeCluster@ API operation.
module Amazonka.Redshift.CreateScheduledAction
  ( -- * Creating a Request
    CreateScheduledAction (..),
    newCreateScheduledAction,

    -- * Request Lenses
    createScheduledAction_enable,
    createScheduledAction_endTime,
    createScheduledAction_scheduledActionDescription,
    createScheduledAction_startTime,
    createScheduledAction_scheduledActionName,
    createScheduledAction_targetAction,
    createScheduledAction_schedule,
    createScheduledAction_iamRole,

    -- * Destructuring the Response
    ScheduledAction (..),
    newScheduledAction,

    -- * Response Lenses
    scheduledAction_endTime,
    scheduledAction_iamRole,
    scheduledAction_nextInvocations,
    scheduledAction_schedule,
    scheduledAction_scheduledActionDescription,
    scheduledAction_scheduledActionName,
    scheduledAction_startTime,
    scheduledAction_state,
    scheduledAction_targetAction,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateScheduledAction' smart constructor.
data CreateScheduledAction = CreateScheduledAction'
  { -- | If true, the schedule is enabled. If false, the scheduled action does
    -- not trigger. For more information about @state@ of the scheduled action,
    -- see ScheduledAction.
    enable :: Prelude.Maybe Prelude.Bool,
    -- | The end time in UTC of the scheduled action. After this time, the
    -- scheduled action does not trigger. For more information about this
    -- parameter, see ScheduledAction.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The description of the scheduled action.
    scheduledActionDescription :: Prelude.Maybe Prelude.Text,
    -- | The start time in UTC of the scheduled action. Before this time, the
    -- scheduled action does not trigger. For more information about this
    -- parameter, see ScheduledAction.
    startTime :: Prelude.Maybe Data.ISO8601,
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
-- 'enable', 'createScheduledAction_enable' - If true, the schedule is enabled. If false, the scheduled action does
-- not trigger. For more information about @state@ of the scheduled action,
-- see ScheduledAction.
--
-- 'endTime', 'createScheduledAction_endTime' - The end time in UTC of the scheduled action. After this time, the
-- scheduled action does not trigger. For more information about this
-- parameter, see ScheduledAction.
--
-- 'scheduledActionDescription', 'createScheduledAction_scheduledActionDescription' - The description of the scheduled action.
--
-- 'startTime', 'createScheduledAction_startTime' - The start time in UTC of the scheduled action. Before this time, the
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
      { enable = Prelude.Nothing,
        endTime = Prelude.Nothing,
        scheduledActionDescription = Prelude.Nothing,
        startTime = Prelude.Nothing,
        scheduledActionName = pScheduledActionName_,
        targetAction = pTargetAction_,
        schedule = pSchedule_,
        iamRole = pIamRole_
      }

-- | If true, the schedule is enabled. If false, the scheduled action does
-- not trigger. For more information about @state@ of the scheduled action,
-- see ScheduledAction.
createScheduledAction_enable :: Lens.Lens' CreateScheduledAction (Prelude.Maybe Prelude.Bool)
createScheduledAction_enable = Lens.lens (\CreateScheduledAction' {enable} -> enable) (\s@CreateScheduledAction' {} a -> s {enable = a} :: CreateScheduledAction)

-- | The end time in UTC of the scheduled action. After this time, the
-- scheduled action does not trigger. For more information about this
-- parameter, see ScheduledAction.
createScheduledAction_endTime :: Lens.Lens' CreateScheduledAction (Prelude.Maybe Prelude.UTCTime)
createScheduledAction_endTime = Lens.lens (\CreateScheduledAction' {endTime} -> endTime) (\s@CreateScheduledAction' {} a -> s {endTime = a} :: CreateScheduledAction) Prelude.. Lens.mapping Data._Time

-- | The description of the scheduled action.
createScheduledAction_scheduledActionDescription :: Lens.Lens' CreateScheduledAction (Prelude.Maybe Prelude.Text)
createScheduledAction_scheduledActionDescription = Lens.lens (\CreateScheduledAction' {scheduledActionDescription} -> scheduledActionDescription) (\s@CreateScheduledAction' {} a -> s {scheduledActionDescription = a} :: CreateScheduledAction)

-- | The start time in UTC of the scheduled action. Before this time, the
-- scheduled action does not trigger. For more information about this
-- parameter, see ScheduledAction.
createScheduledAction_startTime :: Lens.Lens' CreateScheduledAction (Prelude.Maybe Prelude.UTCTime)
createScheduledAction_startTime = Lens.lens (\CreateScheduledAction' {startTime} -> startTime) (\s@CreateScheduledAction' {} a -> s {startTime = a} :: CreateScheduledAction) Prelude.. Lens.mapping Data._Time

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateScheduledActionResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable CreateScheduledAction where
  hashWithSalt _salt CreateScheduledAction' {..} =
    _salt `Prelude.hashWithSalt` enable
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` scheduledActionDescription
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` scheduledActionName
      `Prelude.hashWithSalt` targetAction
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` iamRole

instance Prelude.NFData CreateScheduledAction where
  rnf CreateScheduledAction' {..} =
    Prelude.rnf enable
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf scheduledActionDescription
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf scheduledActionName
      `Prelude.seq` Prelude.rnf targetAction
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf iamRole

instance Data.ToHeaders CreateScheduledAction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateScheduledAction where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateScheduledAction where
  toQuery CreateScheduledAction' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateScheduledAction" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Enable" Data.=: enable,
        "EndTime" Data.=: endTime,
        "ScheduledActionDescription"
          Data.=: scheduledActionDescription,
        "StartTime" Data.=: startTime,
        "ScheduledActionName" Data.=: scheduledActionName,
        "TargetAction" Data.=: targetAction,
        "Schedule" Data.=: schedule,
        "IamRole" Data.=: iamRole
      ]
