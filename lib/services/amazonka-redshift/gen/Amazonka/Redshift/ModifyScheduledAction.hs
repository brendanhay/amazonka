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
-- Module      : Amazonka.Redshift.ModifyScheduledAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a scheduled action.
module Amazonka.Redshift.ModifyScheduledAction
  ( -- * Creating a Request
    ModifyScheduledAction (..),
    newModifyScheduledAction,

    -- * Request Lenses
    modifyScheduledAction_targetAction,
    modifyScheduledAction_startTime,
    modifyScheduledAction_schedule,
    modifyScheduledAction_scheduledActionDescription,
    modifyScheduledAction_enable,
    modifyScheduledAction_endTime,
    modifyScheduledAction_iamRole,
    modifyScheduledAction_scheduledActionName,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyScheduledAction' smart constructor.
data ModifyScheduledAction = ModifyScheduledAction'
  { -- | A modified JSON format of the scheduled action. For more information
    -- about this parameter, see ScheduledAction.
    targetAction :: Prelude.Maybe ScheduledActionType,
    -- | A modified start time of the scheduled action. For more information
    -- about this parameter, see ScheduledAction.
    startTime :: Prelude.Maybe Core.ISO8601,
    -- | A modified schedule in either @at( )@ or @cron( )@ format. For more
    -- information about this parameter, see ScheduledAction.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | A modified description of the scheduled action.
    scheduledActionDescription :: Prelude.Maybe Prelude.Text,
    -- | A modified enable flag of the scheduled action. If true, the scheduled
    -- action is active. If false, the scheduled action is disabled.
    enable :: Prelude.Maybe Prelude.Bool,
    -- | A modified end time of the scheduled action. For more information about
    -- this parameter, see ScheduledAction.
    endTime :: Prelude.Maybe Core.ISO8601,
    -- | A different IAM role to assume to run the target action. For more
    -- information about this parameter, see ScheduledAction.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the scheduled action to modify.
    scheduledActionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyScheduledAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetAction', 'modifyScheduledAction_targetAction' - A modified JSON format of the scheduled action. For more information
-- about this parameter, see ScheduledAction.
--
-- 'startTime', 'modifyScheduledAction_startTime' - A modified start time of the scheduled action. For more information
-- about this parameter, see ScheduledAction.
--
-- 'schedule', 'modifyScheduledAction_schedule' - A modified schedule in either @at( )@ or @cron( )@ format. For more
-- information about this parameter, see ScheduledAction.
--
-- 'scheduledActionDescription', 'modifyScheduledAction_scheduledActionDescription' - A modified description of the scheduled action.
--
-- 'enable', 'modifyScheduledAction_enable' - A modified enable flag of the scheduled action. If true, the scheduled
-- action is active. If false, the scheduled action is disabled.
--
-- 'endTime', 'modifyScheduledAction_endTime' - A modified end time of the scheduled action. For more information about
-- this parameter, see ScheduledAction.
--
-- 'iamRole', 'modifyScheduledAction_iamRole' - A different IAM role to assume to run the target action. For more
-- information about this parameter, see ScheduledAction.
--
-- 'scheduledActionName', 'modifyScheduledAction_scheduledActionName' - The name of the scheduled action to modify.
newModifyScheduledAction ::
  -- | 'scheduledActionName'
  Prelude.Text ->
  ModifyScheduledAction
newModifyScheduledAction pScheduledActionName_ =
  ModifyScheduledAction'
    { targetAction =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      schedule = Prelude.Nothing,
      scheduledActionDescription = Prelude.Nothing,
      enable = Prelude.Nothing,
      endTime = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      scheduledActionName = pScheduledActionName_
    }

-- | A modified JSON format of the scheduled action. For more information
-- about this parameter, see ScheduledAction.
modifyScheduledAction_targetAction :: Lens.Lens' ModifyScheduledAction (Prelude.Maybe ScheduledActionType)
modifyScheduledAction_targetAction = Lens.lens (\ModifyScheduledAction' {targetAction} -> targetAction) (\s@ModifyScheduledAction' {} a -> s {targetAction = a} :: ModifyScheduledAction)

-- | A modified start time of the scheduled action. For more information
-- about this parameter, see ScheduledAction.
modifyScheduledAction_startTime :: Lens.Lens' ModifyScheduledAction (Prelude.Maybe Prelude.UTCTime)
modifyScheduledAction_startTime = Lens.lens (\ModifyScheduledAction' {startTime} -> startTime) (\s@ModifyScheduledAction' {} a -> s {startTime = a} :: ModifyScheduledAction) Prelude.. Lens.mapping Core._Time

-- | A modified schedule in either @at( )@ or @cron( )@ format. For more
-- information about this parameter, see ScheduledAction.
modifyScheduledAction_schedule :: Lens.Lens' ModifyScheduledAction (Prelude.Maybe Prelude.Text)
modifyScheduledAction_schedule = Lens.lens (\ModifyScheduledAction' {schedule} -> schedule) (\s@ModifyScheduledAction' {} a -> s {schedule = a} :: ModifyScheduledAction)

-- | A modified description of the scheduled action.
modifyScheduledAction_scheduledActionDescription :: Lens.Lens' ModifyScheduledAction (Prelude.Maybe Prelude.Text)
modifyScheduledAction_scheduledActionDescription = Lens.lens (\ModifyScheduledAction' {scheduledActionDescription} -> scheduledActionDescription) (\s@ModifyScheduledAction' {} a -> s {scheduledActionDescription = a} :: ModifyScheduledAction)

-- | A modified enable flag of the scheduled action. If true, the scheduled
-- action is active. If false, the scheduled action is disabled.
modifyScheduledAction_enable :: Lens.Lens' ModifyScheduledAction (Prelude.Maybe Prelude.Bool)
modifyScheduledAction_enable = Lens.lens (\ModifyScheduledAction' {enable} -> enable) (\s@ModifyScheduledAction' {} a -> s {enable = a} :: ModifyScheduledAction)

-- | A modified end time of the scheduled action. For more information about
-- this parameter, see ScheduledAction.
modifyScheduledAction_endTime :: Lens.Lens' ModifyScheduledAction (Prelude.Maybe Prelude.UTCTime)
modifyScheduledAction_endTime = Lens.lens (\ModifyScheduledAction' {endTime} -> endTime) (\s@ModifyScheduledAction' {} a -> s {endTime = a} :: ModifyScheduledAction) Prelude.. Lens.mapping Core._Time

-- | A different IAM role to assume to run the target action. For more
-- information about this parameter, see ScheduledAction.
modifyScheduledAction_iamRole :: Lens.Lens' ModifyScheduledAction (Prelude.Maybe Prelude.Text)
modifyScheduledAction_iamRole = Lens.lens (\ModifyScheduledAction' {iamRole} -> iamRole) (\s@ModifyScheduledAction' {} a -> s {iamRole = a} :: ModifyScheduledAction)

-- | The name of the scheduled action to modify.
modifyScheduledAction_scheduledActionName :: Lens.Lens' ModifyScheduledAction Prelude.Text
modifyScheduledAction_scheduledActionName = Lens.lens (\ModifyScheduledAction' {scheduledActionName} -> scheduledActionName) (\s@ModifyScheduledAction' {} a -> s {scheduledActionName = a} :: ModifyScheduledAction)

instance Core.AWSRequest ModifyScheduledAction where
  type
    AWSResponse ModifyScheduledAction =
      ScheduledAction
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyScheduledActionResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable ModifyScheduledAction

instance Prelude.NFData ModifyScheduledAction

instance Core.ToHeaders ModifyScheduledAction where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyScheduledAction where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyScheduledAction where
  toQuery ModifyScheduledAction' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyScheduledAction" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "TargetAction" Core.=: targetAction,
        "StartTime" Core.=: startTime,
        "Schedule" Core.=: schedule,
        "ScheduledActionDescription"
          Core.=: scheduledActionDescription,
        "Enable" Core.=: enable,
        "EndTime" Core.=: endTime,
        "IamRole" Core.=: iamRole,
        "ScheduledActionName" Core.=: scheduledActionName
      ]
