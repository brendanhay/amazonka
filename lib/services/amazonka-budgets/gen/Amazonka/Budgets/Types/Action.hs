{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Budgets.Types.Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.Action where

import Amazonka.Budgets.Types.ActionStatus
import Amazonka.Budgets.Types.ActionThreshold
import Amazonka.Budgets.Types.ActionType
import Amazonka.Budgets.Types.ApprovalModel
import Amazonka.Budgets.Types.Definition
import Amazonka.Budgets.Types.NotificationType
import Amazonka.Budgets.Types.Subscriber
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A budget action resource.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Prelude.Text,
    budgetName :: Prelude.Text,
    notificationType :: NotificationType,
    -- | The type of action. This defines the type of tasks that can be carried
    -- out by this action. This field also determines the format for
    -- definition.
    actionType :: ActionType,
    -- | The trigger threshold of the action.
    actionThreshold :: ActionThreshold,
    -- | Where you specify all of the type-specific parameters.
    definition :: Definition,
    -- | The role passed for action execution and reversion. Roles and actions
    -- must be in the same account.
    executionRoleArn :: Prelude.Text,
    -- | This specifies if the action needs manual or automatic approval.
    approvalModel :: ApprovalModel,
    -- | The status of the action.
    status :: ActionStatus,
    subscribers :: Prelude.NonEmpty Subscriber
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionId', 'action_actionId' - A system-generated universally unique identifier (UUID) for the action.
--
-- 'budgetName', 'action_budgetName' - Undocumented member.
--
-- 'notificationType', 'action_notificationType' - Undocumented member.
--
-- 'actionType', 'action_actionType' - The type of action. This defines the type of tasks that can be carried
-- out by this action. This field also determines the format for
-- definition.
--
-- 'actionThreshold', 'action_actionThreshold' - The trigger threshold of the action.
--
-- 'definition', 'action_definition' - Where you specify all of the type-specific parameters.
--
-- 'executionRoleArn', 'action_executionRoleArn' - The role passed for action execution and reversion. Roles and actions
-- must be in the same account.
--
-- 'approvalModel', 'action_approvalModel' - This specifies if the action needs manual or automatic approval.
--
-- 'status', 'action_status' - The status of the action.
--
-- 'subscribers', 'action_subscribers' - Undocumented member.
newAction ::
  -- | 'actionId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'notificationType'
  NotificationType ->
  -- | 'actionType'
  ActionType ->
  -- | 'actionThreshold'
  ActionThreshold ->
  -- | 'definition'
  Definition ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'approvalModel'
  ApprovalModel ->
  -- | 'status'
  ActionStatus ->
  -- | 'subscribers'
  Prelude.NonEmpty Subscriber ->
  Action
newAction
  pActionId_
  pBudgetName_
  pNotificationType_
  pActionType_
  pActionThreshold_
  pDefinition_
  pExecutionRoleArn_
  pApprovalModel_
  pStatus_
  pSubscribers_ =
    Action'
      { actionId = pActionId_,
        budgetName = pBudgetName_,
        notificationType = pNotificationType_,
        actionType = pActionType_,
        actionThreshold = pActionThreshold_,
        definition = pDefinition_,
        executionRoleArn = pExecutionRoleArn_,
        approvalModel = pApprovalModel_,
        status = pStatus_,
        subscribers = Lens.coerced Lens.# pSubscribers_
      }

-- | A system-generated universally unique identifier (UUID) for the action.
action_actionId :: Lens.Lens' Action Prelude.Text
action_actionId = Lens.lens (\Action' {actionId} -> actionId) (\s@Action' {} a -> s {actionId = a} :: Action)

-- | Undocumented member.
action_budgetName :: Lens.Lens' Action Prelude.Text
action_budgetName = Lens.lens (\Action' {budgetName} -> budgetName) (\s@Action' {} a -> s {budgetName = a} :: Action)

-- | Undocumented member.
action_notificationType :: Lens.Lens' Action NotificationType
action_notificationType = Lens.lens (\Action' {notificationType} -> notificationType) (\s@Action' {} a -> s {notificationType = a} :: Action)

-- | The type of action. This defines the type of tasks that can be carried
-- out by this action. This field also determines the format for
-- definition.
action_actionType :: Lens.Lens' Action ActionType
action_actionType = Lens.lens (\Action' {actionType} -> actionType) (\s@Action' {} a -> s {actionType = a} :: Action)

-- | The trigger threshold of the action.
action_actionThreshold :: Lens.Lens' Action ActionThreshold
action_actionThreshold = Lens.lens (\Action' {actionThreshold} -> actionThreshold) (\s@Action' {} a -> s {actionThreshold = a} :: Action)

-- | Where you specify all of the type-specific parameters.
action_definition :: Lens.Lens' Action Definition
action_definition = Lens.lens (\Action' {definition} -> definition) (\s@Action' {} a -> s {definition = a} :: Action)

-- | The role passed for action execution and reversion. Roles and actions
-- must be in the same account.
action_executionRoleArn :: Lens.Lens' Action Prelude.Text
action_executionRoleArn = Lens.lens (\Action' {executionRoleArn} -> executionRoleArn) (\s@Action' {} a -> s {executionRoleArn = a} :: Action)

-- | This specifies if the action needs manual or automatic approval.
action_approvalModel :: Lens.Lens' Action ApprovalModel
action_approvalModel = Lens.lens (\Action' {approvalModel} -> approvalModel) (\s@Action' {} a -> s {approvalModel = a} :: Action)

-- | The status of the action.
action_status :: Lens.Lens' Action ActionStatus
action_status = Lens.lens (\Action' {status} -> status) (\s@Action' {} a -> s {status = a} :: Action)

-- | Undocumented member.
action_subscribers :: Lens.Lens' Action (Prelude.NonEmpty Subscriber)
action_subscribers = Lens.lens (\Action' {subscribers} -> subscribers) (\s@Action' {} a -> s {subscribers = a} :: Action) Prelude.. Lens.coerced

instance Data.FromJSON Action where
  parseJSON =
    Data.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Data..: "ActionId")
            Prelude.<*> (x Data..: "BudgetName")
            Prelude.<*> (x Data..: "NotificationType")
            Prelude.<*> (x Data..: "ActionType")
            Prelude.<*> (x Data..: "ActionThreshold")
            Prelude.<*> (x Data..: "Definition")
            Prelude.<*> (x Data..: "ExecutionRoleArn")
            Prelude.<*> (x Data..: "ApprovalModel")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "Subscribers")
      )

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt
      `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` notificationType
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` actionThreshold
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` approvalModel
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subscribers

instance Prelude.NFData Action where
  rnf Action' {..} =
    Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf notificationType
      `Prelude.seq` Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf actionThreshold
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf approvalModel
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subscribers
