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
-- Module      : Amazonka.Connect.Types.RuleAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RuleAction where

import Amazonka.Connect.Types.ActionType
import Amazonka.Connect.Types.AssignContactCategoryActionDefinition
import Amazonka.Connect.Types.EventBridgeActionDefinition
import Amazonka.Connect.Types.SendNotificationActionDefinition
import Amazonka.Connect.Types.TaskActionDefinition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the action to be performed when a rule is triggered.
--
-- /See:/ 'newRuleAction' smart constructor.
data RuleAction = RuleAction'
  { -- | Information about the contact category action.
    assignContactCategoryAction :: Prelude.Maybe AssignContactCategoryActionDefinition,
    -- | Information about the EventBridge action.
    eventBridgeAction :: Prelude.Maybe EventBridgeActionDefinition,
    -- | Information about the send notification action.
    sendNotificationAction :: Prelude.Maybe SendNotificationActionDefinition,
    -- | Information about the task action. This field is required if
    -- @TriggerEventSource@ is one of the following values:
    -- @OnZendeskTicketCreate@ | @OnZendeskTicketStatusUpdate@ |
    -- @OnSalesforceCaseCreate@
    taskAction :: Prelude.Maybe TaskActionDefinition,
    -- | The type of action that creates a rule.
    actionType :: ActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignContactCategoryAction', 'ruleAction_assignContactCategoryAction' - Information about the contact category action.
--
-- 'eventBridgeAction', 'ruleAction_eventBridgeAction' - Information about the EventBridge action.
--
-- 'sendNotificationAction', 'ruleAction_sendNotificationAction' - Information about the send notification action.
--
-- 'taskAction', 'ruleAction_taskAction' - Information about the task action. This field is required if
-- @TriggerEventSource@ is one of the following values:
-- @OnZendeskTicketCreate@ | @OnZendeskTicketStatusUpdate@ |
-- @OnSalesforceCaseCreate@
--
-- 'actionType', 'ruleAction_actionType' - The type of action that creates a rule.
newRuleAction ::
  -- | 'actionType'
  ActionType ->
  RuleAction
newRuleAction pActionType_ =
  RuleAction'
    { assignContactCategoryAction =
        Prelude.Nothing,
      eventBridgeAction = Prelude.Nothing,
      sendNotificationAction = Prelude.Nothing,
      taskAction = Prelude.Nothing,
      actionType = pActionType_
    }

-- | Information about the contact category action.
ruleAction_assignContactCategoryAction :: Lens.Lens' RuleAction (Prelude.Maybe AssignContactCategoryActionDefinition)
ruleAction_assignContactCategoryAction = Lens.lens (\RuleAction' {assignContactCategoryAction} -> assignContactCategoryAction) (\s@RuleAction' {} a -> s {assignContactCategoryAction = a} :: RuleAction)

-- | Information about the EventBridge action.
ruleAction_eventBridgeAction :: Lens.Lens' RuleAction (Prelude.Maybe EventBridgeActionDefinition)
ruleAction_eventBridgeAction = Lens.lens (\RuleAction' {eventBridgeAction} -> eventBridgeAction) (\s@RuleAction' {} a -> s {eventBridgeAction = a} :: RuleAction)

-- | Information about the send notification action.
ruleAction_sendNotificationAction :: Lens.Lens' RuleAction (Prelude.Maybe SendNotificationActionDefinition)
ruleAction_sendNotificationAction = Lens.lens (\RuleAction' {sendNotificationAction} -> sendNotificationAction) (\s@RuleAction' {} a -> s {sendNotificationAction = a} :: RuleAction)

-- | Information about the task action. This field is required if
-- @TriggerEventSource@ is one of the following values:
-- @OnZendeskTicketCreate@ | @OnZendeskTicketStatusUpdate@ |
-- @OnSalesforceCaseCreate@
ruleAction_taskAction :: Lens.Lens' RuleAction (Prelude.Maybe TaskActionDefinition)
ruleAction_taskAction = Lens.lens (\RuleAction' {taskAction} -> taskAction) (\s@RuleAction' {} a -> s {taskAction = a} :: RuleAction)

-- | The type of action that creates a rule.
ruleAction_actionType :: Lens.Lens' RuleAction ActionType
ruleAction_actionType = Lens.lens (\RuleAction' {actionType} -> actionType) (\s@RuleAction' {} a -> s {actionType = a} :: RuleAction)

instance Data.FromJSON RuleAction where
  parseJSON =
    Data.withObject
      "RuleAction"
      ( \x ->
          RuleAction'
            Prelude.<$> (x Data..:? "AssignContactCategoryAction")
            Prelude.<*> (x Data..:? "EventBridgeAction")
            Prelude.<*> (x Data..:? "SendNotificationAction")
            Prelude.<*> (x Data..:? "TaskAction")
            Prelude.<*> (x Data..: "ActionType")
      )

instance Prelude.Hashable RuleAction where
  hashWithSalt _salt RuleAction' {..} =
    _salt
      `Prelude.hashWithSalt` assignContactCategoryAction
      `Prelude.hashWithSalt` eventBridgeAction
      `Prelude.hashWithSalt` sendNotificationAction
      `Prelude.hashWithSalt` taskAction
      `Prelude.hashWithSalt` actionType

instance Prelude.NFData RuleAction where
  rnf RuleAction' {..} =
    Prelude.rnf assignContactCategoryAction
      `Prelude.seq` Prelude.rnf eventBridgeAction
      `Prelude.seq` Prelude.rnf sendNotificationAction
      `Prelude.seq` Prelude.rnf taskAction
      `Prelude.seq` Prelude.rnf actionType

instance Data.ToJSON RuleAction where
  toJSON RuleAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssignContactCategoryAction" Data..=)
              Prelude.<$> assignContactCategoryAction,
            ("EventBridgeAction" Data..=)
              Prelude.<$> eventBridgeAction,
            ("SendNotificationAction" Data..=)
              Prelude.<$> sendNotificationAction,
            ("TaskAction" Data..=) Prelude.<$> taskAction,
            Prelude.Just ("ActionType" Data..= actionType)
          ]
      )
