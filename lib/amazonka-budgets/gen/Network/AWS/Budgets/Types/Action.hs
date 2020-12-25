{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Action
  ( Action (..),

    -- * Smart constructor
    mkAction,

    -- * Lenses
    aActionId,
    aBudgetName,
    aNotificationType,
    aActionType,
    aActionThreshold,
    aDefinition,
    aExecutionRoleArn,
    aApprovalModel,
    aStatus,
    aSubscribers,
  )
where

import qualified Network.AWS.Budgets.Types.ActionId as Types
import qualified Network.AWS.Budgets.Types.ActionStatus as Types
import qualified Network.AWS.Budgets.Types.ActionThreshold as Types
import qualified Network.AWS.Budgets.Types.ActionType as Types
import qualified Network.AWS.Budgets.Types.ApprovalModel as Types
import qualified Network.AWS.Budgets.Types.BudgetName as Types
import qualified Network.AWS.Budgets.Types.Definition as Types
import qualified Network.AWS.Budgets.Types.ExecutionRoleArn as Types
import qualified Network.AWS.Budgets.Types.NotificationType as Types
import qualified Network.AWS.Budgets.Types.Subscriber as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A budget action resource.
--
-- /See:/ 'mkAction' smart constructor.
data Action = Action'
  { -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Types.ActionId,
    budgetName :: Types.BudgetName,
    notificationType :: Types.NotificationType,
    -- | The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
    actionType :: Types.ActionType,
    -- | The trigger threshold of the action.
    actionThreshold :: Types.ActionThreshold,
    -- | Where you specify all of the type-specific parameters.
    definition :: Types.Definition,
    -- | The role passed for action execution and reversion. Roles and actions must be in the same account.
    executionRoleArn :: Types.ExecutionRoleArn,
    -- | This specifies if the action needs manual or automatic approval.
    approvalModel :: Types.ApprovalModel,
    -- | The status of action.
    status :: Types.ActionStatus,
    subscribers :: Core.NonEmpty Types.Subscriber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Action' value with any optional fields omitted.
mkAction ::
  -- | 'actionId'
  Types.ActionId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'notificationType'
  Types.NotificationType ->
  -- | 'actionType'
  Types.ActionType ->
  -- | 'actionThreshold'
  Types.ActionThreshold ->
  -- | 'definition'
  Types.Definition ->
  -- | 'executionRoleArn'
  Types.ExecutionRoleArn ->
  -- | 'approvalModel'
  Types.ApprovalModel ->
  -- | 'status'
  Types.ActionStatus ->
  -- | 'subscribers'
  Core.NonEmpty Types.Subscriber ->
  Action
mkAction
  actionId
  budgetName
  notificationType
  actionType
  actionThreshold
  definition
  executionRoleArn
  approvalModel
  status
  subscribers =
    Action'
      { actionId,
        budgetName,
        notificationType,
        actionType,
        actionThreshold,
        definition,
        executionRoleArn,
        approvalModel,
        status,
        subscribers
      }

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActionId :: Lens.Lens' Action Types.ActionId
aActionId = Lens.field @"actionId"
{-# DEPRECATED aActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBudgetName :: Lens.Lens' Action Types.BudgetName
aBudgetName = Lens.field @"budgetName"
{-# DEPRECATED aBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNotificationType :: Lens.Lens' Action Types.NotificationType
aNotificationType = Lens.field @"notificationType"
{-# DEPRECATED aNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActionType :: Lens.Lens' Action Types.ActionType
aActionType = Lens.field @"actionType"
{-# DEPRECATED aActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | The trigger threshold of the action.
--
-- /Note:/ Consider using 'actionThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActionThreshold :: Lens.Lens' Action Types.ActionThreshold
aActionThreshold = Lens.field @"actionThreshold"
{-# DEPRECATED aActionThreshold "Use generic-lens or generic-optics with 'actionThreshold' instead." #-}

-- | Where you specify all of the type-specific parameters.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDefinition :: Lens.Lens' Action Types.Definition
aDefinition = Lens.field @"definition"
{-# DEPRECATED aDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The role passed for action execution and reversion. Roles and actions must be in the same account.
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aExecutionRoleArn :: Lens.Lens' Action Types.ExecutionRoleArn
aExecutionRoleArn = Lens.field @"executionRoleArn"
{-# DEPRECATED aExecutionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead." #-}

-- | This specifies if the action needs manual or automatic approval.
--
-- /Note:/ Consider using 'approvalModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aApprovalModel :: Lens.Lens' Action Types.ApprovalModel
aApprovalModel = Lens.field @"approvalModel"
{-# DEPRECATED aApprovalModel "Use generic-lens or generic-optics with 'approvalModel' instead." #-}

-- | The status of action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatus :: Lens.Lens' Action Types.ActionStatus
aStatus = Lens.field @"status"
{-# DEPRECATED aStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSubscribers :: Lens.Lens' Action (Core.NonEmpty Types.Subscriber)
aSubscribers = Lens.field @"subscribers"
{-# DEPRECATED aSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject "Action" Core.$
      \x ->
        Action'
          Core.<$> (x Core..: "ActionId")
          Core.<*> (x Core..: "BudgetName")
          Core.<*> (x Core..: "NotificationType")
          Core.<*> (x Core..: "ActionType")
          Core.<*> (x Core..: "ActionThreshold")
          Core.<*> (x Core..: "Definition")
          Core.<*> (x Core..: "ExecutionRoleArn")
          Core.<*> (x Core..: "ApprovalModel")
          Core.<*> (x Core..: "Status")
          Core.<*> (x Core..: "Subscribers")
