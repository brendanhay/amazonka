{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.UpdateBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a budget action.
module Network.AWS.Budgets.UpdateBudgetAction
  ( -- * Creating a request
    UpdateBudgetAction (..),
    mkUpdateBudgetAction,

    -- ** Request lenses
    ubaAccountId,
    ubaBudgetName,
    ubaActionId,
    ubaActionThreshold,
    ubaApprovalModel,
    ubaDefinition,
    ubaExecutionRoleArn,
    ubaNotificationType,
    ubaSubscribers,

    -- * Destructuring the response
    UpdateBudgetActionResponse (..),
    mkUpdateBudgetActionResponse,

    -- ** Response lenses
    ubarrsAccountId,
    ubarrsBudgetName,
    ubarrsOldAction,
    ubarrsNewAction,
    ubarrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateBudgetAction' smart constructor.
data UpdateBudgetAction = UpdateBudgetAction'
  { accountId :: Types.AccountId,
    budgetName :: Types.BudgetName,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Types.ActionId,
    actionThreshold :: Core.Maybe Types.ActionThreshold,
    -- | This specifies if the action needs manual or automatic approval.
    approvalModel :: Core.Maybe Types.ApprovalModel,
    definition :: Core.Maybe Types.Definition,
    -- | The role passed for action execution and reversion. Roles and actions must be in the same account.
    executionRoleArn :: Core.Maybe Types.ExecutionRoleArn,
    notificationType :: Core.Maybe Types.NotificationType,
    subscribers :: Core.Maybe (Core.NonEmpty Types.Subscriber)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBudgetAction' value with any optional fields omitted.
mkUpdateBudgetAction ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'actionId'
  Types.ActionId ->
  UpdateBudgetAction
mkUpdateBudgetAction accountId budgetName actionId =
  UpdateBudgetAction'
    { accountId,
      budgetName,
      actionId,
      actionThreshold = Core.Nothing,
      approvalModel = Core.Nothing,
      definition = Core.Nothing,
      executionRoleArn = Core.Nothing,
      notificationType = Core.Nothing,
      subscribers = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaAccountId :: Lens.Lens' UpdateBudgetAction Types.AccountId
ubaAccountId = Lens.field @"accountId"
{-# DEPRECATED ubaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaBudgetName :: Lens.Lens' UpdateBudgetAction Types.BudgetName
ubaBudgetName = Lens.field @"budgetName"
{-# DEPRECATED ubaBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaActionId :: Lens.Lens' UpdateBudgetAction Types.ActionId
ubaActionId = Lens.field @"actionId"
{-# DEPRECATED ubaActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaActionThreshold :: Lens.Lens' UpdateBudgetAction (Core.Maybe Types.ActionThreshold)
ubaActionThreshold = Lens.field @"actionThreshold"
{-# DEPRECATED ubaActionThreshold "Use generic-lens or generic-optics with 'actionThreshold' instead." #-}

-- | This specifies if the action needs manual or automatic approval.
--
-- /Note:/ Consider using 'approvalModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaApprovalModel :: Lens.Lens' UpdateBudgetAction (Core.Maybe Types.ApprovalModel)
ubaApprovalModel = Lens.field @"approvalModel"
{-# DEPRECATED ubaApprovalModel "Use generic-lens or generic-optics with 'approvalModel' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaDefinition :: Lens.Lens' UpdateBudgetAction (Core.Maybe Types.Definition)
ubaDefinition = Lens.field @"definition"
{-# DEPRECATED ubaDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The role passed for action execution and reversion. Roles and actions must be in the same account.
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaExecutionRoleArn :: Lens.Lens' UpdateBudgetAction (Core.Maybe Types.ExecutionRoleArn)
ubaExecutionRoleArn = Lens.field @"executionRoleArn"
{-# DEPRECATED ubaExecutionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaNotificationType :: Lens.Lens' UpdateBudgetAction (Core.Maybe Types.NotificationType)
ubaNotificationType = Lens.field @"notificationType"
{-# DEPRECATED ubaNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaSubscribers :: Lens.Lens' UpdateBudgetAction (Core.Maybe (Core.NonEmpty Types.Subscriber))
ubaSubscribers = Lens.field @"subscribers"
{-# DEPRECATED ubaSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

instance Core.FromJSON UpdateBudgetAction where
  toJSON UpdateBudgetAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("ActionId" Core..= actionId),
            ("ActionThreshold" Core..=) Core.<$> actionThreshold,
            ("ApprovalModel" Core..=) Core.<$> approvalModel,
            ("Definition" Core..=) Core.<$> definition,
            ("ExecutionRoleArn" Core..=) Core.<$> executionRoleArn,
            ("NotificationType" Core..=) Core.<$> notificationType,
            ("Subscribers" Core..=) Core.<$> subscribers
          ]
      )

instance Core.AWSRequest UpdateBudgetAction where
  type Rs UpdateBudgetAction = UpdateBudgetActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSBudgetServiceGateway.UpdateBudgetAction")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBudgetActionResponse'
            Core.<$> (x Core..: "AccountId")
            Core.<*> (x Core..: "BudgetName")
            Core.<*> (x Core..: "OldAction")
            Core.<*> (x Core..: "NewAction")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateBudgetActionResponse' smart constructor.
data UpdateBudgetActionResponse = UpdateBudgetActionResponse'
  { accountId :: Types.AccountId,
    budgetName :: Types.BudgetName,
    -- | The previous action resource information.
    oldAction :: Types.Action,
    -- | The updated action resource information.
    newAction :: Types.Action,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBudgetActionResponse' value with any optional fields omitted.
mkUpdateBudgetActionResponse ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'oldAction'
  Types.Action ->
  -- | 'newAction'
  Types.Action ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateBudgetActionResponse
mkUpdateBudgetActionResponse
  accountId
  budgetName
  oldAction
  newAction
  responseStatus =
    UpdateBudgetActionResponse'
      { accountId,
        budgetName,
        oldAction,
        newAction,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarrsAccountId :: Lens.Lens' UpdateBudgetActionResponse Types.AccountId
ubarrsAccountId = Lens.field @"accountId"
{-# DEPRECATED ubarrsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarrsBudgetName :: Lens.Lens' UpdateBudgetActionResponse Types.BudgetName
ubarrsBudgetName = Lens.field @"budgetName"
{-# DEPRECATED ubarrsBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The previous action resource information.
--
-- /Note:/ Consider using 'oldAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarrsOldAction :: Lens.Lens' UpdateBudgetActionResponse Types.Action
ubarrsOldAction = Lens.field @"oldAction"
{-# DEPRECATED ubarrsOldAction "Use generic-lens or generic-optics with 'oldAction' instead." #-}

-- | The updated action resource information.
--
-- /Note:/ Consider using 'newAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarrsNewAction :: Lens.Lens' UpdateBudgetActionResponse Types.Action
ubarrsNewAction = Lens.field @"newAction"
{-# DEPRECATED ubarrsNewAction "Use generic-lens or generic-optics with 'newAction' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarrsResponseStatus :: Lens.Lens' UpdateBudgetActionResponse Core.Int
ubarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ubarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
