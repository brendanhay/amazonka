{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DeleteBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a budget action.
module Network.AWS.Budgets.DeleteBudgetAction
  ( -- * Creating a request
    DeleteBudgetAction (..),
    mkDeleteBudgetAction,

    -- ** Request lenses
    dbaAccountId,
    dbaBudgetName,
    dbaActionId,

    -- * Destructuring the response
    DeleteBudgetActionResponse (..),
    mkDeleteBudgetActionResponse,

    -- ** Response lenses
    drsAccountId,
    drsBudgetName,
    drsAction,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBudgetAction' smart constructor.
data DeleteBudgetAction = DeleteBudgetAction'
  { accountId :: Types.AccountId,
    budgetName :: Types.BudgetName,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Types.ActionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBudgetAction' value with any optional fields omitted.
mkDeleteBudgetAction ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'actionId'
  Types.ActionId ->
  DeleteBudgetAction
mkDeleteBudgetAction accountId budgetName actionId =
  DeleteBudgetAction' {accountId, budgetName, actionId}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaAccountId :: Lens.Lens' DeleteBudgetAction Types.AccountId
dbaAccountId = Lens.field @"accountId"
{-# DEPRECATED dbaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaBudgetName :: Lens.Lens' DeleteBudgetAction Types.BudgetName
dbaBudgetName = Lens.field @"budgetName"
{-# DEPRECATED dbaBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaActionId :: Lens.Lens' DeleteBudgetAction Types.ActionId
dbaActionId = Lens.field @"actionId"
{-# DEPRECATED dbaActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

instance Core.FromJSON DeleteBudgetAction where
  toJSON DeleteBudgetAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("ActionId" Core..= actionId)
          ]
      )

instance Core.AWSRequest DeleteBudgetAction where
  type Rs DeleteBudgetAction = DeleteBudgetActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSBudgetServiceGateway.DeleteBudgetAction")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBudgetActionResponse'
            Core.<$> (x Core..: "AccountId")
            Core.<*> (x Core..: "BudgetName")
            Core.<*> (x Core..: "Action")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteBudgetActionResponse' smart constructor.
data DeleteBudgetActionResponse = DeleteBudgetActionResponse'
  { accountId :: Types.AccountId,
    budgetName :: Types.BudgetName,
    action :: Types.Action,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBudgetActionResponse' value with any optional fields omitted.
mkDeleteBudgetActionResponse ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'action'
  Types.Action ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteBudgetActionResponse
mkDeleteBudgetActionResponse
  accountId
  budgetName
  action
  responseStatus =
    DeleteBudgetActionResponse'
      { accountId,
        budgetName,
        action,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAccountId :: Lens.Lens' DeleteBudgetActionResponse Types.AccountId
drsAccountId = Lens.field @"accountId"
{-# DEPRECATED drsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsBudgetName :: Lens.Lens' DeleteBudgetActionResponse Types.BudgetName
drsBudgetName = Lens.field @"budgetName"
{-# DEPRECATED drsBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAction :: Lens.Lens' DeleteBudgetActionResponse Types.Action
drsAction = Lens.field @"action"
{-# DEPRECATED drsAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteBudgetActionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
