{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DeleteBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a budget. You can delete your budget at any time.
--
-- /Important:/ Deleting a budget also deletes the notifications and subscribers that are associated with that budget.
module Network.AWS.Budgets.DeleteBudget
  ( -- * Creating a request
    DeleteBudget (..),
    mkDeleteBudget,

    -- ** Request lenses
    dAccountId,
    dBudgetName,

    -- * Destructuring the response
    DeleteBudgetResponse (..),
    mkDeleteBudgetResponse,

    -- ** Response lenses
    dbrfrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DeleteBudget
--
-- /See:/ 'mkDeleteBudget' smart constructor.
data DeleteBudget = DeleteBudget'
  { -- | The @accountId@ that is associated with the budget that you want to delete.
    accountId :: Types.AccountId,
    -- | The name of the budget that you want to delete.
    budgetName :: Types.BudgetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBudget' value with any optional fields omitted.
mkDeleteBudget ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  DeleteBudget
mkDeleteBudget accountId budgetName =
  DeleteBudget' {accountId, budgetName}

-- | The @accountId@ that is associated with the budget that you want to delete.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccountId :: Lens.Lens' DeleteBudget Types.AccountId
dAccountId = Lens.field @"accountId"
{-# DEPRECATED dAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget that you want to delete.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBudgetName :: Lens.Lens' DeleteBudget Types.BudgetName
dBudgetName = Lens.field @"budgetName"
{-# DEPRECATED dBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Core.FromJSON DeleteBudget where
  toJSON DeleteBudget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName)
          ]
      )

instance Core.AWSRequest DeleteBudget where
  type Rs DeleteBudget = DeleteBudgetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSBudgetServiceGateway.DeleteBudget")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBudgetResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Response of DeleteBudget
--
-- /See:/ 'mkDeleteBudgetResponse' smart constructor.
newtype DeleteBudgetResponse = DeleteBudgetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBudgetResponse' value with any optional fields omitted.
mkDeleteBudgetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteBudgetResponse
mkDeleteBudgetResponse responseStatus =
  DeleteBudgetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrfrsResponseStatus :: Lens.Lens' DeleteBudgetResponse Core.Int
dbrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
