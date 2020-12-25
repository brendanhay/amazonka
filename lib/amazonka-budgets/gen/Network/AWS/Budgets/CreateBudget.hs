{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.CreateBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a budget and, if included, notifications and subscribers.
--
-- /Important:/ Only one of @BudgetLimit@ or @PlannedBudgetLimits@ can be present in the syntax at one time. Use the syntax that matches your case. The Request Syntax section shows the @BudgetLimit@ syntax. For @PlannedBudgetLimits@ , see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_CreateBudget.html#API_CreateBudget_Examples Examples> section.
module Network.AWS.Budgets.CreateBudget
  ( -- * Creating a request
    CreateBudget (..),
    mkCreateBudget,

    -- ** Request lenses
    cbAccountId,
    cbBudget,
    cbNotificationsWithSubscribers,

    -- * Destructuring the response
    CreateBudgetResponse (..),
    mkCreateBudgetResponse,

    -- ** Response lenses
    cbrrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of CreateBudget
--
-- /See:/ 'mkCreateBudget' smart constructor.
data CreateBudget = CreateBudget'
  { -- | The @accountId@ that is associated with the budget.
    accountId :: Types.AccountId,
    -- | The budget object that you want to create.
    budget :: Types.Budget,
    -- | A notification that you want to associate with a budget. A budget can have up to five notifications, and each notification can have one SNS subscriber and up to 10 email subscribers. If you include notifications and subscribers in your @CreateBudget@ call, AWS creates the notifications and subscribers for you.
    notificationsWithSubscribers :: Core.Maybe [Types.NotificationWithSubscribers]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateBudget' value with any optional fields omitted.
mkCreateBudget ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budget'
  Types.Budget ->
  CreateBudget
mkCreateBudget accountId budget =
  CreateBudget'
    { accountId,
      budget,
      notificationsWithSubscribers = Core.Nothing
    }

-- | The @accountId@ that is associated with the budget.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbAccountId :: Lens.Lens' CreateBudget Types.AccountId
cbAccountId = Lens.field @"accountId"
{-# DEPRECATED cbAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The budget object that you want to create.
--
-- /Note:/ Consider using 'budget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBudget :: Lens.Lens' CreateBudget Types.Budget
cbBudget = Lens.field @"budget"
{-# DEPRECATED cbBudget "Use generic-lens or generic-optics with 'budget' instead." #-}

-- | A notification that you want to associate with a budget. A budget can have up to five notifications, and each notification can have one SNS subscriber and up to 10 email subscribers. If you include notifications and subscribers in your @CreateBudget@ call, AWS creates the notifications and subscribers for you.
--
-- /Note:/ Consider using 'notificationsWithSubscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbNotificationsWithSubscribers :: Lens.Lens' CreateBudget (Core.Maybe [Types.NotificationWithSubscribers])
cbNotificationsWithSubscribers = Lens.field @"notificationsWithSubscribers"
{-# DEPRECATED cbNotificationsWithSubscribers "Use generic-lens or generic-optics with 'notificationsWithSubscribers' instead." #-}

instance Core.FromJSON CreateBudget where
  toJSON CreateBudget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("Budget" Core..= budget),
            ("NotificationsWithSubscribers" Core..=)
              Core.<$> notificationsWithSubscribers
          ]
      )

instance Core.AWSRequest CreateBudget where
  type Rs CreateBudget = CreateBudgetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSBudgetServiceGateway.CreateBudget")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateBudgetResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Response of CreateBudget
--
-- /See:/ 'mkCreateBudgetResponse' smart constructor.
newtype CreateBudgetResponse = CreateBudgetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBudgetResponse' value with any optional fields omitted.
mkCreateBudgetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateBudgetResponse
mkCreateBudgetResponse responseStatus =
  CreateBudgetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsResponseStatus :: Lens.Lens' CreateBudgetResponse Core.Int
cbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
