{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cbNotificationsWithSubscribers,
    cbAccountId,
    cbBudget,

    -- * Destructuring the response
    CreateBudgetResponse (..),
    mkCreateBudgetResponse,

    -- ** Response lenses
    cbrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of CreateBudget
--
-- /See:/ 'mkCreateBudget' smart constructor.
data CreateBudget = CreateBudget'
  { notificationsWithSubscribers ::
      Lude.Maybe [NotificationWithSubscribers],
    accountId :: Lude.Text,
    budget :: Budget
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBudget' with the minimum fields required to make a request.
--
-- * 'accountId' - The @accountId@ that is associated with the budget.
-- * 'budget' - The budget object that you want to create.
-- * 'notificationsWithSubscribers' - A notification that you want to associate with a budget. A budget can have up to five notifications, and each notification can have one SNS subscriber and up to 10 email subscribers. If you include notifications and subscribers in your @CreateBudget@ call, AWS creates the notifications and subscribers for you.
mkCreateBudget ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budget'
  Budget ->
  CreateBudget
mkCreateBudget pAccountId_ pBudget_ =
  CreateBudget'
    { notificationsWithSubscribers = Lude.Nothing,
      accountId = pAccountId_,
      budget = pBudget_
    }

-- | A notification that you want to associate with a budget. A budget can have up to five notifications, and each notification can have one SNS subscriber and up to 10 email subscribers. If you include notifications and subscribers in your @CreateBudget@ call, AWS creates the notifications and subscribers for you.
--
-- /Note:/ Consider using 'notificationsWithSubscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbNotificationsWithSubscribers :: Lens.Lens' CreateBudget (Lude.Maybe [NotificationWithSubscribers])
cbNotificationsWithSubscribers = Lens.lens (notificationsWithSubscribers :: CreateBudget -> Lude.Maybe [NotificationWithSubscribers]) (\s a -> s {notificationsWithSubscribers = a} :: CreateBudget)
{-# DEPRECATED cbNotificationsWithSubscribers "Use generic-lens or generic-optics with 'notificationsWithSubscribers' instead." #-}

-- | The @accountId@ that is associated with the budget.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbAccountId :: Lens.Lens' CreateBudget Lude.Text
cbAccountId = Lens.lens (accountId :: CreateBudget -> Lude.Text) (\s a -> s {accountId = a} :: CreateBudget)
{-# DEPRECATED cbAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The budget object that you want to create.
--
-- /Note:/ Consider using 'budget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBudget :: Lens.Lens' CreateBudget Budget
cbBudget = Lens.lens (budget :: CreateBudget -> Budget) (\s a -> s {budget = a} :: CreateBudget)
{-# DEPRECATED cbBudget "Use generic-lens or generic-optics with 'budget' instead." #-}

instance Lude.AWSRequest CreateBudget where
  type Rs CreateBudget = CreateBudgetResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateBudgetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateBudget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.CreateBudget" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBudget where
  toJSON CreateBudget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NotificationsWithSubscribers" Lude..=)
              Lude.<$> notificationsWithSubscribers,
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("Budget" Lude..= budget)
          ]
      )

instance Lude.ToPath CreateBudget where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateBudget where
  toQuery = Lude.const Lude.mempty

-- | Response of CreateBudget
--
-- /See:/ 'mkCreateBudgetResponse' smart constructor.
newtype CreateBudgetResponse = CreateBudgetResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBudgetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateBudgetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBudgetResponse
mkCreateBudgetResponse pResponseStatus_ =
  CreateBudgetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsResponseStatus :: Lens.Lens' CreateBudgetResponse Lude.Int
cbrsResponseStatus = Lens.lens (responseStatus :: CreateBudgetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBudgetResponse)
{-# DEPRECATED cbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
