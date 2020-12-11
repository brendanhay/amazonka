{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeNotificationsForBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the notifications that are associated with a budget.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeNotificationsForBudget
  ( -- * Creating a request
    DescribeNotificationsForBudget (..),
    mkDescribeNotificationsForBudget,

    -- ** Request lenses
    dnfbNextToken,
    dnfbMaxResults,
    dnfbAccountId,
    dnfbBudgetName,

    -- * Destructuring the response
    DescribeNotificationsForBudgetResponse (..),
    mkDescribeNotificationsForBudgetResponse,

    -- ** Response lenses
    dnfbrsNextToken,
    dnfbrsNotifications,
    dnfbrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of DescribeNotificationsForBudget
--
-- /See:/ 'mkDescribeNotificationsForBudget' smart constructor.
data DescribeNotificationsForBudget = DescribeNotificationsForBudget'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    accountId :: Lude.Text,
    budgetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotificationsForBudget' with the minimum fields required to make a request.
--
-- * 'accountId' - The @accountId@ that is associated with the budget whose notifications you want descriptions of.
-- * 'budgetName' - The name of the budget whose notifications you want descriptions of.
-- * 'maxResults' - An optional integer that represents how many entries a paginated response contains. The maximum is 100.
-- * 'nextToken' - The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
mkDescribeNotificationsForBudget ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  DescribeNotificationsForBudget
mkDescribeNotificationsForBudget pAccountId_ pBudgetName_ =
  DescribeNotificationsForBudget'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      accountId = pAccountId_,
      budgetName = pBudgetName_
    }

-- | The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbNextToken :: Lens.Lens' DescribeNotificationsForBudget (Lude.Maybe Lude.Text)
dnfbNextToken = Lens.lens (nextToken :: DescribeNotificationsForBudget -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNotificationsForBudget)
{-# DEPRECATED dnfbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An optional integer that represents how many entries a paginated response contains. The maximum is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbMaxResults :: Lens.Lens' DescribeNotificationsForBudget (Lude.Maybe Lude.Natural)
dnfbMaxResults = Lens.lens (maxResults :: DescribeNotificationsForBudget -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeNotificationsForBudget)
{-# DEPRECATED dnfbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @accountId@ that is associated with the budget whose notifications you want descriptions of.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbAccountId :: Lens.Lens' DescribeNotificationsForBudget Lude.Text
dnfbAccountId = Lens.lens (accountId :: DescribeNotificationsForBudget -> Lude.Text) (\s a -> s {accountId = a} :: DescribeNotificationsForBudget)
{-# DEPRECATED dnfbAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget whose notifications you want descriptions of.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbBudgetName :: Lens.Lens' DescribeNotificationsForBudget Lude.Text
dnfbBudgetName = Lens.lens (budgetName :: DescribeNotificationsForBudget -> Lude.Text) (\s a -> s {budgetName = a} :: DescribeNotificationsForBudget)
{-# DEPRECATED dnfbBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Page.AWSPager DescribeNotificationsForBudget where
  page rq rs
    | Page.stop (rs Lens.^. dnfbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dnfbrsNotifications) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dnfbNextToken Lens..~ rs Lens.^. dnfbrsNextToken

instance Lude.AWSRequest DescribeNotificationsForBudget where
  type
    Rs DescribeNotificationsForBudget =
      DescribeNotificationsForBudgetResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeNotificationsForBudgetResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Notifications" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNotificationsForBudget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSBudgetServiceGateway.DescribeNotificationsForBudget" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeNotificationsForBudget where
  toJSON DescribeNotificationsForBudget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName)
          ]
      )

instance Lude.ToPath DescribeNotificationsForBudget where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNotificationsForBudget where
  toQuery = Lude.const Lude.mempty

-- | Response of GetNotificationsForBudget
--
-- /See:/ 'mkDescribeNotificationsForBudgetResponse' smart constructor.
data DescribeNotificationsForBudgetResponse = DescribeNotificationsForBudgetResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    notifications ::
      Lude.Maybe
        [Notification],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotificationsForBudgetResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token in the service response that indicates the next set of results that you can retrieve.
-- * 'notifications' - A list of notifications that are associated with a budget.
-- * 'responseStatus' - The response status code.
mkDescribeNotificationsForBudgetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNotificationsForBudgetResponse
mkDescribeNotificationsForBudgetResponse pResponseStatus_ =
  DescribeNotificationsForBudgetResponse'
    { nextToken = Lude.Nothing,
      notifications = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token in the service response that indicates the next set of results that you can retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbrsNextToken :: Lens.Lens' DescribeNotificationsForBudgetResponse (Lude.Maybe Lude.Text)
dnfbrsNextToken = Lens.lens (nextToken :: DescribeNotificationsForBudgetResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNotificationsForBudgetResponse)
{-# DEPRECATED dnfbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of notifications that are associated with a budget.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbrsNotifications :: Lens.Lens' DescribeNotificationsForBudgetResponse (Lude.Maybe [Notification])
dnfbrsNotifications = Lens.lens (notifications :: DescribeNotificationsForBudgetResponse -> Lude.Maybe [Notification]) (\s a -> s {notifications = a} :: DescribeNotificationsForBudgetResponse)
{-# DEPRECATED dnfbrsNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbrsResponseStatus :: Lens.Lens' DescribeNotificationsForBudgetResponse Lude.Int
dnfbrsResponseStatus = Lens.lens (responseStatus :: DescribeNotificationsForBudgetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNotificationsForBudgetResponse)
{-# DEPRECATED dnfbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
