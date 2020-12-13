{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the budgets that are associated with an account.
--
-- /Important:/ The Request Syntax section shows the @BudgetLimit@ syntax. For @PlannedBudgetLimits@ , see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_DescribeBudgets.html#API_DescribeBudgets_Examples Examples> section.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgets
  ( -- * Creating a request
    DescribeBudgets (..),
    mkDescribeBudgets,

    -- ** Request lenses
    dbAccountId,
    dbNextToken,
    dbMaxResults,

    -- * Destructuring the response
    DescribeBudgetsResponse (..),
    mkDescribeBudgetsResponse,

    -- ** Response lenses
    dbrsNextToken,
    dbrsBudgets,
    dbrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of DescribeBudgets
--
-- /See:/ 'mkDescribeBudgets' smart constructor.
data DescribeBudgets = DescribeBudgets'
  { -- | The @accountId@ that is associated with the budgets that you want descriptions of.
    accountId :: Lude.Text,
    -- | The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An optional integer that represents how many entries a paginated response contains. The maximum is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBudgets' with the minimum fields required to make a request.
--
-- * 'accountId' - The @accountId@ that is associated with the budgets that you want descriptions of.
-- * 'nextToken' - The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
-- * 'maxResults' - An optional integer that represents how many entries a paginated response contains. The maximum is 100.
mkDescribeBudgets ::
  -- | 'accountId'
  Lude.Text ->
  DescribeBudgets
mkDescribeBudgets pAccountId_ =
  DescribeBudgets'
    { accountId = pAccountId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The @accountId@ that is associated with the budgets that you want descriptions of.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbAccountId :: Lens.Lens' DescribeBudgets Lude.Text
dbAccountId = Lens.lens (accountId :: DescribeBudgets -> Lude.Text) (\s a -> s {accountId = a} :: DescribeBudgets)
{-# DEPRECATED dbAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbNextToken :: Lens.Lens' DescribeBudgets (Lude.Maybe Lude.Text)
dbNextToken = Lens.lens (nextToken :: DescribeBudgets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBudgets)
{-# DEPRECATED dbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An optional integer that represents how many entries a paginated response contains. The maximum is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbMaxResults :: Lens.Lens' DescribeBudgets (Lude.Maybe Lude.Natural)
dbMaxResults = Lens.lens (maxResults :: DescribeBudgets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBudgets)
{-# DEPRECATED dbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeBudgets where
  page rq rs
    | Page.stop (rs Lens.^. dbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dbrsBudgets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dbNextToken Lens..~ rs Lens.^. dbrsNextToken

instance Lude.AWSRequest DescribeBudgets where
  type Rs DescribeBudgets = DescribeBudgetsResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBudgetsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Budgets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBudgets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.DescribeBudgets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBudgets where
  toJSON DescribeBudgets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountId" Lude..= accountId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeBudgets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBudgets where
  toQuery = Lude.const Lude.mempty

-- | Response of DescribeBudgets
--
-- /See:/ 'mkDescribeBudgetsResponse' smart constructor.
data DescribeBudgetsResponse = DescribeBudgetsResponse'
  { -- | The pagination token in the service response that indicates the next set of results that you can retrieve.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of budgets.
    budgets :: Lude.Maybe [Budget],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBudgetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token in the service response that indicates the next set of results that you can retrieve.
-- * 'budgets' - A list of budgets.
-- * 'responseStatus' - The response status code.
mkDescribeBudgetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBudgetsResponse
mkDescribeBudgetsResponse pResponseStatus_ =
  DescribeBudgetsResponse'
    { nextToken = Lude.Nothing,
      budgets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token in the service response that indicates the next set of results that you can retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsNextToken :: Lens.Lens' DescribeBudgetsResponse (Lude.Maybe Lude.Text)
dbrsNextToken = Lens.lens (nextToken :: DescribeBudgetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBudgetsResponse)
{-# DEPRECATED dbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsBudgets :: Lens.Lens' DescribeBudgetsResponse (Lude.Maybe [Budget])
dbrsBudgets = Lens.lens (budgets :: DescribeBudgetsResponse -> Lude.Maybe [Budget]) (\s a -> s {budgets = a} :: DescribeBudgetsResponse)
{-# DEPRECATED dbrsBudgets "Use generic-lens or generic-optics with 'budgets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsResponseStatus :: Lens.Lens' DescribeBudgetsResponse Lude.Int
dbrsResponseStatus = Lens.lens (responseStatus :: DescribeBudgetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBudgetsResponse)
{-# DEPRECATED dbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
