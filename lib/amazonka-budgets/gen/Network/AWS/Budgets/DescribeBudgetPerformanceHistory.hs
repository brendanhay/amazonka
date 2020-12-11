{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgetPerformanceHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the history for @DAILY@ , @MONTHLY@ , and @QUARTERLY@ budgets. Budget history isn't available for @ANNUAL@ budgets.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetPerformanceHistory
  ( -- * Creating a request
    DescribeBudgetPerformanceHistory (..),
    mkDescribeBudgetPerformanceHistory,

    -- ** Request lenses
    dbphTimePeriod,
    dbphNextToken,
    dbphMaxResults,
    dbphAccountId,
    dbphBudgetName,

    -- * Destructuring the response
    DescribeBudgetPerformanceHistoryResponse (..),
    mkDescribeBudgetPerformanceHistoryResponse,

    -- ** Response lenses
    dbphrsBudgetPerformanceHistory,
    dbphrsNextToken,
    dbphrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBudgetPerformanceHistory' smart constructor.
data DescribeBudgetPerformanceHistory = DescribeBudgetPerformanceHistory'
  { timePeriod ::
      Lude.Maybe TimePeriod,
    nextToken ::
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

-- | Creates a value of 'DescribeBudgetPerformanceHistory' with the minimum fields required to make a request.
--
-- * 'accountId' - Undocumented field.
-- * 'budgetName' - Undocumented field.
-- * 'maxResults' - Undocumented field.
-- * 'nextToken' - Undocumented field.
-- * 'timePeriod' - Retrieves how often the budget went into an @ALARM@ state for the specified time period.
mkDescribeBudgetPerformanceHistory ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  DescribeBudgetPerformanceHistory
mkDescribeBudgetPerformanceHistory pAccountId_ pBudgetName_ =
  DescribeBudgetPerformanceHistory'
    { timePeriod = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      accountId = pAccountId_,
      budgetName = pBudgetName_
    }

-- | Retrieves how often the budget went into an @ALARM@ state for the specified time period.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphTimePeriod :: Lens.Lens' DescribeBudgetPerformanceHistory (Lude.Maybe TimePeriod)
dbphTimePeriod = Lens.lens (timePeriod :: DescribeBudgetPerformanceHistory -> Lude.Maybe TimePeriod) (\s a -> s {timePeriod = a} :: DescribeBudgetPerformanceHistory)
{-# DEPRECATED dbphTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphNextToken :: Lens.Lens' DescribeBudgetPerformanceHistory (Lude.Maybe Lude.Text)
dbphNextToken = Lens.lens (nextToken :: DescribeBudgetPerformanceHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBudgetPerformanceHistory)
{-# DEPRECATED dbphNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphMaxResults :: Lens.Lens' DescribeBudgetPerformanceHistory (Lude.Maybe Lude.Natural)
dbphMaxResults = Lens.lens (maxResults :: DescribeBudgetPerformanceHistory -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBudgetPerformanceHistory)
{-# DEPRECATED dbphMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphAccountId :: Lens.Lens' DescribeBudgetPerformanceHistory Lude.Text
dbphAccountId = Lens.lens (accountId :: DescribeBudgetPerformanceHistory -> Lude.Text) (\s a -> s {accountId = a} :: DescribeBudgetPerformanceHistory)
{-# DEPRECATED dbphAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphBudgetName :: Lens.Lens' DescribeBudgetPerformanceHistory Lude.Text
dbphBudgetName = Lens.lens (budgetName :: DescribeBudgetPerformanceHistory -> Lude.Text) (\s a -> s {budgetName = a} :: DescribeBudgetPerformanceHistory)
{-# DEPRECATED dbphBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Page.AWSPager DescribeBudgetPerformanceHistory where
  page rq rs
    | Page.stop (rs Lens.^. dbphrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dbphrsBudgetPerformanceHistory) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dbphNextToken Lens..~ rs Lens.^. dbphrsNextToken

instance Lude.AWSRequest DescribeBudgetPerformanceHistory where
  type
    Rs DescribeBudgetPerformanceHistory =
      DescribeBudgetPerformanceHistoryResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBudgetPerformanceHistoryResponse'
            Lude.<$> (x Lude..?> "BudgetPerformanceHistory")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBudgetPerformanceHistory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSBudgetServiceGateway.DescribeBudgetPerformanceHistory" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBudgetPerformanceHistory where
  toJSON DescribeBudgetPerformanceHistory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TimePeriod" Lude..=) Lude.<$> timePeriod,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName)
          ]
      )

instance Lude.ToPath DescribeBudgetPerformanceHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBudgetPerformanceHistory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeBudgetPerformanceHistoryResponse' smart constructor.
data DescribeBudgetPerformanceHistoryResponse = DescribeBudgetPerformanceHistoryResponse'
  { budgetPerformanceHistory ::
      Lude.Maybe
        BudgetPerformanceHistory,
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'DescribeBudgetPerformanceHistoryResponse' with the minimum fields required to make a request.
--
-- * 'budgetPerformanceHistory' - The history of how often the budget has gone into an @ALARM@ state.
--
-- For @DAILY@ budgets, the history saves the state of the budget for the last 60 days. For @MONTHLY@ budgets, the history saves the state of the budget for the current month plus the last 12 months. For @QUARTERLY@ budgets, the history saves the state of the budget for the last four quarters.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeBudgetPerformanceHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBudgetPerformanceHistoryResponse
mkDescribeBudgetPerformanceHistoryResponse pResponseStatus_ =
  DescribeBudgetPerformanceHistoryResponse'
    { budgetPerformanceHistory =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The history of how often the budget has gone into an @ALARM@ state.
--
-- For @DAILY@ budgets, the history saves the state of the budget for the last 60 days. For @MONTHLY@ budgets, the history saves the state of the budget for the current month plus the last 12 months. For @QUARTERLY@ budgets, the history saves the state of the budget for the last four quarters.
--
-- /Note:/ Consider using 'budgetPerformanceHistory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphrsBudgetPerformanceHistory :: Lens.Lens' DescribeBudgetPerformanceHistoryResponse (Lude.Maybe BudgetPerformanceHistory)
dbphrsBudgetPerformanceHistory = Lens.lens (budgetPerformanceHistory :: DescribeBudgetPerformanceHistoryResponse -> Lude.Maybe BudgetPerformanceHistory) (\s a -> s {budgetPerformanceHistory = a} :: DescribeBudgetPerformanceHistoryResponse)
{-# DEPRECATED dbphrsBudgetPerformanceHistory "Use generic-lens or generic-optics with 'budgetPerformanceHistory' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphrsNextToken :: Lens.Lens' DescribeBudgetPerformanceHistoryResponse (Lude.Maybe Lude.Text)
dbphrsNextToken = Lens.lens (nextToken :: DescribeBudgetPerformanceHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBudgetPerformanceHistoryResponse)
{-# DEPRECATED dbphrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphrsResponseStatus :: Lens.Lens' DescribeBudgetPerformanceHistoryResponse Lude.Int
dbphrsResponseStatus = Lens.lens (responseStatus :: DescribeBudgetPerformanceHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBudgetPerformanceHistoryResponse)
{-# DEPRECATED dbphrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
