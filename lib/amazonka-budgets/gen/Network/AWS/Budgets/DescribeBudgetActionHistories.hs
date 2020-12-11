{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgetActionHistories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget action history detail.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionHistories
  ( -- * Creating a request
    DescribeBudgetActionHistories (..),
    mkDescribeBudgetActionHistories,

    -- ** Request lenses
    dbahTimePeriod,
    dbahNextToken,
    dbahMaxResults,
    dbahAccountId,
    dbahBudgetName,
    dbahActionId,

    -- * Destructuring the response
    DescribeBudgetActionHistoriesResponse (..),
    mkDescribeBudgetActionHistoriesResponse,

    -- ** Response lenses
    dbahrsNextToken,
    dbahrsResponseStatus,
    dbahrsActionHistories,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBudgetActionHistories' smart constructor.
data DescribeBudgetActionHistories = DescribeBudgetActionHistories'
  { timePeriod ::
      Lude.Maybe TimePeriod,
    nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    accountId :: Lude.Text,
    budgetName :: Lude.Text,
    actionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBudgetActionHistories' with the minimum fields required to make a request.
--
-- * 'accountId' - Undocumented field.
-- * 'actionId' - A system-generated universally unique identifier (UUID) for the action.
-- * 'budgetName' - Undocumented field.
-- * 'maxResults' - Undocumented field.
-- * 'nextToken' - Undocumented field.
-- * 'timePeriod' - Undocumented field.
mkDescribeBudgetActionHistories ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'actionId'
  Lude.Text ->
  DescribeBudgetActionHistories
mkDescribeBudgetActionHistories pAccountId_ pBudgetName_ pActionId_ =
  DescribeBudgetActionHistories'
    { timePeriod = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      accountId = pAccountId_,
      budgetName = pBudgetName_,
      actionId = pActionId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahTimePeriod :: Lens.Lens' DescribeBudgetActionHistories (Lude.Maybe TimePeriod)
dbahTimePeriod = Lens.lens (timePeriod :: DescribeBudgetActionHistories -> Lude.Maybe TimePeriod) (\s a -> s {timePeriod = a} :: DescribeBudgetActionHistories)
{-# DEPRECATED dbahTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahNextToken :: Lens.Lens' DescribeBudgetActionHistories (Lude.Maybe Lude.Text)
dbahNextToken = Lens.lens (nextToken :: DescribeBudgetActionHistories -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBudgetActionHistories)
{-# DEPRECATED dbahNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahMaxResults :: Lens.Lens' DescribeBudgetActionHistories (Lude.Maybe Lude.Natural)
dbahMaxResults = Lens.lens (maxResults :: DescribeBudgetActionHistories -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBudgetActionHistories)
{-# DEPRECATED dbahMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahAccountId :: Lens.Lens' DescribeBudgetActionHistories Lude.Text
dbahAccountId = Lens.lens (accountId :: DescribeBudgetActionHistories -> Lude.Text) (\s a -> s {accountId = a} :: DescribeBudgetActionHistories)
{-# DEPRECATED dbahAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahBudgetName :: Lens.Lens' DescribeBudgetActionHistories Lude.Text
dbahBudgetName = Lens.lens (budgetName :: DescribeBudgetActionHistories -> Lude.Text) (\s a -> s {budgetName = a} :: DescribeBudgetActionHistories)
{-# DEPRECATED dbahBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahActionId :: Lens.Lens' DescribeBudgetActionHistories Lude.Text
dbahActionId = Lens.lens (actionId :: DescribeBudgetActionHistories -> Lude.Text) (\s a -> s {actionId = a} :: DescribeBudgetActionHistories)
{-# DEPRECATED dbahActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

instance Page.AWSPager DescribeBudgetActionHistories where
  page rq rs
    | Page.stop (rs Lens.^. dbahrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dbahrsActionHistories) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dbahNextToken Lens..~ rs Lens.^. dbahrsNextToken

instance Lude.AWSRequest DescribeBudgetActionHistories where
  type
    Rs DescribeBudgetActionHistories =
      DescribeBudgetActionHistoriesResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBudgetActionHistoriesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "ActionHistories" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DescribeBudgetActionHistories where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSBudgetServiceGateway.DescribeBudgetActionHistories" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBudgetActionHistories where
  toJSON DescribeBudgetActionHistories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TimePeriod" Lude..=) Lude.<$> timePeriod,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName),
            Lude.Just ("ActionId" Lude..= actionId)
          ]
      )

instance Lude.ToPath DescribeBudgetActionHistories where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBudgetActionHistories where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeBudgetActionHistoriesResponse' smart constructor.
data DescribeBudgetActionHistoriesResponse = DescribeBudgetActionHistoriesResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    actionHistories ::
      [ActionHistory]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBudgetActionHistoriesResponse' with the minimum fields required to make a request.
--
-- * 'actionHistories' - The historical record of the budget action resource.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeBudgetActionHistoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBudgetActionHistoriesResponse
mkDescribeBudgetActionHistoriesResponse pResponseStatus_ =
  DescribeBudgetActionHistoriesResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      actionHistories = Lude.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahrsNextToken :: Lens.Lens' DescribeBudgetActionHistoriesResponse (Lude.Maybe Lude.Text)
dbahrsNextToken = Lens.lens (nextToken :: DescribeBudgetActionHistoriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBudgetActionHistoriesResponse)
{-# DEPRECATED dbahrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahrsResponseStatus :: Lens.Lens' DescribeBudgetActionHistoriesResponse Lude.Int
dbahrsResponseStatus = Lens.lens (responseStatus :: DescribeBudgetActionHistoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBudgetActionHistoriesResponse)
{-# DEPRECATED dbahrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The historical record of the budget action resource.
--
-- /Note:/ Consider using 'actionHistories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahrsActionHistories :: Lens.Lens' DescribeBudgetActionHistoriesResponse [ActionHistory]
dbahrsActionHistories = Lens.lens (actionHistories :: DescribeBudgetActionHistoriesResponse -> [ActionHistory]) (\s a -> s {actionHistories = a} :: DescribeBudgetActionHistoriesResponse)
{-# DEPRECATED dbahrsActionHistories "Use generic-lens or generic-optics with 'actionHistories' instead." #-}
