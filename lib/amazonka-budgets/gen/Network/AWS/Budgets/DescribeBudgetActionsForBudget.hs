{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgetActionsForBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all of the budget actions for a budget.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionsForBudget
  ( -- * Creating a request
    DescribeBudgetActionsForBudget (..),
    mkDescribeBudgetActionsForBudget,

    -- ** Request lenses
    dbafbNextToken,
    dbafbMaxResults,
    dbafbAccountId,
    dbafbBudgetName,

    -- * Destructuring the response
    DescribeBudgetActionsForBudgetResponse (..),
    mkDescribeBudgetActionsForBudgetResponse,

    -- ** Response lenses
    dbafbrsNextToken,
    dbafbrsResponseStatus,
    dbafbrsActions,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBudgetActionsForBudget' smart constructor.
data DescribeBudgetActionsForBudget = DescribeBudgetActionsForBudget'
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

-- | Creates a value of 'DescribeBudgetActionsForBudget' with the minimum fields required to make a request.
--
-- * 'accountId' - Undocumented field.
-- * 'budgetName' - Undocumented field.
-- * 'maxResults' - Undocumented field.
-- * 'nextToken' - Undocumented field.
mkDescribeBudgetActionsForBudget ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  DescribeBudgetActionsForBudget
mkDescribeBudgetActionsForBudget pAccountId_ pBudgetName_ =
  DescribeBudgetActionsForBudget'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      accountId = pAccountId_,
      budgetName = pBudgetName_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbNextToken :: Lens.Lens' DescribeBudgetActionsForBudget (Lude.Maybe Lude.Text)
dbafbNextToken = Lens.lens (nextToken :: DescribeBudgetActionsForBudget -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBudgetActionsForBudget)
{-# DEPRECATED dbafbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbMaxResults :: Lens.Lens' DescribeBudgetActionsForBudget (Lude.Maybe Lude.Natural)
dbafbMaxResults = Lens.lens (maxResults :: DescribeBudgetActionsForBudget -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBudgetActionsForBudget)
{-# DEPRECATED dbafbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbAccountId :: Lens.Lens' DescribeBudgetActionsForBudget Lude.Text
dbafbAccountId = Lens.lens (accountId :: DescribeBudgetActionsForBudget -> Lude.Text) (\s a -> s {accountId = a} :: DescribeBudgetActionsForBudget)
{-# DEPRECATED dbafbAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbBudgetName :: Lens.Lens' DescribeBudgetActionsForBudget Lude.Text
dbafbBudgetName = Lens.lens (budgetName :: DescribeBudgetActionsForBudget -> Lude.Text) (\s a -> s {budgetName = a} :: DescribeBudgetActionsForBudget)
{-# DEPRECATED dbafbBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Page.AWSPager DescribeBudgetActionsForBudget where
  page rq rs
    | Page.stop (rs Lens.^. dbafbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dbafbrsActions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dbafbNextToken Lens..~ rs Lens.^. dbafbrsNextToken

instance Lude.AWSRequest DescribeBudgetActionsForBudget where
  type
    Rs DescribeBudgetActionsForBudget =
      DescribeBudgetActionsForBudgetResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBudgetActionsForBudgetResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Actions" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DescribeBudgetActionsForBudget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSBudgetServiceGateway.DescribeBudgetActionsForBudget" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBudgetActionsForBudget where
  toJSON DescribeBudgetActionsForBudget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName)
          ]
      )

instance Lude.ToPath DescribeBudgetActionsForBudget where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBudgetActionsForBudget where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeBudgetActionsForBudgetResponse' smart constructor.
data DescribeBudgetActionsForBudgetResponse = DescribeBudgetActionsForBudgetResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    actions ::
      [Action]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBudgetActionsForBudgetResponse' with the minimum fields required to make a request.
--
-- * 'actions' - A list of the budget action resources information.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeBudgetActionsForBudgetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBudgetActionsForBudgetResponse
mkDescribeBudgetActionsForBudgetResponse pResponseStatus_ =
  DescribeBudgetActionsForBudgetResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      actions = Lude.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbrsNextToken :: Lens.Lens' DescribeBudgetActionsForBudgetResponse (Lude.Maybe Lude.Text)
dbafbrsNextToken = Lens.lens (nextToken :: DescribeBudgetActionsForBudgetResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBudgetActionsForBudgetResponse)
{-# DEPRECATED dbafbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbrsResponseStatus :: Lens.Lens' DescribeBudgetActionsForBudgetResponse Lude.Int
dbafbrsResponseStatus = Lens.lens (responseStatus :: DescribeBudgetActionsForBudgetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBudgetActionsForBudgetResponse)
{-# DEPRECATED dbafbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of the budget action resources information.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbrsActions :: Lens.Lens' DescribeBudgetActionsForBudgetResponse [Action]
dbafbrsActions = Lens.lens (actions :: DescribeBudgetActionsForBudgetResponse -> [Action]) (\s a -> s {actions = a} :: DescribeBudgetActionsForBudgetResponse)
{-# DEPRECATED dbafbrsActions "Use generic-lens or generic-optics with 'actions' instead." #-}
