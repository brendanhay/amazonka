{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgetActionsForAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all of the budget actions for an account.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionsForAccount
  ( -- * Creating a request
    DescribeBudgetActionsForAccount (..),
    mkDescribeBudgetActionsForAccount,

    -- ** Request lenses
    dbafaNextToken,
    dbafaMaxResults,
    dbafaAccountId,

    -- * Destructuring the response
    DescribeBudgetActionsForAccountResponse (..),
    mkDescribeBudgetActionsForAccountResponse,

    -- ** Response lenses
    dbafarsNextToken,
    dbafarsResponseStatus,
    dbafarsActions,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBudgetActionsForAccount' smart constructor.
data DescribeBudgetActionsForAccount = DescribeBudgetActionsForAccount'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    accountId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBudgetActionsForAccount' with the minimum fields required to make a request.
--
-- * 'accountId' - Undocumented field.
-- * 'maxResults' - Undocumented field.
-- * 'nextToken' - Undocumented field.
mkDescribeBudgetActionsForAccount ::
  -- | 'accountId'
  Lude.Text ->
  DescribeBudgetActionsForAccount
mkDescribeBudgetActionsForAccount pAccountId_ =
  DescribeBudgetActionsForAccount'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      accountId = pAccountId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafaNextToken :: Lens.Lens' DescribeBudgetActionsForAccount (Lude.Maybe Lude.Text)
dbafaNextToken = Lens.lens (nextToken :: DescribeBudgetActionsForAccount -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBudgetActionsForAccount)
{-# DEPRECATED dbafaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafaMaxResults :: Lens.Lens' DescribeBudgetActionsForAccount (Lude.Maybe Lude.Natural)
dbafaMaxResults = Lens.lens (maxResults :: DescribeBudgetActionsForAccount -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBudgetActionsForAccount)
{-# DEPRECATED dbafaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafaAccountId :: Lens.Lens' DescribeBudgetActionsForAccount Lude.Text
dbafaAccountId = Lens.lens (accountId :: DescribeBudgetActionsForAccount -> Lude.Text) (\s a -> s {accountId = a} :: DescribeBudgetActionsForAccount)
{-# DEPRECATED dbafaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Page.AWSPager DescribeBudgetActionsForAccount where
  page rq rs
    | Page.stop (rs Lens.^. dbafarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dbafarsActions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dbafaNextToken Lens..~ rs Lens.^. dbafarsNextToken

instance Lude.AWSRequest DescribeBudgetActionsForAccount where
  type
    Rs DescribeBudgetActionsForAccount =
      DescribeBudgetActionsForAccountResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBudgetActionsForAccountResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Actions" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DescribeBudgetActionsForAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSBudgetServiceGateway.DescribeBudgetActionsForAccount" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBudgetActionsForAccount where
  toJSON DescribeBudgetActionsForAccount' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AccountId" Lude..= accountId)
          ]
      )

instance Lude.ToPath DescribeBudgetActionsForAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBudgetActionsForAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeBudgetActionsForAccountResponse' smart constructor.
data DescribeBudgetActionsForAccountResponse = DescribeBudgetActionsForAccountResponse'
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

-- | Creates a value of 'DescribeBudgetActionsForAccountResponse' with the minimum fields required to make a request.
--
-- * 'actions' - A list of the budget action resources information.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeBudgetActionsForAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBudgetActionsForAccountResponse
mkDescribeBudgetActionsForAccountResponse pResponseStatus_ =
  DescribeBudgetActionsForAccountResponse'
    { nextToken =
        Lude.Nothing,
      responseStatus = pResponseStatus_,
      actions = Lude.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafarsNextToken :: Lens.Lens' DescribeBudgetActionsForAccountResponse (Lude.Maybe Lude.Text)
dbafarsNextToken = Lens.lens (nextToken :: DescribeBudgetActionsForAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBudgetActionsForAccountResponse)
{-# DEPRECATED dbafarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafarsResponseStatus :: Lens.Lens' DescribeBudgetActionsForAccountResponse Lude.Int
dbafarsResponseStatus = Lens.lens (responseStatus :: DescribeBudgetActionsForAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBudgetActionsForAccountResponse)
{-# DEPRECATED dbafarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of the budget action resources information.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafarsActions :: Lens.Lens' DescribeBudgetActionsForAccountResponse [Action]
dbafarsActions = Lens.lens (actions :: DescribeBudgetActionsForAccountResponse -> [Action]) (\s a -> s {actions = a} :: DescribeBudgetActionsForAccountResponse)
{-# DEPRECATED dbafarsActions "Use generic-lens or generic-optics with 'actions' instead." #-}
