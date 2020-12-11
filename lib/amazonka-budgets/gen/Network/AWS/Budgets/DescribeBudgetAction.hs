{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget action detail.
module Network.AWS.Budgets.DescribeBudgetAction
  ( -- * Creating a request
    DescribeBudgetAction (..),
    mkDescribeBudgetAction,

    -- ** Request lenses
    dbabAccountId,
    dbabBudgetName,
    dbabActionId,

    -- * Destructuring the response
    DescribeBudgetActionResponse (..),
    mkDescribeBudgetActionResponse,

    -- ** Response lenses
    dbarsResponseStatus,
    dbarsAccountId,
    dbarsBudgetName,
    dbarsAction,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBudgetAction' smart constructor.
data DescribeBudgetAction = DescribeBudgetAction'
  { accountId ::
      Lude.Text,
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

-- | Creates a value of 'DescribeBudgetAction' with the minimum fields required to make a request.
--
-- * 'accountId' - Undocumented field.
-- * 'actionId' - A system-generated universally unique identifier (UUID) for the action.
-- * 'budgetName' - Undocumented field.
mkDescribeBudgetAction ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'actionId'
  Lude.Text ->
  DescribeBudgetAction
mkDescribeBudgetAction pAccountId_ pBudgetName_ pActionId_ =
  DescribeBudgetAction'
    { accountId = pAccountId_,
      budgetName = pBudgetName_,
      actionId = pActionId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbabAccountId :: Lens.Lens' DescribeBudgetAction Lude.Text
dbabAccountId = Lens.lens (accountId :: DescribeBudgetAction -> Lude.Text) (\s a -> s {accountId = a} :: DescribeBudgetAction)
{-# DEPRECATED dbabAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbabBudgetName :: Lens.Lens' DescribeBudgetAction Lude.Text
dbabBudgetName = Lens.lens (budgetName :: DescribeBudgetAction -> Lude.Text) (\s a -> s {budgetName = a} :: DescribeBudgetAction)
{-# DEPRECATED dbabBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbabActionId :: Lens.Lens' DescribeBudgetAction Lude.Text
dbabActionId = Lens.lens (actionId :: DescribeBudgetAction -> Lude.Text) (\s a -> s {actionId = a} :: DescribeBudgetAction)
{-# DEPRECATED dbabActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

instance Lude.AWSRequest DescribeBudgetAction where
  type Rs DescribeBudgetAction = DescribeBudgetActionResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBudgetActionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "AccountId")
            Lude.<*> (x Lude..:> "BudgetName")
            Lude.<*> (x Lude..:> "Action")
      )

instance Lude.ToHeaders DescribeBudgetAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSBudgetServiceGateway.DescribeBudgetAction" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBudgetAction where
  toJSON DescribeBudgetAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName),
            Lude.Just ("ActionId" Lude..= actionId)
          ]
      )

instance Lude.ToPath DescribeBudgetAction where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBudgetAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeBudgetActionResponse' smart constructor.
data DescribeBudgetActionResponse = DescribeBudgetActionResponse'
  { responseStatus ::
      Lude.Int,
    accountId :: Lude.Text,
    budgetName :: Lude.Text,
    action :: Action
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBudgetActionResponse' with the minimum fields required to make a request.
--
-- * 'accountId' - Undocumented field.
-- * 'action' - A budget action resource.
-- * 'budgetName' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeBudgetActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'action'
  Action ->
  DescribeBudgetActionResponse
mkDescribeBudgetActionResponse
  pResponseStatus_
  pAccountId_
  pBudgetName_
  pAction_ =
    DescribeBudgetActionResponse'
      { responseStatus = pResponseStatus_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        action = pAction_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbarsResponseStatus :: Lens.Lens' DescribeBudgetActionResponse Lude.Int
dbarsResponseStatus = Lens.lens (responseStatus :: DescribeBudgetActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBudgetActionResponse)
{-# DEPRECATED dbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbarsAccountId :: Lens.Lens' DescribeBudgetActionResponse Lude.Text
dbarsAccountId = Lens.lens (accountId :: DescribeBudgetActionResponse -> Lude.Text) (\s a -> s {accountId = a} :: DescribeBudgetActionResponse)
{-# DEPRECATED dbarsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbarsBudgetName :: Lens.Lens' DescribeBudgetActionResponse Lude.Text
dbarsBudgetName = Lens.lens (budgetName :: DescribeBudgetActionResponse -> Lude.Text) (\s a -> s {budgetName = a} :: DescribeBudgetActionResponse)
{-# DEPRECATED dbarsBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | A budget action resource.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbarsAction :: Lens.Lens' DescribeBudgetActionResponse Action
dbarsAction = Lens.lens (action :: DescribeBudgetActionResponse -> Action) (\s a -> s {action = a} :: DescribeBudgetActionResponse)
{-# DEPRECATED dbarsAction "Use generic-lens or generic-optics with 'action' instead." #-}
