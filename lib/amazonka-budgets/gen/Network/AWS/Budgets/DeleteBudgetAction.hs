{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DeleteBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a budget action.
module Network.AWS.Budgets.DeleteBudgetAction
  ( -- * Creating a request
    DeleteBudgetAction (..),
    mkDeleteBudgetAction,

    -- ** Request lenses
    dbaAccountId,
    dbaBudgetName,
    dbaActionId,

    -- * Destructuring the response
    DeleteBudgetActionResponse (..),
    mkDeleteBudgetActionResponse,

    -- ** Response lenses
    drsResponseStatus,
    drsAccountId,
    drsBudgetName,
    drsAction,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBudgetAction' smart constructor.
data DeleteBudgetAction = DeleteBudgetAction'
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

-- | Creates a value of 'DeleteBudgetAction' with the minimum fields required to make a request.
--
-- * 'accountId' - Undocumented field.
-- * 'actionId' - A system-generated universally unique identifier (UUID) for the action.
-- * 'budgetName' - Undocumented field.
mkDeleteBudgetAction ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'actionId'
  Lude.Text ->
  DeleteBudgetAction
mkDeleteBudgetAction pAccountId_ pBudgetName_ pActionId_ =
  DeleteBudgetAction'
    { accountId = pAccountId_,
      budgetName = pBudgetName_,
      actionId = pActionId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaAccountId :: Lens.Lens' DeleteBudgetAction Lude.Text
dbaAccountId = Lens.lens (accountId :: DeleteBudgetAction -> Lude.Text) (\s a -> s {accountId = a} :: DeleteBudgetAction)
{-# DEPRECATED dbaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaBudgetName :: Lens.Lens' DeleteBudgetAction Lude.Text
dbaBudgetName = Lens.lens (budgetName :: DeleteBudgetAction -> Lude.Text) (\s a -> s {budgetName = a} :: DeleteBudgetAction)
{-# DEPRECATED dbaBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaActionId :: Lens.Lens' DeleteBudgetAction Lude.Text
dbaActionId = Lens.lens (actionId :: DeleteBudgetAction -> Lude.Text) (\s a -> s {actionId = a} :: DeleteBudgetAction)
{-# DEPRECATED dbaActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

instance Lude.AWSRequest DeleteBudgetAction where
  type Rs DeleteBudgetAction = DeleteBudgetActionResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteBudgetActionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "AccountId")
            Lude.<*> (x Lude..:> "BudgetName")
            Lude.<*> (x Lude..:> "Action")
      )

instance Lude.ToHeaders DeleteBudgetAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.DeleteBudgetAction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBudgetAction where
  toJSON DeleteBudgetAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName),
            Lude.Just ("ActionId" Lude..= actionId)
          ]
      )

instance Lude.ToPath DeleteBudgetAction where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBudgetAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBudgetActionResponse' smart constructor.
data DeleteBudgetActionResponse = DeleteBudgetActionResponse'
  { responseStatus ::
      Lude.Int,
    accountId :: Lude.Text,
    budgetName :: Lude.Text,
    action :: Action
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBudgetActionResponse' with the minimum fields required to make a request.
--
-- * 'accountId' - Undocumented field.
-- * 'action' - Undocumented field.
-- * 'budgetName' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteBudgetActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'action'
  Action ->
  DeleteBudgetActionResponse
mkDeleteBudgetActionResponse
  pResponseStatus_
  pAccountId_
  pBudgetName_
  pAction_ =
    DeleteBudgetActionResponse'
      { responseStatus = pResponseStatus_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        action = pAction_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteBudgetActionResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteBudgetActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBudgetActionResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAccountId :: Lens.Lens' DeleteBudgetActionResponse Lude.Text
drsAccountId = Lens.lens (accountId :: DeleteBudgetActionResponse -> Lude.Text) (\s a -> s {accountId = a} :: DeleteBudgetActionResponse)
{-# DEPRECATED drsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsBudgetName :: Lens.Lens' DeleteBudgetActionResponse Lude.Text
drsBudgetName = Lens.lens (budgetName :: DeleteBudgetActionResponse -> Lude.Text) (\s a -> s {budgetName = a} :: DeleteBudgetActionResponse)
{-# DEPRECATED drsBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAction :: Lens.Lens' DeleteBudgetActionResponse Action
drsAction = Lens.lens (action :: DeleteBudgetActionResponse -> Action) (\s a -> s {action = a} :: DeleteBudgetActionResponse)
{-# DEPRECATED drsAction "Use generic-lens or generic-optics with 'action' instead." #-}
