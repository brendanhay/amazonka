{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DeleteBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a budget. You can delete your budget at any time.
--
-- /Important:/ Deleting a budget also deletes the notifications and subscribers that are associated with that budget.
module Network.AWS.Budgets.DeleteBudget
  ( -- * Creating a request
    DeleteBudget (..),
    mkDeleteBudget,

    -- ** Request lenses
    dAccountId,
    dBudgetName,

    -- * Destructuring the response
    DeleteBudgetResponse (..),
    mkDeleteBudgetResponse,

    -- ** Response lenses
    delrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of DeleteBudget
--
-- /See:/ 'mkDeleteBudget' smart constructor.
data DeleteBudget = DeleteBudget'
  { accountId :: Lude.Text,
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

-- | Creates a value of 'DeleteBudget' with the minimum fields required to make a request.
--
-- * 'accountId' - The @accountId@ that is associated with the budget that you want to delete.
-- * 'budgetName' - The name of the budget that you want to delete.
mkDeleteBudget ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  DeleteBudget
mkDeleteBudget pAccountId_ pBudgetName_ =
  DeleteBudget' {accountId = pAccountId_, budgetName = pBudgetName_}

-- | The @accountId@ that is associated with the budget that you want to delete.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccountId :: Lens.Lens' DeleteBudget Lude.Text
dAccountId = Lens.lens (accountId :: DeleteBudget -> Lude.Text) (\s a -> s {accountId = a} :: DeleteBudget)
{-# DEPRECATED dAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget that you want to delete.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBudgetName :: Lens.Lens' DeleteBudget Lude.Text
dBudgetName = Lens.lens (budgetName :: DeleteBudget -> Lude.Text) (\s a -> s {budgetName = a} :: DeleteBudget)
{-# DEPRECATED dBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Lude.AWSRequest DeleteBudget where
  type Rs DeleteBudget = DeleteBudgetResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteBudgetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBudget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.DeleteBudget" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBudget where
  toJSON DeleteBudget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName)
          ]
      )

instance Lude.ToPath DeleteBudget where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBudget where
  toQuery = Lude.const Lude.mempty

-- | Response of DeleteBudget
--
-- /See:/ 'mkDeleteBudgetResponse' smart constructor.
newtype DeleteBudgetResponse = DeleteBudgetResponse'
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

-- | Creates a value of 'DeleteBudgetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteBudgetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBudgetResponse
mkDeleteBudgetResponse pResponseStatus_ =
  DeleteBudgetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteBudgetResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteBudgetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBudgetResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
