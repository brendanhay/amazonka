{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.ExecuteBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes a budget action.
module Network.AWS.Budgets.ExecuteBudgetAction
  ( -- * Creating a request
    ExecuteBudgetAction (..),
    mkExecuteBudgetAction,

    -- ** Request lenses
    ebaExecutionType,
    ebaActionId,
    ebaAccountId,
    ebaBudgetName,

    -- * Destructuring the response
    ExecuteBudgetActionResponse (..),
    mkExecuteBudgetActionResponse,

    -- ** Response lenses
    ebarsExecutionType,
    ebarsActionId,
    ebarsAccountId,
    ebarsBudgetName,
    ebarsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExecuteBudgetAction' smart constructor.
data ExecuteBudgetAction = ExecuteBudgetAction'
  { -- | The type of execution.
    executionType :: ExecutionType,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Lude.Text,
    accountId :: Lude.Text,
    budgetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecuteBudgetAction' with the minimum fields required to make a request.
--
-- * 'executionType' - The type of execution.
-- * 'actionId' - A system-generated universally unique identifier (UUID) for the action.
-- * 'accountId' -
-- * 'budgetName' -
mkExecuteBudgetAction ::
  -- | 'executionType'
  ExecutionType ->
  -- | 'actionId'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  ExecuteBudgetAction
mkExecuteBudgetAction
  pExecutionType_
  pActionId_
  pAccountId_
  pBudgetName_ =
    ExecuteBudgetAction'
      { executionType = pExecutionType_,
        actionId = pActionId_,
        accountId = pAccountId_,
        budgetName = pBudgetName_
      }

-- | The type of execution.
--
-- /Note:/ Consider using 'executionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebaExecutionType :: Lens.Lens' ExecuteBudgetAction ExecutionType
ebaExecutionType = Lens.lens (executionType :: ExecuteBudgetAction -> ExecutionType) (\s a -> s {executionType = a} :: ExecuteBudgetAction)
{-# DEPRECATED ebaExecutionType "Use generic-lens or generic-optics with 'executionType' instead." #-}

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebaActionId :: Lens.Lens' ExecuteBudgetAction Lude.Text
ebaActionId = Lens.lens (actionId :: ExecuteBudgetAction -> Lude.Text) (\s a -> s {actionId = a} :: ExecuteBudgetAction)
{-# DEPRECATED ebaActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebaAccountId :: Lens.Lens' ExecuteBudgetAction Lude.Text
ebaAccountId = Lens.lens (accountId :: ExecuteBudgetAction -> Lude.Text) (\s a -> s {accountId = a} :: ExecuteBudgetAction)
{-# DEPRECATED ebaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebaBudgetName :: Lens.Lens' ExecuteBudgetAction Lude.Text
ebaBudgetName = Lens.lens (budgetName :: ExecuteBudgetAction -> Lude.Text) (\s a -> s {budgetName = a} :: ExecuteBudgetAction)
{-# DEPRECATED ebaBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Lude.AWSRequest ExecuteBudgetAction where
  type Rs ExecuteBudgetAction = ExecuteBudgetActionResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ExecuteBudgetActionResponse'
            Lude.<$> (x Lude..:> "ExecutionType")
            Lude.<*> (x Lude..:> "ActionId")
            Lude.<*> (x Lude..:> "AccountId")
            Lude.<*> (x Lude..:> "BudgetName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExecuteBudgetAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.ExecuteBudgetAction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExecuteBudgetAction where
  toJSON ExecuteBudgetAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ExecutionType" Lude..= executionType),
            Lude.Just ("ActionId" Lude..= actionId),
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName)
          ]
      )

instance Lude.ToPath ExecuteBudgetAction where
  toPath = Lude.const "/"

instance Lude.ToQuery ExecuteBudgetAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkExecuteBudgetActionResponse' smart constructor.
data ExecuteBudgetActionResponse = ExecuteBudgetActionResponse'
  { -- | The type of execution.
    executionType :: ExecutionType,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Lude.Text,
    accountId :: Lude.Text,
    budgetName :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecuteBudgetActionResponse' with the minimum fields required to make a request.
--
-- * 'executionType' - The type of execution.
-- * 'actionId' - A system-generated universally unique identifier (UUID) for the action.
-- * 'accountId' -
-- * 'budgetName' -
-- * 'responseStatus' - The response status code.
mkExecuteBudgetActionResponse ::
  -- | 'executionType'
  ExecutionType ->
  -- | 'actionId'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  ExecuteBudgetActionResponse
mkExecuteBudgetActionResponse
  pExecutionType_
  pActionId_
  pAccountId_
  pBudgetName_
  pResponseStatus_ =
    ExecuteBudgetActionResponse'
      { executionType = pExecutionType_,
        actionId = pActionId_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        responseStatus = pResponseStatus_
      }

-- | The type of execution.
--
-- /Note:/ Consider using 'executionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebarsExecutionType :: Lens.Lens' ExecuteBudgetActionResponse ExecutionType
ebarsExecutionType = Lens.lens (executionType :: ExecuteBudgetActionResponse -> ExecutionType) (\s a -> s {executionType = a} :: ExecuteBudgetActionResponse)
{-# DEPRECATED ebarsExecutionType "Use generic-lens or generic-optics with 'executionType' instead." #-}

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebarsActionId :: Lens.Lens' ExecuteBudgetActionResponse Lude.Text
ebarsActionId = Lens.lens (actionId :: ExecuteBudgetActionResponse -> Lude.Text) (\s a -> s {actionId = a} :: ExecuteBudgetActionResponse)
{-# DEPRECATED ebarsActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebarsAccountId :: Lens.Lens' ExecuteBudgetActionResponse Lude.Text
ebarsAccountId = Lens.lens (accountId :: ExecuteBudgetActionResponse -> Lude.Text) (\s a -> s {accountId = a} :: ExecuteBudgetActionResponse)
{-# DEPRECATED ebarsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebarsBudgetName :: Lens.Lens' ExecuteBudgetActionResponse Lude.Text
ebarsBudgetName = Lens.lens (budgetName :: ExecuteBudgetActionResponse -> Lude.Text) (\s a -> s {budgetName = a} :: ExecuteBudgetActionResponse)
{-# DEPRECATED ebarsBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebarsResponseStatus :: Lens.Lens' ExecuteBudgetActionResponse Lude.Int
ebarsResponseStatus = Lens.lens (responseStatus :: ExecuteBudgetActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExecuteBudgetActionResponse)
{-# DEPRECATED ebarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
