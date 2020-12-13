{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.UpdateBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a budget action.
module Network.AWS.Budgets.UpdateBudgetAction
  ( -- * Creating a request
    UpdateBudgetAction (..),
    mkUpdateBudgetAction,

    -- ** Request lenses
    ubaDefinition,
    ubaExecutionRoleARN,
    ubaActionId,
    ubaActionThreshold,
    ubaAccountId,
    ubaBudgetName,
    ubaNotificationType,
    ubaApprovalModel,
    ubaSubscribers,

    -- * Destructuring the response
    UpdateBudgetActionResponse (..),
    mkUpdateBudgetActionResponse,

    -- ** Response lenses
    ubarsNewAction,
    ubarsAccountId,
    ubarsBudgetName,
    ubarsOldAction,
    ubarsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateBudgetAction' smart constructor.
data UpdateBudgetAction = UpdateBudgetAction'
  { definition :: Lude.Maybe Definition,
    -- | The role passed for action execution and reversion. Roles and actions must be in the same account.
    executionRoleARN :: Lude.Maybe Lude.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Lude.Text,
    actionThreshold :: Lude.Maybe ActionThreshold,
    accountId :: Lude.Text,
    budgetName :: Lude.Text,
    notificationType :: Lude.Maybe NotificationType,
    -- | This specifies if the action needs manual or automatic approval.
    approvalModel :: Lude.Maybe ApprovalModel,
    subscribers :: Lude.Maybe (Lude.NonEmpty Subscriber)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBudgetAction' with the minimum fields required to make a request.
--
-- * 'definition' -
-- * 'executionRoleARN' - The role passed for action execution and reversion. Roles and actions must be in the same account.
-- * 'actionId' - A system-generated universally unique identifier (UUID) for the action.
-- * 'actionThreshold' -
-- * 'accountId' -
-- * 'budgetName' -
-- * 'notificationType' -
-- * 'approvalModel' - This specifies if the action needs manual or automatic approval.
-- * 'subscribers' -
mkUpdateBudgetAction ::
  -- | 'actionId'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  UpdateBudgetAction
mkUpdateBudgetAction pActionId_ pAccountId_ pBudgetName_ =
  UpdateBudgetAction'
    { definition = Lude.Nothing,
      executionRoleARN = Lude.Nothing,
      actionId = pActionId_,
      actionThreshold = Lude.Nothing,
      accountId = pAccountId_,
      budgetName = pBudgetName_,
      notificationType = Lude.Nothing,
      approvalModel = Lude.Nothing,
      subscribers = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaDefinition :: Lens.Lens' UpdateBudgetAction (Lude.Maybe Definition)
ubaDefinition = Lens.lens (definition :: UpdateBudgetAction -> Lude.Maybe Definition) (\s a -> s {definition = a} :: UpdateBudgetAction)
{-# DEPRECATED ubaDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The role passed for action execution and reversion. Roles and actions must be in the same account.
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaExecutionRoleARN :: Lens.Lens' UpdateBudgetAction (Lude.Maybe Lude.Text)
ubaExecutionRoleARN = Lens.lens (executionRoleARN :: UpdateBudgetAction -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleARN = a} :: UpdateBudgetAction)
{-# DEPRECATED ubaExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaActionId :: Lens.Lens' UpdateBudgetAction Lude.Text
ubaActionId = Lens.lens (actionId :: UpdateBudgetAction -> Lude.Text) (\s a -> s {actionId = a} :: UpdateBudgetAction)
{-# DEPRECATED ubaActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaActionThreshold :: Lens.Lens' UpdateBudgetAction (Lude.Maybe ActionThreshold)
ubaActionThreshold = Lens.lens (actionThreshold :: UpdateBudgetAction -> Lude.Maybe ActionThreshold) (\s a -> s {actionThreshold = a} :: UpdateBudgetAction)
{-# DEPRECATED ubaActionThreshold "Use generic-lens or generic-optics with 'actionThreshold' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaAccountId :: Lens.Lens' UpdateBudgetAction Lude.Text
ubaAccountId = Lens.lens (accountId :: UpdateBudgetAction -> Lude.Text) (\s a -> s {accountId = a} :: UpdateBudgetAction)
{-# DEPRECATED ubaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaBudgetName :: Lens.Lens' UpdateBudgetAction Lude.Text
ubaBudgetName = Lens.lens (budgetName :: UpdateBudgetAction -> Lude.Text) (\s a -> s {budgetName = a} :: UpdateBudgetAction)
{-# DEPRECATED ubaBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaNotificationType :: Lens.Lens' UpdateBudgetAction (Lude.Maybe NotificationType)
ubaNotificationType = Lens.lens (notificationType :: UpdateBudgetAction -> Lude.Maybe NotificationType) (\s a -> s {notificationType = a} :: UpdateBudgetAction)
{-# DEPRECATED ubaNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | This specifies if the action needs manual or automatic approval.
--
-- /Note:/ Consider using 'approvalModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaApprovalModel :: Lens.Lens' UpdateBudgetAction (Lude.Maybe ApprovalModel)
ubaApprovalModel = Lens.lens (approvalModel :: UpdateBudgetAction -> Lude.Maybe ApprovalModel) (\s a -> s {approvalModel = a} :: UpdateBudgetAction)
{-# DEPRECATED ubaApprovalModel "Use generic-lens or generic-optics with 'approvalModel' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaSubscribers :: Lens.Lens' UpdateBudgetAction (Lude.Maybe (Lude.NonEmpty Subscriber))
ubaSubscribers = Lens.lens (subscribers :: UpdateBudgetAction -> Lude.Maybe (Lude.NonEmpty Subscriber)) (\s a -> s {subscribers = a} :: UpdateBudgetAction)
{-# DEPRECATED ubaSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

instance Lude.AWSRequest UpdateBudgetAction where
  type Rs UpdateBudgetAction = UpdateBudgetActionResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateBudgetActionResponse'
            Lude.<$> (x Lude..:> "NewAction")
            Lude.<*> (x Lude..:> "AccountId")
            Lude.<*> (x Lude..:> "BudgetName")
            Lude.<*> (x Lude..:> "OldAction")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateBudgetAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.UpdateBudgetAction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateBudgetAction where
  toJSON UpdateBudgetAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Definition" Lude..=) Lude.<$> definition,
            ("ExecutionRoleArn" Lude..=) Lude.<$> executionRoleARN,
            Lude.Just ("ActionId" Lude..= actionId),
            ("ActionThreshold" Lude..=) Lude.<$> actionThreshold,
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName),
            ("NotificationType" Lude..=) Lude.<$> notificationType,
            ("ApprovalModel" Lude..=) Lude.<$> approvalModel,
            ("Subscribers" Lude..=) Lude.<$> subscribers
          ]
      )

instance Lude.ToPath UpdateBudgetAction where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateBudgetAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateBudgetActionResponse' smart constructor.
data UpdateBudgetActionResponse = UpdateBudgetActionResponse'
  { -- | The updated action resource information.
    newAction :: Action,
    accountId :: Lude.Text,
    budgetName :: Lude.Text,
    -- | The previous action resource information.
    oldAction :: Action,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBudgetActionResponse' with the minimum fields required to make a request.
--
-- * 'newAction' - The updated action resource information.
-- * 'accountId' -
-- * 'budgetName' -
-- * 'oldAction' - The previous action resource information.
-- * 'responseStatus' - The response status code.
mkUpdateBudgetActionResponse ::
  -- | 'newAction'
  Action ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'oldAction'
  Action ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateBudgetActionResponse
mkUpdateBudgetActionResponse
  pNewAction_
  pAccountId_
  pBudgetName_
  pOldAction_
  pResponseStatus_ =
    UpdateBudgetActionResponse'
      { newAction = pNewAction_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        oldAction = pOldAction_,
        responseStatus = pResponseStatus_
      }

-- | The updated action resource information.
--
-- /Note:/ Consider using 'newAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarsNewAction :: Lens.Lens' UpdateBudgetActionResponse Action
ubarsNewAction = Lens.lens (newAction :: UpdateBudgetActionResponse -> Action) (\s a -> s {newAction = a} :: UpdateBudgetActionResponse)
{-# DEPRECATED ubarsNewAction "Use generic-lens or generic-optics with 'newAction' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarsAccountId :: Lens.Lens' UpdateBudgetActionResponse Lude.Text
ubarsAccountId = Lens.lens (accountId :: UpdateBudgetActionResponse -> Lude.Text) (\s a -> s {accountId = a} :: UpdateBudgetActionResponse)
{-# DEPRECATED ubarsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarsBudgetName :: Lens.Lens' UpdateBudgetActionResponse Lude.Text
ubarsBudgetName = Lens.lens (budgetName :: UpdateBudgetActionResponse -> Lude.Text) (\s a -> s {budgetName = a} :: UpdateBudgetActionResponse)
{-# DEPRECATED ubarsBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The previous action resource information.
--
-- /Note:/ Consider using 'oldAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarsOldAction :: Lens.Lens' UpdateBudgetActionResponse Action
ubarsOldAction = Lens.lens (oldAction :: UpdateBudgetActionResponse -> Action) (\s a -> s {oldAction = a} :: UpdateBudgetActionResponse)
{-# DEPRECATED ubarsOldAction "Use generic-lens or generic-optics with 'oldAction' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarsResponseStatus :: Lens.Lens' UpdateBudgetActionResponse Lude.Int
ubarsResponseStatus = Lens.lens (responseStatus :: UpdateBudgetActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateBudgetActionResponse)
{-# DEPRECATED ubarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
