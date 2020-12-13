{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.CreateBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a budget action.
module Network.AWS.Budgets.CreateBudgetAction
  ( -- * Creating a request
    CreateBudgetAction (..),
    mkCreateBudgetAction,

    -- ** Request lenses
    cbaDefinition,
    cbaExecutionRoleARN,
    cbaActionThreshold,
    cbaAccountId,
    cbaBudgetName,
    cbaNotificationType,
    cbaApprovalModel,
    cbaActionType,
    cbaSubscribers,

    -- * Destructuring the response
    CreateBudgetActionResponse (..),
    mkCreateBudgetActionResponse,

    -- ** Response lenses
    cbarsActionId,
    cbarsAccountId,
    cbarsBudgetName,
    cbarsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateBudgetAction' smart constructor.
data CreateBudgetAction = CreateBudgetAction'
  { definition :: Definition,
    -- | The role passed for action execution and reversion. Roles and actions must be in the same account.
    executionRoleARN :: Lude.Text,
    actionThreshold :: ActionThreshold,
    accountId :: Lude.Text,
    budgetName :: Lude.Text,
    notificationType :: NotificationType,
    -- | This specifies if the action needs manual or automatic approval.
    approvalModel :: ApprovalModel,
    -- | The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
    actionType :: ActionType,
    subscribers :: Lude.NonEmpty Subscriber
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBudgetAction' with the minimum fields required to make a request.
--
-- * 'definition' -
-- * 'executionRoleARN' - The role passed for action execution and reversion. Roles and actions must be in the same account.
-- * 'actionThreshold' -
-- * 'accountId' -
-- * 'budgetName' -
-- * 'notificationType' -
-- * 'approvalModel' - This specifies if the action needs manual or automatic approval.
-- * 'actionType' - The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
-- * 'subscribers' -
mkCreateBudgetAction ::
  -- | 'definition'
  Definition ->
  -- | 'executionRoleARN'
  Lude.Text ->
  -- | 'actionThreshold'
  ActionThreshold ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'notificationType'
  NotificationType ->
  -- | 'approvalModel'
  ApprovalModel ->
  -- | 'actionType'
  ActionType ->
  -- | 'subscribers'
  Lude.NonEmpty Subscriber ->
  CreateBudgetAction
mkCreateBudgetAction
  pDefinition_
  pExecutionRoleARN_
  pActionThreshold_
  pAccountId_
  pBudgetName_
  pNotificationType_
  pApprovalModel_
  pActionType_
  pSubscribers_ =
    CreateBudgetAction'
      { definition = pDefinition_,
        executionRoleARN = pExecutionRoleARN_,
        actionThreshold = pActionThreshold_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        notificationType = pNotificationType_,
        approvalModel = pApprovalModel_,
        actionType = pActionType_,
        subscribers = pSubscribers_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaDefinition :: Lens.Lens' CreateBudgetAction Definition
cbaDefinition = Lens.lens (definition :: CreateBudgetAction -> Definition) (\s a -> s {definition = a} :: CreateBudgetAction)
{-# DEPRECATED cbaDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The role passed for action execution and reversion. Roles and actions must be in the same account.
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaExecutionRoleARN :: Lens.Lens' CreateBudgetAction Lude.Text
cbaExecutionRoleARN = Lens.lens (executionRoleARN :: CreateBudgetAction -> Lude.Text) (\s a -> s {executionRoleARN = a} :: CreateBudgetAction)
{-# DEPRECATED cbaExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaActionThreshold :: Lens.Lens' CreateBudgetAction ActionThreshold
cbaActionThreshold = Lens.lens (actionThreshold :: CreateBudgetAction -> ActionThreshold) (\s a -> s {actionThreshold = a} :: CreateBudgetAction)
{-# DEPRECATED cbaActionThreshold "Use generic-lens or generic-optics with 'actionThreshold' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaAccountId :: Lens.Lens' CreateBudgetAction Lude.Text
cbaAccountId = Lens.lens (accountId :: CreateBudgetAction -> Lude.Text) (\s a -> s {accountId = a} :: CreateBudgetAction)
{-# DEPRECATED cbaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaBudgetName :: Lens.Lens' CreateBudgetAction Lude.Text
cbaBudgetName = Lens.lens (budgetName :: CreateBudgetAction -> Lude.Text) (\s a -> s {budgetName = a} :: CreateBudgetAction)
{-# DEPRECATED cbaBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaNotificationType :: Lens.Lens' CreateBudgetAction NotificationType
cbaNotificationType = Lens.lens (notificationType :: CreateBudgetAction -> NotificationType) (\s a -> s {notificationType = a} :: CreateBudgetAction)
{-# DEPRECATED cbaNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | This specifies if the action needs manual or automatic approval.
--
-- /Note:/ Consider using 'approvalModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaApprovalModel :: Lens.Lens' CreateBudgetAction ApprovalModel
cbaApprovalModel = Lens.lens (approvalModel :: CreateBudgetAction -> ApprovalModel) (\s a -> s {approvalModel = a} :: CreateBudgetAction)
{-# DEPRECATED cbaApprovalModel "Use generic-lens or generic-optics with 'approvalModel' instead." #-}

-- | The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaActionType :: Lens.Lens' CreateBudgetAction ActionType
cbaActionType = Lens.lens (actionType :: CreateBudgetAction -> ActionType) (\s a -> s {actionType = a} :: CreateBudgetAction)
{-# DEPRECATED cbaActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaSubscribers :: Lens.Lens' CreateBudgetAction (Lude.NonEmpty Subscriber)
cbaSubscribers = Lens.lens (subscribers :: CreateBudgetAction -> Lude.NonEmpty Subscriber) (\s a -> s {subscribers = a} :: CreateBudgetAction)
{-# DEPRECATED cbaSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

instance Lude.AWSRequest CreateBudgetAction where
  type Rs CreateBudgetAction = CreateBudgetActionResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateBudgetActionResponse'
            Lude.<$> (x Lude..:> "ActionId")
            Lude.<*> (x Lude..:> "AccountId")
            Lude.<*> (x Lude..:> "BudgetName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateBudgetAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.CreateBudgetAction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBudgetAction where
  toJSON CreateBudgetAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Definition" Lude..= definition),
            Lude.Just ("ExecutionRoleArn" Lude..= executionRoleARN),
            Lude.Just ("ActionThreshold" Lude..= actionThreshold),
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName),
            Lude.Just ("NotificationType" Lude..= notificationType),
            Lude.Just ("ApprovalModel" Lude..= approvalModel),
            Lude.Just ("ActionType" Lude..= actionType),
            Lude.Just ("Subscribers" Lude..= subscribers)
          ]
      )

instance Lude.ToPath CreateBudgetAction where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateBudgetAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateBudgetActionResponse' smart constructor.
data CreateBudgetActionResponse = CreateBudgetActionResponse'
  { -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Lude.Text,
    accountId :: Lude.Text,
    budgetName :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBudgetActionResponse' with the minimum fields required to make a request.
--
-- * 'actionId' - A system-generated universally unique identifier (UUID) for the action.
-- * 'accountId' -
-- * 'budgetName' -
-- * 'responseStatus' - The response status code.
mkCreateBudgetActionResponse ::
  -- | 'actionId'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateBudgetActionResponse
mkCreateBudgetActionResponse
  pActionId_
  pAccountId_
  pBudgetName_
  pResponseStatus_ =
    CreateBudgetActionResponse'
      { actionId = pActionId_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        responseStatus = pResponseStatus_
      }

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbarsActionId :: Lens.Lens' CreateBudgetActionResponse Lude.Text
cbarsActionId = Lens.lens (actionId :: CreateBudgetActionResponse -> Lude.Text) (\s a -> s {actionId = a} :: CreateBudgetActionResponse)
{-# DEPRECATED cbarsActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbarsAccountId :: Lens.Lens' CreateBudgetActionResponse Lude.Text
cbarsAccountId = Lens.lens (accountId :: CreateBudgetActionResponse -> Lude.Text) (\s a -> s {accountId = a} :: CreateBudgetActionResponse)
{-# DEPRECATED cbarsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbarsBudgetName :: Lens.Lens' CreateBudgetActionResponse Lude.Text
cbarsBudgetName = Lens.lens (budgetName :: CreateBudgetActionResponse -> Lude.Text) (\s a -> s {budgetName = a} :: CreateBudgetActionResponse)
{-# DEPRECATED cbarsBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbarsResponseStatus :: Lens.Lens' CreateBudgetActionResponse Lude.Int
cbarsResponseStatus = Lens.lens (responseStatus :: CreateBudgetActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBudgetActionResponse)
{-# DEPRECATED cbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
