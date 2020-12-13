{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Action
  ( Action (..),

    -- * Smart constructor
    mkAction,

    -- * Lenses
    aStatus,
    aDefinition,
    aExecutionRoleARN,
    aActionId,
    aActionThreshold,
    aBudgetName,
    aNotificationType,
    aApprovalModel,
    aActionType,
    aSubscribers,
  )
where

import Network.AWS.Budgets.Types.ActionStatus
import Network.AWS.Budgets.Types.ActionThreshold
import Network.AWS.Budgets.Types.ActionType
import Network.AWS.Budgets.Types.ApprovalModel
import Network.AWS.Budgets.Types.Definition
import Network.AWS.Budgets.Types.NotificationType
import Network.AWS.Budgets.Types.Subscriber
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A budget action resource.
--
-- /See:/ 'mkAction' smart constructor.
data Action = Action'
  { -- | The status of action.
    status :: ActionStatus,
    -- | Where you specify all of the type-specific parameters.
    definition :: Definition,
    -- | The role passed for action execution and reversion. Roles and actions must be in the same account.
    executionRoleARN :: Lude.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Lude.Text,
    -- | The trigger threshold of the action.
    actionThreshold :: ActionThreshold,
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

-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- * 'status' - The status of action.
-- * 'definition' - Where you specify all of the type-specific parameters.
-- * 'executionRoleARN' - The role passed for action execution and reversion. Roles and actions must be in the same account.
-- * 'actionId' - A system-generated universally unique identifier (UUID) for the action.
-- * 'actionThreshold' - The trigger threshold of the action.
-- * 'budgetName' -
-- * 'notificationType' -
-- * 'approvalModel' - This specifies if the action needs manual or automatic approval.
-- * 'actionType' - The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
-- * 'subscribers' -
mkAction ::
  -- | 'status'
  ActionStatus ->
  -- | 'definition'
  Definition ->
  -- | 'executionRoleARN'
  Lude.Text ->
  -- | 'actionId'
  Lude.Text ->
  -- | 'actionThreshold'
  ActionThreshold ->
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
  Action
mkAction
  pStatus_
  pDefinition_
  pExecutionRoleARN_
  pActionId_
  pActionThreshold_
  pBudgetName_
  pNotificationType_
  pApprovalModel_
  pActionType_
  pSubscribers_ =
    Action'
      { status = pStatus_,
        definition = pDefinition_,
        executionRoleARN = pExecutionRoleARN_,
        actionId = pActionId_,
        actionThreshold = pActionThreshold_,
        budgetName = pBudgetName_,
        notificationType = pNotificationType_,
        approvalModel = pApprovalModel_,
        actionType = pActionType_,
        subscribers = pSubscribers_
      }

-- | The status of action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatus :: Lens.Lens' Action ActionStatus
aStatus = Lens.lens (status :: Action -> ActionStatus) (\s a -> s {status = a} :: Action)
{-# DEPRECATED aStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Where you specify all of the type-specific parameters.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDefinition :: Lens.Lens' Action Definition
aDefinition = Lens.lens (definition :: Action -> Definition) (\s a -> s {definition = a} :: Action)
{-# DEPRECATED aDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The role passed for action execution and reversion. Roles and actions must be in the same account.
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aExecutionRoleARN :: Lens.Lens' Action Lude.Text
aExecutionRoleARN = Lens.lens (executionRoleARN :: Action -> Lude.Text) (\s a -> s {executionRoleARN = a} :: Action)
{-# DEPRECATED aExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActionId :: Lens.Lens' Action Lude.Text
aActionId = Lens.lens (actionId :: Action -> Lude.Text) (\s a -> s {actionId = a} :: Action)
{-# DEPRECATED aActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The trigger threshold of the action.
--
-- /Note:/ Consider using 'actionThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActionThreshold :: Lens.Lens' Action ActionThreshold
aActionThreshold = Lens.lens (actionThreshold :: Action -> ActionThreshold) (\s a -> s {actionThreshold = a} :: Action)
{-# DEPRECATED aActionThreshold "Use generic-lens or generic-optics with 'actionThreshold' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBudgetName :: Lens.Lens' Action Lude.Text
aBudgetName = Lens.lens (budgetName :: Action -> Lude.Text) (\s a -> s {budgetName = a} :: Action)
{-# DEPRECATED aBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNotificationType :: Lens.Lens' Action NotificationType
aNotificationType = Lens.lens (notificationType :: Action -> NotificationType) (\s a -> s {notificationType = a} :: Action)
{-# DEPRECATED aNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | This specifies if the action needs manual or automatic approval.
--
-- /Note:/ Consider using 'approvalModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aApprovalModel :: Lens.Lens' Action ApprovalModel
aApprovalModel = Lens.lens (approvalModel :: Action -> ApprovalModel) (\s a -> s {approvalModel = a} :: Action)
{-# DEPRECATED aApprovalModel "Use generic-lens or generic-optics with 'approvalModel' instead." #-}

-- | The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActionType :: Lens.Lens' Action ActionType
aActionType = Lens.lens (actionType :: Action -> ActionType) (\s a -> s {actionType = a} :: Action)
{-# DEPRECATED aActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSubscribers :: Lens.Lens' Action (Lude.NonEmpty Subscriber)
aSubscribers = Lens.lens (subscribers :: Action -> Lude.NonEmpty Subscriber) (\s a -> s {subscribers = a} :: Action)
{-# DEPRECATED aSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

instance Lude.FromJSON Action where
  parseJSON =
    Lude.withObject
      "Action"
      ( \x ->
          Action'
            Lude.<$> (x Lude..: "Status")
            Lude.<*> (x Lude..: "Definition")
            Lude.<*> (x Lude..: "ExecutionRoleArn")
            Lude.<*> (x Lude..: "ActionId")
            Lude.<*> (x Lude..: "ActionThreshold")
            Lude.<*> (x Lude..: "BudgetName")
            Lude.<*> (x Lude..: "NotificationType")
            Lude.<*> (x Lude..: "ApprovalModel")
            Lude.<*> (x Lude..: "ActionType")
            Lude.<*> (x Lude..: "Subscribers")
      )
