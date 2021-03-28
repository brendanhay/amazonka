{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateBudgetAction (..)
    , mkCreateBudgetAction
    -- ** Request lenses
    , cbaAccountId
    , cbaBudgetName
    , cbaNotificationType
    , cbaActionType
    , cbaActionThreshold
    , cbaDefinition
    , cbaExecutionRoleArn
    , cbaApprovalModel
    , cbaSubscribers

    -- * Destructuring the response
    , CreateBudgetActionResponse (..)
    , mkCreateBudgetActionResponse
    -- ** Response lenses
    , cbarrsAccountId
    , cbarrsBudgetName
    , cbarrsActionId
    , cbarrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateBudgetAction' smart constructor.
data CreateBudgetAction = CreateBudgetAction'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , notificationType :: Types.NotificationType
  , actionType :: Types.ActionType
    -- ^ The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition. 
  , actionThreshold :: Types.ActionThreshold
  , definition :: Types.Definition
  , executionRoleArn :: Types.ExecutionRoleArn
    -- ^ The role passed for action execution and reversion. Roles and actions must be in the same account. 
  , approvalModel :: Types.ApprovalModel
    -- ^ This specifies if the action needs manual or automatic approval. 
  , subscribers :: Core.NonEmpty Types.Subscriber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBudgetAction' value with any optional fields omitted.
mkCreateBudgetAction
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.NotificationType -- ^ 'notificationType'
    -> Types.ActionType -- ^ 'actionType'
    -> Types.ActionThreshold -- ^ 'actionThreshold'
    -> Types.Definition -- ^ 'definition'
    -> Types.ExecutionRoleArn -- ^ 'executionRoleArn'
    -> Types.ApprovalModel -- ^ 'approvalModel'
    -> Core.NonEmpty Types.Subscriber -- ^ 'subscribers'
    -> CreateBudgetAction
mkCreateBudgetAction accountId budgetName notificationType
  actionType actionThreshold definition executionRoleArn
  approvalModel subscribers
  = CreateBudgetAction'{accountId, budgetName, notificationType,
                        actionType, actionThreshold, definition, executionRoleArn,
                        approvalModel, subscribers}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaAccountId :: Lens.Lens' CreateBudgetAction Types.AccountId
cbaAccountId = Lens.field @"accountId"
{-# INLINEABLE cbaAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaBudgetName :: Lens.Lens' CreateBudgetAction Types.BudgetName
cbaBudgetName = Lens.field @"budgetName"
{-# INLINEABLE cbaBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaNotificationType :: Lens.Lens' CreateBudgetAction Types.NotificationType
cbaNotificationType = Lens.field @"notificationType"
{-# INLINEABLE cbaNotificationType #-}
{-# DEPRECATED notificationType "Use generic-lens or generic-optics with 'notificationType' instead"  #-}

-- | The type of action. This defines the type of tasks that can be carried out by this action. This field also determines the format for definition. 
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaActionType :: Lens.Lens' CreateBudgetAction Types.ActionType
cbaActionType = Lens.field @"actionType"
{-# INLINEABLE cbaActionType #-}
{-# DEPRECATED actionType "Use generic-lens or generic-optics with 'actionType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaActionThreshold :: Lens.Lens' CreateBudgetAction Types.ActionThreshold
cbaActionThreshold = Lens.field @"actionThreshold"
{-# INLINEABLE cbaActionThreshold #-}
{-# DEPRECATED actionThreshold "Use generic-lens or generic-optics with 'actionThreshold' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaDefinition :: Lens.Lens' CreateBudgetAction Types.Definition
cbaDefinition = Lens.field @"definition"
{-# INLINEABLE cbaDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The role passed for action execution and reversion. Roles and actions must be in the same account. 
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaExecutionRoleArn :: Lens.Lens' CreateBudgetAction Types.ExecutionRoleArn
cbaExecutionRoleArn = Lens.field @"executionRoleArn"
{-# INLINEABLE cbaExecutionRoleArn #-}
{-# DEPRECATED executionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead"  #-}

-- | This specifies if the action needs manual or automatic approval. 
--
-- /Note:/ Consider using 'approvalModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaApprovalModel :: Lens.Lens' CreateBudgetAction Types.ApprovalModel
cbaApprovalModel = Lens.field @"approvalModel"
{-# INLINEABLE cbaApprovalModel #-}
{-# DEPRECATED approvalModel "Use generic-lens or generic-optics with 'approvalModel' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbaSubscribers :: Lens.Lens' CreateBudgetAction (Core.NonEmpty Types.Subscriber)
cbaSubscribers = Lens.field @"subscribers"
{-# INLINEABLE cbaSubscribers #-}
{-# DEPRECATED subscribers "Use generic-lens or generic-optics with 'subscribers' instead"  #-}

instance Core.ToQuery CreateBudgetAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateBudgetAction where
        toHeaders CreateBudgetAction{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.CreateBudgetAction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateBudgetAction where
        toJSON CreateBudgetAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("NotificationType" Core..= notificationType),
                  Core.Just ("ActionType" Core..= actionType),
                  Core.Just ("ActionThreshold" Core..= actionThreshold),
                  Core.Just ("Definition" Core..= definition),
                  Core.Just ("ExecutionRoleArn" Core..= executionRoleArn),
                  Core.Just ("ApprovalModel" Core..= approvalModel),
                  Core.Just ("Subscribers" Core..= subscribers)])

instance Core.AWSRequest CreateBudgetAction where
        type Rs CreateBudgetAction = CreateBudgetActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateBudgetActionResponse' Core.<$>
                   (x Core..: "AccountId") Core.<*> x Core..: "BudgetName" Core.<*>
                     x Core..: "ActionId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateBudgetActionResponse' smart constructor.
data CreateBudgetActionResponse = CreateBudgetActionResponse'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , actionId :: Types.ActionId
    -- ^ A system-generated universally unique identifier (UUID) for the action. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBudgetActionResponse' value with any optional fields omitted.
mkCreateBudgetActionResponse
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.ActionId -- ^ 'actionId'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateBudgetActionResponse
mkCreateBudgetActionResponse accountId budgetName actionId
  responseStatus
  = CreateBudgetActionResponse'{accountId, budgetName, actionId,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbarrsAccountId :: Lens.Lens' CreateBudgetActionResponse Types.AccountId
cbarrsAccountId = Lens.field @"accountId"
{-# INLINEABLE cbarrsAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbarrsBudgetName :: Lens.Lens' CreateBudgetActionResponse Types.BudgetName
cbarrsBudgetName = Lens.field @"budgetName"
{-# INLINEABLE cbarrsBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | A system-generated universally unique identifier (UUID) for the action. 
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbarrsActionId :: Lens.Lens' CreateBudgetActionResponse Types.ActionId
cbarrsActionId = Lens.field @"actionId"
{-# INLINEABLE cbarrsActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbarrsResponseStatus :: Lens.Lens' CreateBudgetActionResponse Core.Int
cbarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cbarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
