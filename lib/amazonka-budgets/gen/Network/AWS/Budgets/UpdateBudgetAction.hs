{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateBudgetAction (..)
    , mkUpdateBudgetAction
    -- ** Request lenses
    , ubaAccountId
    , ubaBudgetName
    , ubaActionId
    , ubaActionThreshold
    , ubaApprovalModel
    , ubaDefinition
    , ubaExecutionRoleArn
    , ubaNotificationType
    , ubaSubscribers

    -- * Destructuring the response
    , UpdateBudgetActionResponse (..)
    , mkUpdateBudgetActionResponse
    -- ** Response lenses
    , ubarrsAccountId
    , ubarrsBudgetName
    , ubarrsOldAction
    , ubarrsNewAction
    , ubarrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateBudgetAction' smart constructor.
data UpdateBudgetAction = UpdateBudgetAction'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , actionId :: Types.ActionId
    -- ^ A system-generated universally unique identifier (UUID) for the action. 
  , actionThreshold :: Core.Maybe Types.ActionThreshold
  , approvalModel :: Core.Maybe Types.ApprovalModel
    -- ^ This specifies if the action needs manual or automatic approval. 
  , definition :: Core.Maybe Types.Definition
  , executionRoleArn :: Core.Maybe Types.ExecutionRoleArn
    -- ^ The role passed for action execution and reversion. Roles and actions must be in the same account. 
  , notificationType :: Core.Maybe Types.NotificationType
  , subscribers :: Core.Maybe (Core.NonEmpty Types.Subscriber)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBudgetAction' value with any optional fields omitted.
mkUpdateBudgetAction
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.ActionId -- ^ 'actionId'
    -> UpdateBudgetAction
mkUpdateBudgetAction accountId budgetName actionId
  = UpdateBudgetAction'{accountId, budgetName, actionId,
                        actionThreshold = Core.Nothing, approvalModel = Core.Nothing,
                        definition = Core.Nothing, executionRoleArn = Core.Nothing,
                        notificationType = Core.Nothing, subscribers = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaAccountId :: Lens.Lens' UpdateBudgetAction Types.AccountId
ubaAccountId = Lens.field @"accountId"
{-# INLINEABLE ubaAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaBudgetName :: Lens.Lens' UpdateBudgetAction Types.BudgetName
ubaBudgetName = Lens.field @"budgetName"
{-# INLINEABLE ubaBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | A system-generated universally unique identifier (UUID) for the action. 
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaActionId :: Lens.Lens' UpdateBudgetAction Types.ActionId
ubaActionId = Lens.field @"actionId"
{-# INLINEABLE ubaActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaActionThreshold :: Lens.Lens' UpdateBudgetAction (Core.Maybe Types.ActionThreshold)
ubaActionThreshold = Lens.field @"actionThreshold"
{-# INLINEABLE ubaActionThreshold #-}
{-# DEPRECATED actionThreshold "Use generic-lens or generic-optics with 'actionThreshold' instead"  #-}

-- | This specifies if the action needs manual or automatic approval. 
--
-- /Note:/ Consider using 'approvalModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaApprovalModel :: Lens.Lens' UpdateBudgetAction (Core.Maybe Types.ApprovalModel)
ubaApprovalModel = Lens.field @"approvalModel"
{-# INLINEABLE ubaApprovalModel #-}
{-# DEPRECATED approvalModel "Use generic-lens or generic-optics with 'approvalModel' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaDefinition :: Lens.Lens' UpdateBudgetAction (Core.Maybe Types.Definition)
ubaDefinition = Lens.field @"definition"
{-# INLINEABLE ubaDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The role passed for action execution and reversion. Roles and actions must be in the same account. 
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaExecutionRoleArn :: Lens.Lens' UpdateBudgetAction (Core.Maybe Types.ExecutionRoleArn)
ubaExecutionRoleArn = Lens.field @"executionRoleArn"
{-# INLINEABLE ubaExecutionRoleArn #-}
{-# DEPRECATED executionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaNotificationType :: Lens.Lens' UpdateBudgetAction (Core.Maybe Types.NotificationType)
ubaNotificationType = Lens.field @"notificationType"
{-# INLINEABLE ubaNotificationType #-}
{-# DEPRECATED notificationType "Use generic-lens or generic-optics with 'notificationType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubaSubscribers :: Lens.Lens' UpdateBudgetAction (Core.Maybe (Core.NonEmpty Types.Subscriber))
ubaSubscribers = Lens.field @"subscribers"
{-# INLINEABLE ubaSubscribers #-}
{-# DEPRECATED subscribers "Use generic-lens or generic-optics with 'subscribers' instead"  #-}

instance Core.ToQuery UpdateBudgetAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateBudgetAction where
        toHeaders UpdateBudgetAction{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.UpdateBudgetAction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateBudgetAction where
        toJSON UpdateBudgetAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("ActionId" Core..= actionId),
                  ("ActionThreshold" Core..=) Core.<$> actionThreshold,
                  ("ApprovalModel" Core..=) Core.<$> approvalModel,
                  ("Definition" Core..=) Core.<$> definition,
                  ("ExecutionRoleArn" Core..=) Core.<$> executionRoleArn,
                  ("NotificationType" Core..=) Core.<$> notificationType,
                  ("Subscribers" Core..=) Core.<$> subscribers])

instance Core.AWSRequest UpdateBudgetAction where
        type Rs UpdateBudgetAction = UpdateBudgetActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateBudgetActionResponse' Core.<$>
                   (x Core..: "AccountId") Core.<*> x Core..: "BudgetName" Core.<*>
                     x Core..: "OldAction"
                     Core.<*> x Core..: "NewAction"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateBudgetActionResponse' smart constructor.
data UpdateBudgetActionResponse = UpdateBudgetActionResponse'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , oldAction :: Types.Action
    -- ^ The previous action resource information. 
  , newAction :: Types.Action
    -- ^ The updated action resource information. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBudgetActionResponse' value with any optional fields omitted.
mkUpdateBudgetActionResponse
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.Action -- ^ 'oldAction'
    -> Types.Action -- ^ 'newAction'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateBudgetActionResponse
mkUpdateBudgetActionResponse accountId budgetName oldAction
  newAction responseStatus
  = UpdateBudgetActionResponse'{accountId, budgetName, oldAction,
                                newAction, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarrsAccountId :: Lens.Lens' UpdateBudgetActionResponse Types.AccountId
ubarrsAccountId = Lens.field @"accountId"
{-# INLINEABLE ubarrsAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarrsBudgetName :: Lens.Lens' UpdateBudgetActionResponse Types.BudgetName
ubarrsBudgetName = Lens.field @"budgetName"
{-# INLINEABLE ubarrsBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | The previous action resource information. 
--
-- /Note:/ Consider using 'oldAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarrsOldAction :: Lens.Lens' UpdateBudgetActionResponse Types.Action
ubarrsOldAction = Lens.field @"oldAction"
{-# INLINEABLE ubarrsOldAction #-}
{-# DEPRECATED oldAction "Use generic-lens or generic-optics with 'oldAction' instead"  #-}

-- | The updated action resource information. 
--
-- /Note:/ Consider using 'newAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarrsNewAction :: Lens.Lens' UpdateBudgetActionResponse Types.Action
ubarrsNewAction = Lens.field @"newAction"
{-# INLINEABLE ubarrsNewAction #-}
{-# DEPRECATED newAction "Use generic-lens or generic-optics with 'newAction' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubarrsResponseStatus :: Lens.Lens' UpdateBudgetActionResponse Core.Int
ubarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ubarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
