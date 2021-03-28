{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteBudgetAction (..)
    , mkDeleteBudgetAction
    -- ** Request lenses
    , dbaAccountId
    , dbaBudgetName
    , dbaActionId

    -- * Destructuring the response
    , DeleteBudgetActionResponse (..)
    , mkDeleteBudgetActionResponse
    -- ** Response lenses
    , drsAccountId
    , drsBudgetName
    , drsAction
    , drsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBudgetAction' smart constructor.
data DeleteBudgetAction = DeleteBudgetAction'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , actionId :: Types.ActionId
    -- ^ A system-generated universally unique identifier (UUID) for the action. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBudgetAction' value with any optional fields omitted.
mkDeleteBudgetAction
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.ActionId -- ^ 'actionId'
    -> DeleteBudgetAction
mkDeleteBudgetAction accountId budgetName actionId
  = DeleteBudgetAction'{accountId, budgetName, actionId}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaAccountId :: Lens.Lens' DeleteBudgetAction Types.AccountId
dbaAccountId = Lens.field @"accountId"
{-# INLINEABLE dbaAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaBudgetName :: Lens.Lens' DeleteBudgetAction Types.BudgetName
dbaBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dbaBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | A system-generated universally unique identifier (UUID) for the action. 
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbaActionId :: Lens.Lens' DeleteBudgetAction Types.ActionId
dbaActionId = Lens.field @"actionId"
{-# INLINEABLE dbaActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

instance Core.ToQuery DeleteBudgetAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBudgetAction where
        toHeaders DeleteBudgetAction{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.DeleteBudgetAction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteBudgetAction where
        toJSON DeleteBudgetAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("ActionId" Core..= actionId)])

instance Core.AWSRequest DeleteBudgetAction where
        type Rs DeleteBudgetAction = DeleteBudgetActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteBudgetActionResponse' Core.<$>
                   (x Core..: "AccountId") Core.<*> x Core..: "BudgetName" Core.<*>
                     x Core..: "Action"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBudgetActionResponse' smart constructor.
data DeleteBudgetActionResponse = DeleteBudgetActionResponse'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , action :: Types.Action
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBudgetActionResponse' value with any optional fields omitted.
mkDeleteBudgetActionResponse
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.Action -- ^ 'action'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteBudgetActionResponse
mkDeleteBudgetActionResponse accountId budgetName action
  responseStatus
  = DeleteBudgetActionResponse'{accountId, budgetName, action,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAccountId :: Lens.Lens' DeleteBudgetActionResponse Types.AccountId
drsAccountId = Lens.field @"accountId"
{-# INLINEABLE drsAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsBudgetName :: Lens.Lens' DeleteBudgetActionResponse Types.BudgetName
drsBudgetName = Lens.field @"budgetName"
{-# INLINEABLE drsBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAction :: Lens.Lens' DeleteBudgetActionResponse Types.Action
drsAction = Lens.field @"action"
{-# INLINEABLE drsAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteBudgetActionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
