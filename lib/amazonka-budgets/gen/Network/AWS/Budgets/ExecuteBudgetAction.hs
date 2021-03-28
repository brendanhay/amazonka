{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ExecuteBudgetAction (..)
    , mkExecuteBudgetAction
    -- ** Request lenses
    , ebaAccountId
    , ebaBudgetName
    , ebaActionId
    , ebaExecutionType

    -- * Destructuring the response
    , ExecuteBudgetActionResponse (..)
    , mkExecuteBudgetActionResponse
    -- ** Response lenses
    , ebarrsAccountId
    , ebarrsBudgetName
    , ebarrsActionId
    , ebarrsExecutionType
    , ebarrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExecuteBudgetAction' smart constructor.
data ExecuteBudgetAction = ExecuteBudgetAction'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , actionId :: Types.ActionId
    -- ^ A system-generated universally unique identifier (UUID) for the action. 
  , executionType :: Types.ExecutionType
    -- ^ The type of execution. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecuteBudgetAction' value with any optional fields omitted.
mkExecuteBudgetAction
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.ActionId -- ^ 'actionId'
    -> Types.ExecutionType -- ^ 'executionType'
    -> ExecuteBudgetAction
mkExecuteBudgetAction accountId budgetName actionId executionType
  = ExecuteBudgetAction'{accountId, budgetName, actionId,
                         executionType}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebaAccountId :: Lens.Lens' ExecuteBudgetAction Types.AccountId
ebaAccountId = Lens.field @"accountId"
{-# INLINEABLE ebaAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebaBudgetName :: Lens.Lens' ExecuteBudgetAction Types.BudgetName
ebaBudgetName = Lens.field @"budgetName"
{-# INLINEABLE ebaBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | A system-generated universally unique identifier (UUID) for the action. 
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebaActionId :: Lens.Lens' ExecuteBudgetAction Types.ActionId
ebaActionId = Lens.field @"actionId"
{-# INLINEABLE ebaActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | The type of execution. 
--
-- /Note:/ Consider using 'executionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebaExecutionType :: Lens.Lens' ExecuteBudgetAction Types.ExecutionType
ebaExecutionType = Lens.field @"executionType"
{-# INLINEABLE ebaExecutionType #-}
{-# DEPRECATED executionType "Use generic-lens or generic-optics with 'executionType' instead"  #-}

instance Core.ToQuery ExecuteBudgetAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ExecuteBudgetAction where
        toHeaders ExecuteBudgetAction{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.ExecuteBudgetAction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ExecuteBudgetAction where
        toJSON ExecuteBudgetAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("ActionId" Core..= actionId),
                  Core.Just ("ExecutionType" Core..= executionType)])

instance Core.AWSRequest ExecuteBudgetAction where
        type Rs ExecuteBudgetAction = ExecuteBudgetActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ExecuteBudgetActionResponse' Core.<$>
                   (x Core..: "AccountId") Core.<*> x Core..: "BudgetName" Core.<*>
                     x Core..: "ActionId"
                     Core.<*> x Core..: "ExecutionType"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkExecuteBudgetActionResponse' smart constructor.
data ExecuteBudgetActionResponse = ExecuteBudgetActionResponse'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , actionId :: Types.ActionId
    -- ^ A system-generated universally unique identifier (UUID) for the action. 
  , executionType :: Types.ExecutionType
    -- ^ The type of execution. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecuteBudgetActionResponse' value with any optional fields omitted.
mkExecuteBudgetActionResponse
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.ActionId -- ^ 'actionId'
    -> Types.ExecutionType -- ^ 'executionType'
    -> Core.Int -- ^ 'responseStatus'
    -> ExecuteBudgetActionResponse
mkExecuteBudgetActionResponse accountId budgetName actionId
  executionType responseStatus
  = ExecuteBudgetActionResponse'{accountId, budgetName, actionId,
                                 executionType, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebarrsAccountId :: Lens.Lens' ExecuteBudgetActionResponse Types.AccountId
ebarrsAccountId = Lens.field @"accountId"
{-# INLINEABLE ebarrsAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebarrsBudgetName :: Lens.Lens' ExecuteBudgetActionResponse Types.BudgetName
ebarrsBudgetName = Lens.field @"budgetName"
{-# INLINEABLE ebarrsBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | A system-generated universally unique identifier (UUID) for the action. 
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebarrsActionId :: Lens.Lens' ExecuteBudgetActionResponse Types.ActionId
ebarrsActionId = Lens.field @"actionId"
{-# INLINEABLE ebarrsActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | The type of execution. 
--
-- /Note:/ Consider using 'executionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebarrsExecutionType :: Lens.Lens' ExecuteBudgetActionResponse Types.ExecutionType
ebarrsExecutionType = Lens.field @"executionType"
{-# INLINEABLE ebarrsExecutionType #-}
{-# DEPRECATED executionType "Use generic-lens or generic-optics with 'executionType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebarrsResponseStatus :: Lens.Lens' ExecuteBudgetActionResponse Core.Int
ebarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ebarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
