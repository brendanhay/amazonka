{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget action detail. 
module Network.AWS.Budgets.DescribeBudgetAction
    (
    -- * Creating a request
      DescribeBudgetAction (..)
    , mkDescribeBudgetAction
    -- ** Request lenses
    , dbafAccountId
    , dbafBudgetName
    , dbafActionId

    -- * Destructuring the response
    , DescribeBudgetActionResponse (..)
    , mkDescribeBudgetActionResponse
    -- ** Response lenses
    , dbarrsAccountId
    , dbarrsBudgetName
    , dbarrsAction
    , dbarrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBudgetAction' smart constructor.
data DescribeBudgetAction = DescribeBudgetAction'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , actionId :: Types.ActionId
    -- ^ A system-generated universally unique identifier (UUID) for the action. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudgetAction' value with any optional fields omitted.
mkDescribeBudgetAction
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.ActionId -- ^ 'actionId'
    -> DescribeBudgetAction
mkDescribeBudgetAction accountId budgetName actionId
  = DescribeBudgetAction'{accountId, budgetName, actionId}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafAccountId :: Lens.Lens' DescribeBudgetAction Types.AccountId
dbafAccountId = Lens.field @"accountId"
{-# INLINEABLE dbafAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafBudgetName :: Lens.Lens' DescribeBudgetAction Types.BudgetName
dbafBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dbafBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | A system-generated universally unique identifier (UUID) for the action. 
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafActionId :: Lens.Lens' DescribeBudgetAction Types.ActionId
dbafActionId = Lens.field @"actionId"
{-# INLINEABLE dbafActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

instance Core.ToQuery DescribeBudgetAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeBudgetAction where
        toHeaders DescribeBudgetAction{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.DescribeBudgetAction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeBudgetAction where
        toJSON DescribeBudgetAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("ActionId" Core..= actionId)])

instance Core.AWSRequest DescribeBudgetAction where
        type Rs DescribeBudgetAction = DescribeBudgetActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBudgetActionResponse' Core.<$>
                   (x Core..: "AccountId") Core.<*> x Core..: "BudgetName" Core.<*>
                     x Core..: "Action"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeBudgetActionResponse' smart constructor.
data DescribeBudgetActionResponse = DescribeBudgetActionResponse'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , action :: Types.Action
    -- ^ A budget action resource. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudgetActionResponse' value with any optional fields omitted.
mkDescribeBudgetActionResponse
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.Action -- ^ 'action'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeBudgetActionResponse
mkDescribeBudgetActionResponse accountId budgetName action
  responseStatus
  = DescribeBudgetActionResponse'{accountId, budgetName, action,
                                  responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbarrsAccountId :: Lens.Lens' DescribeBudgetActionResponse Types.AccountId
dbarrsAccountId = Lens.field @"accountId"
{-# INLINEABLE dbarrsAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbarrsBudgetName :: Lens.Lens' DescribeBudgetActionResponse Types.BudgetName
dbarrsBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dbarrsBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | A budget action resource. 
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbarrsAction :: Lens.Lens' DescribeBudgetActionResponse Types.Action
dbarrsAction = Lens.field @"action"
{-# INLINEABLE dbarrsAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbarrsResponseStatus :: Lens.Lens' DescribeBudgetActionResponse Core.Int
dbarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
