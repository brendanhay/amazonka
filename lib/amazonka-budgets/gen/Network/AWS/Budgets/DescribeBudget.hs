{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget.
--
-- /Important:/ The Request Syntax section shows the @BudgetLimit@ syntax. For @PlannedBudgetLimits@ , see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_DescribeBudget.html#API_DescribeBudget_Examples Examples> section. 
module Network.AWS.Budgets.DescribeBudget
    (
    -- * Creating a request
      DescribeBudget (..)
    , mkDescribeBudget
    -- ** Request lenses
    , dbfAccountId
    , dbfBudgetName

    -- * Destructuring the response
    , DescribeBudgetResponse (..)
    , mkDescribeBudgetResponse
    -- ** Response lenses
    , dbrgrsBudget
    , dbrgrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DescribeBudget 
--
-- /See:/ 'mkDescribeBudget' smart constructor.
data DescribeBudget = DescribeBudget'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budget that you want a description of.
  , budgetName :: Types.BudgetName
    -- ^ The name of the budget that you want a description of.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudget' value with any optional fields omitted.
mkDescribeBudget
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> DescribeBudget
mkDescribeBudget accountId budgetName
  = DescribeBudget'{accountId, budgetName}

-- | The @accountId@ that is associated with the budget that you want a description of.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfAccountId :: Lens.Lens' DescribeBudget Types.AccountId
dbfAccountId = Lens.field @"accountId"
{-# INLINEABLE dbfAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the budget that you want a description of.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfBudgetName :: Lens.Lens' DescribeBudget Types.BudgetName
dbfBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dbfBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

instance Core.ToQuery DescribeBudget where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeBudget where
        toHeaders DescribeBudget{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.DescribeBudget")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeBudget where
        toJSON DescribeBudget{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName)])

instance Core.AWSRequest DescribeBudget where
        type Rs DescribeBudget = DescribeBudgetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBudgetResponse' Core.<$>
                   (x Core..:? "Budget") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response of DescribeBudget 
--
-- /See:/ 'mkDescribeBudgetResponse' smart constructor.
data DescribeBudgetResponse = DescribeBudgetResponse'
  { budget :: Core.Maybe Types.Budget
    -- ^ The description of the budget.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeBudgetResponse' value with any optional fields omitted.
mkDescribeBudgetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeBudgetResponse
mkDescribeBudgetResponse responseStatus
  = DescribeBudgetResponse'{budget = Core.Nothing, responseStatus}

-- | The description of the budget.
--
-- /Note:/ Consider using 'budget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrgrsBudget :: Lens.Lens' DescribeBudgetResponse (Core.Maybe Types.Budget)
dbrgrsBudget = Lens.field @"budget"
{-# INLINEABLE dbrgrsBudget #-}
{-# DEPRECATED budget "Use generic-lens or generic-optics with 'budget' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrgrsResponseStatus :: Lens.Lens' DescribeBudgetResponse Core.Int
dbrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
