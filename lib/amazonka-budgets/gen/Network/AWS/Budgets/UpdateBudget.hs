{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.UpdateBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a budget. You can change every part of a budget except for the @budgetName@ and the @calculatedSpend@ . When you modify a budget, the @calculatedSpend@ drops to zero until AWS has new usage data to use for forecasting.
--
-- /Important:/ Only one of @BudgetLimit@ or @PlannedBudgetLimits@ can be present in the syntax at one time. Use the syntax that matches your case. The Request Syntax section shows the @BudgetLimit@ syntax. For @PlannedBudgetLimits@ , see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_UpdateBudget.html#API_UpdateBudget_Examples Examples> section. 
module Network.AWS.Budgets.UpdateBudget
    (
    -- * Creating a request
      UpdateBudget (..)
    , mkUpdateBudget
    -- ** Request lenses
    , ubAccountId
    , ubNewBudget

    -- * Destructuring the response
    , UpdateBudgetResponse (..)
    , mkUpdateBudgetResponse
    -- ** Response lenses
    , ubrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of UpdateBudget 
--
-- /See:/ 'mkUpdateBudget' smart constructor.
data UpdateBudget = UpdateBudget'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budget that you want to update.
  , newBudget :: Types.Budget
    -- ^ The budget that you want to update your budget to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateBudget' value with any optional fields omitted.
mkUpdateBudget
    :: Types.AccountId -- ^ 'accountId'
    -> Types.Budget -- ^ 'newBudget'
    -> UpdateBudget
mkUpdateBudget accountId newBudget
  = UpdateBudget'{accountId, newBudget}

-- | The @accountId@ that is associated with the budget that you want to update.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubAccountId :: Lens.Lens' UpdateBudget Types.AccountId
ubAccountId = Lens.field @"accountId"
{-# INLINEABLE ubAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The budget that you want to update your budget to.
--
-- /Note:/ Consider using 'newBudget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubNewBudget :: Lens.Lens' UpdateBudget Types.Budget
ubNewBudget = Lens.field @"newBudget"
{-# INLINEABLE ubNewBudget #-}
{-# DEPRECATED newBudget "Use generic-lens or generic-optics with 'newBudget' instead"  #-}

instance Core.ToQuery UpdateBudget where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateBudget where
        toHeaders UpdateBudget{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.UpdateBudget")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateBudget where
        toJSON UpdateBudget{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("NewBudget" Core..= newBudget)])

instance Core.AWSRequest UpdateBudget where
        type Rs UpdateBudget = UpdateBudgetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateBudgetResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Response of UpdateBudget 
--
-- /See:/ 'mkUpdateBudgetResponse' smart constructor.
newtype UpdateBudgetResponse = UpdateBudgetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBudgetResponse' value with any optional fields omitted.
mkUpdateBudgetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateBudgetResponse
mkUpdateBudgetResponse responseStatus
  = UpdateBudgetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsResponseStatus :: Lens.Lens' UpdateBudgetResponse Core.Int
ubrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ubrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
