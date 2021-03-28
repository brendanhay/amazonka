{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.DeleteCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Cost Category. Expenses from this month going forward will no longer be categorized with this Cost Category.
module Network.AWS.CostExplorer.DeleteCostCategoryDefinition
    (
    -- * Creating a request
      DeleteCostCategoryDefinition (..)
    , mkDeleteCostCategoryDefinition
    -- ** Request lenses
    , dccdCostCategoryArn

    -- * Destructuring the response
    , DeleteCostCategoryDefinitionResponse (..)
    , mkDeleteCostCategoryDefinitionResponse
    -- ** Response lenses
    , dccdrrsCostCategoryArn
    , dccdrrsEffectiveEnd
    , dccdrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCostCategoryDefinition' smart constructor.
newtype DeleteCostCategoryDefinition = DeleteCostCategoryDefinition'
  { costCategoryArn :: Types.Arn
    -- ^ The unique identifier for your Cost Category. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCostCategoryDefinition' value with any optional fields omitted.
mkDeleteCostCategoryDefinition
    :: Types.Arn -- ^ 'costCategoryArn'
    -> DeleteCostCategoryDefinition
mkDeleteCostCategoryDefinition costCategoryArn
  = DeleteCostCategoryDefinition'{costCategoryArn}

-- | The unique identifier for your Cost Category. 
--
-- /Note:/ Consider using 'costCategoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccdCostCategoryArn :: Lens.Lens' DeleteCostCategoryDefinition Types.Arn
dccdCostCategoryArn = Lens.field @"costCategoryArn"
{-# INLINEABLE dccdCostCategoryArn #-}
{-# DEPRECATED costCategoryArn "Use generic-lens or generic-optics with 'costCategoryArn' instead"  #-}

instance Core.ToQuery DeleteCostCategoryDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCostCategoryDefinition where
        toHeaders DeleteCostCategoryDefinition{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSInsightsIndexService.DeleteCostCategoryDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCostCategoryDefinition where
        toJSON DeleteCostCategoryDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CostCategoryArn" Core..= costCategoryArn)])

instance Core.AWSRequest DeleteCostCategoryDefinition where
        type Rs DeleteCostCategoryDefinition =
             DeleteCostCategoryDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteCostCategoryDefinitionResponse' Core.<$>
                   (x Core..:? "CostCategoryArn") Core.<*> x Core..:? "EffectiveEnd"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCostCategoryDefinitionResponse' smart constructor.
data DeleteCostCategoryDefinitionResponse = DeleteCostCategoryDefinitionResponse'
  { costCategoryArn :: Core.Maybe Types.Arn
    -- ^ The unique identifier for your Cost Category. 
  , effectiveEnd :: Core.Maybe Types.EffectiveEnd
    -- ^ The effective end date of the Cost Category as a result of deleting it. No costs after this date will be categorized by the deleted Cost Category. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCostCategoryDefinitionResponse' value with any optional fields omitted.
mkDeleteCostCategoryDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCostCategoryDefinitionResponse
mkDeleteCostCategoryDefinitionResponse responseStatus
  = DeleteCostCategoryDefinitionResponse'{costCategoryArn =
                                            Core.Nothing,
                                          effectiveEnd = Core.Nothing, responseStatus}

-- | The unique identifier for your Cost Category. 
--
-- /Note:/ Consider using 'costCategoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccdrrsCostCategoryArn :: Lens.Lens' DeleteCostCategoryDefinitionResponse (Core.Maybe Types.Arn)
dccdrrsCostCategoryArn = Lens.field @"costCategoryArn"
{-# INLINEABLE dccdrrsCostCategoryArn #-}
{-# DEPRECATED costCategoryArn "Use generic-lens or generic-optics with 'costCategoryArn' instead"  #-}

-- | The effective end date of the Cost Category as a result of deleting it. No costs after this date will be categorized by the deleted Cost Category. 
--
-- /Note:/ Consider using 'effectiveEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccdrrsEffectiveEnd :: Lens.Lens' DeleteCostCategoryDefinitionResponse (Core.Maybe Types.EffectiveEnd)
dccdrrsEffectiveEnd = Lens.field @"effectiveEnd"
{-# INLINEABLE dccdrrsEffectiveEnd #-}
{-# DEPRECATED effectiveEnd "Use generic-lens or generic-optics with 'effectiveEnd' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccdrrsResponseStatus :: Lens.Lens' DeleteCostCategoryDefinitionResponse Core.Int
dccdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dccdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
