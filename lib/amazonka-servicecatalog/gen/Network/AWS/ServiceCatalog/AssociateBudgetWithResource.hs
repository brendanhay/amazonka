{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociateBudgetWithResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified budget with the specified resource.
module Network.AWS.ServiceCatalog.AssociateBudgetWithResource
    (
    -- * Creating a request
      AssociateBudgetWithResource (..)
    , mkAssociateBudgetWithResource
    -- ** Request lenses
    , abwrBudgetName
    , abwrResourceId

    -- * Destructuring the response
    , AssociateBudgetWithResourceResponse (..)
    , mkAssociateBudgetWithResourceResponse
    -- ** Response lenses
    , abwrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkAssociateBudgetWithResource' smart constructor.
data AssociateBudgetWithResource = AssociateBudgetWithResource'
  { budgetName :: Types.BudgetName
    -- ^ The name of the budget you want to associate.
  , resourceId :: Types.ResourceId
    -- ^ The resource identifier. Either a portfolio-id or a product-id.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateBudgetWithResource' value with any optional fields omitted.
mkAssociateBudgetWithResource
    :: Types.BudgetName -- ^ 'budgetName'
    -> Types.ResourceId -- ^ 'resourceId'
    -> AssociateBudgetWithResource
mkAssociateBudgetWithResource budgetName resourceId
  = AssociateBudgetWithResource'{budgetName, resourceId}

-- | The name of the budget you want to associate.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abwrBudgetName :: Lens.Lens' AssociateBudgetWithResource Types.BudgetName
abwrBudgetName = Lens.field @"budgetName"
{-# INLINEABLE abwrBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | The resource identifier. Either a portfolio-id or a product-id.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abwrResourceId :: Lens.Lens' AssociateBudgetWithResource Types.ResourceId
abwrResourceId = Lens.field @"resourceId"
{-# INLINEABLE abwrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.ToQuery AssociateBudgetWithResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateBudgetWithResource where
        toHeaders AssociateBudgetWithResource{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.AssociateBudgetWithResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateBudgetWithResource where
        toJSON AssociateBudgetWithResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("ResourceId" Core..= resourceId)])

instance Core.AWSRequest AssociateBudgetWithResource where
        type Rs AssociateBudgetWithResource =
             AssociateBudgetWithResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateBudgetWithResourceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateBudgetWithResourceResponse' smart constructor.
newtype AssociateBudgetWithResourceResponse = AssociateBudgetWithResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateBudgetWithResourceResponse' value with any optional fields omitted.
mkAssociateBudgetWithResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateBudgetWithResourceResponse
mkAssociateBudgetWithResourceResponse responseStatus
  = AssociateBudgetWithResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abwrrrsResponseStatus :: Lens.Lens' AssociateBudgetWithResourceResponse Core.Int
abwrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE abwrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
