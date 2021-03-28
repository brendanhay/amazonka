{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisassociateBudgetFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified budget from the specified resource.
module Network.AWS.ServiceCatalog.DisassociateBudgetFromResource
    (
    -- * Creating a request
      DisassociateBudgetFromResource (..)
    , mkDisassociateBudgetFromResource
    -- ** Request lenses
    , dbfrBudgetName
    , dbfrResourceId

    -- * Destructuring the response
    , DisassociateBudgetFromResourceResponse (..)
    , mkDisassociateBudgetFromResourceResponse
    -- ** Response lenses
    , dbfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDisassociateBudgetFromResource' smart constructor.
data DisassociateBudgetFromResource = DisassociateBudgetFromResource'
  { budgetName :: Types.BudgetName
    -- ^ The name of the budget you want to disassociate.
  , resourceId :: Types.ResourceId
    -- ^ The resource identifier you want to disassociate from. Either a portfolio-id or a product-id.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateBudgetFromResource' value with any optional fields omitted.
mkDisassociateBudgetFromResource
    :: Types.BudgetName -- ^ 'budgetName'
    -> Types.ResourceId -- ^ 'resourceId'
    -> DisassociateBudgetFromResource
mkDisassociateBudgetFromResource budgetName resourceId
  = DisassociateBudgetFromResource'{budgetName, resourceId}

-- | The name of the budget you want to disassociate.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfrBudgetName :: Lens.Lens' DisassociateBudgetFromResource Types.BudgetName
dbfrBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dbfrBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | The resource identifier you want to disassociate from. Either a portfolio-id or a product-id.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfrResourceId :: Lens.Lens' DisassociateBudgetFromResource Types.ResourceId
dbfrResourceId = Lens.field @"resourceId"
{-# INLINEABLE dbfrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.ToQuery DisassociateBudgetFromResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateBudgetFromResource where
        toHeaders DisassociateBudgetFromResource{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DisassociateBudgetFromResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateBudgetFromResource where
        toJSON DisassociateBudgetFromResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("ResourceId" Core..= resourceId)])

instance Core.AWSRequest DisassociateBudgetFromResource where
        type Rs DisassociateBudgetFromResource =
             DisassociateBudgetFromResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateBudgetFromResourceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateBudgetFromResourceResponse' smart constructor.
newtype DisassociateBudgetFromResourceResponse = DisassociateBudgetFromResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateBudgetFromResourceResponse' value with any optional fields omitted.
mkDisassociateBudgetFromResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateBudgetFromResourceResponse
mkDisassociateBudgetFromResourceResponse responseStatus
  = DisassociateBudgetFromResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfrrrsResponseStatus :: Lens.Lens' DisassociateBudgetFromResourceResponse Core.Int
dbfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
