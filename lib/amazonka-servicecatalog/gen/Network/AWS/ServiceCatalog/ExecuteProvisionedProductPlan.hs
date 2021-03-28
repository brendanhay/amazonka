{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions or modifies a product based on the resource changes for the specified plan.
module Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan
    (
    -- * Creating a request
      ExecuteProvisionedProductPlan (..)
    , mkExecuteProvisionedProductPlan
    -- ** Request lenses
    , epppPlanId
    , epppIdempotencyToken
    , epppAcceptLanguage

    -- * Destructuring the response
    , ExecuteProvisionedProductPlanResponse (..)
    , mkExecuteProvisionedProductPlanResponse
    -- ** Response lenses
    , eppprrsRecordDetail
    , eppprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkExecuteProvisionedProductPlan' smart constructor.
data ExecuteProvisionedProductPlan = ExecuteProvisionedProductPlan'
  { planId :: Types.Id
    -- ^ The plan identifier.
  , idempotencyToken :: Types.IdempotencyToken
    -- ^ A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
  , acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecuteProvisionedProductPlan' value with any optional fields omitted.
mkExecuteProvisionedProductPlan
    :: Types.Id -- ^ 'planId'
    -> Types.IdempotencyToken -- ^ 'idempotencyToken'
    -> ExecuteProvisionedProductPlan
mkExecuteProvisionedProductPlan planId idempotencyToken
  = ExecuteProvisionedProductPlan'{planId, idempotencyToken,
                                   acceptLanguage = Core.Nothing}

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epppPlanId :: Lens.Lens' ExecuteProvisionedProductPlan Types.Id
epppPlanId = Lens.field @"planId"
{-# INLINEABLE epppPlanId #-}
{-# DEPRECATED planId "Use generic-lens or generic-optics with 'planId' instead"  #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epppIdempotencyToken :: Lens.Lens' ExecuteProvisionedProductPlan Types.IdempotencyToken
epppIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE epppIdempotencyToken #-}
{-# DEPRECATED idempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead"  #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epppAcceptLanguage :: Lens.Lens' ExecuteProvisionedProductPlan (Core.Maybe Types.AcceptLanguage)
epppAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE epppAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery ExecuteProvisionedProductPlan where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ExecuteProvisionedProductPlan where
        toHeaders ExecuteProvisionedProductPlan{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.ExecuteProvisionedProductPlan")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ExecuteProvisionedProductPlan where
        toJSON ExecuteProvisionedProductPlan{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PlanId" Core..= planId),
                  Core.Just ("IdempotencyToken" Core..= idempotencyToken),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest ExecuteProvisionedProductPlan where
        type Rs ExecuteProvisionedProductPlan =
             ExecuteProvisionedProductPlanResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ExecuteProvisionedProductPlanResponse' Core.<$>
                   (x Core..:? "RecordDetail") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkExecuteProvisionedProductPlanResponse' smart constructor.
data ExecuteProvisionedProductPlanResponse = ExecuteProvisionedProductPlanResponse'
  { recordDetail :: Core.Maybe Types.RecordDetail
    -- ^ Information about the result of provisioning the product.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ExecuteProvisionedProductPlanResponse' value with any optional fields omitted.
mkExecuteProvisionedProductPlanResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ExecuteProvisionedProductPlanResponse
mkExecuteProvisionedProductPlanResponse responseStatus
  = ExecuteProvisionedProductPlanResponse'{recordDetail =
                                             Core.Nothing,
                                           responseStatus}

-- | Information about the result of provisioning the product.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eppprrsRecordDetail :: Lens.Lens' ExecuteProvisionedProductPlanResponse (Core.Maybe Types.RecordDetail)
eppprrsRecordDetail = Lens.field @"recordDetail"
{-# INLINEABLE eppprrsRecordDetail #-}
{-# DEPRECATED recordDetail "Use generic-lens or generic-optics with 'recordDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eppprrsResponseStatus :: Lens.Lens' ExecuteProvisionedProductPlanResponse Core.Int
eppprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eppprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
