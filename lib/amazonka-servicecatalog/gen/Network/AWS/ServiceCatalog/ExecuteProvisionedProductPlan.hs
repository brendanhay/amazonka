{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ExecuteProvisionedProductPlan (..),
    mkExecuteProvisionedProductPlan,

    -- ** Request lenses
    epppPlanId,
    epppIdempotencyToken,
    epppAcceptLanguage,

    -- * Destructuring the response
    ExecuteProvisionedProductPlanResponse (..),
    mkExecuteProvisionedProductPlanResponse,

    -- ** Response lenses
    eppprrsRecordDetail,
    eppprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkExecuteProvisionedProductPlan' smart constructor.
data ExecuteProvisionedProductPlan = ExecuteProvisionedProductPlan'
  { -- | The plan identifier.
    planId :: Types.Id,
    -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Types.IdempotencyToken,
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
    acceptLanguage :: Core.Maybe Types.AcceptLanguage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecuteProvisionedProductPlan' value with any optional fields omitted.
mkExecuteProvisionedProductPlan ::
  -- | 'planId'
  Types.Id ->
  -- | 'idempotencyToken'
  Types.IdempotencyToken ->
  ExecuteProvisionedProductPlan
mkExecuteProvisionedProductPlan planId idempotencyToken =
  ExecuteProvisionedProductPlan'
    { planId,
      idempotencyToken,
      acceptLanguage = Core.Nothing
    }

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epppPlanId :: Lens.Lens' ExecuteProvisionedProductPlan Types.Id
epppPlanId = Lens.field @"planId"
{-# DEPRECATED epppPlanId "Use generic-lens or generic-optics with 'planId' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epppIdempotencyToken :: Lens.Lens' ExecuteProvisionedProductPlan Types.IdempotencyToken
epppIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED epppIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

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
{-# DEPRECATED epppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON ExecuteProvisionedProductPlan where
  toJSON ExecuteProvisionedProductPlan {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PlanId" Core..= planId),
            Core.Just ("IdempotencyToken" Core..= idempotencyToken),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest ExecuteProvisionedProductPlan where
  type
    Rs ExecuteProvisionedProductPlan =
      ExecuteProvisionedProductPlanResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.ExecuteProvisionedProductPlan"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteProvisionedProductPlanResponse'
            Core.<$> (x Core..:? "RecordDetail") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkExecuteProvisionedProductPlanResponse' smart constructor.
data ExecuteProvisionedProductPlanResponse = ExecuteProvisionedProductPlanResponse'
  { -- | Information about the result of provisioning the product.
    recordDetail :: Core.Maybe Types.RecordDetail,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ExecuteProvisionedProductPlanResponse' value with any optional fields omitted.
mkExecuteProvisionedProductPlanResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ExecuteProvisionedProductPlanResponse
mkExecuteProvisionedProductPlanResponse responseStatus =
  ExecuteProvisionedProductPlanResponse'
    { recordDetail =
        Core.Nothing,
      responseStatus
    }

-- | Information about the result of provisioning the product.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eppprrsRecordDetail :: Lens.Lens' ExecuteProvisionedProductPlanResponse (Core.Maybe Types.RecordDetail)
eppprrsRecordDetail = Lens.field @"recordDetail"
{-# DEPRECATED eppprrsRecordDetail "Use generic-lens or generic-optics with 'recordDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eppprrsResponseStatus :: Lens.Lens' ExecuteProvisionedProductPlanResponse Core.Int
eppprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eppprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
