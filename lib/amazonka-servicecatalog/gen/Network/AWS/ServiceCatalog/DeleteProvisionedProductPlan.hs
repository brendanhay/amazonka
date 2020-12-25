{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified plan.
module Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan
  ( -- * Creating a request
    DeleteProvisionedProductPlan (..),
    mkDeleteProvisionedProductPlan,

    -- ** Request lenses
    dpppfPlanId,
    dpppfAcceptLanguage,
    dpppfIgnoreErrors,

    -- * Destructuring the response
    DeleteProvisionedProductPlanResponse (..),
    mkDeleteProvisionedProductPlanResponse,

    -- ** Response lenses
    dppprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDeleteProvisionedProductPlan' smart constructor.
data DeleteProvisionedProductPlan = DeleteProvisionedProductPlan'
  { -- | The plan identifier.
    planId :: Types.PlanId,
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
    acceptLanguage :: Core.Maybe Types.AcceptLanguage,
    -- | If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
    ignoreErrors :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProvisionedProductPlan' value with any optional fields omitted.
mkDeleteProvisionedProductPlan ::
  -- | 'planId'
  Types.PlanId ->
  DeleteProvisionedProductPlan
mkDeleteProvisionedProductPlan planId =
  DeleteProvisionedProductPlan'
    { planId,
      acceptLanguage = Core.Nothing,
      ignoreErrors = Core.Nothing
    }

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppfPlanId :: Lens.Lens' DeleteProvisionedProductPlan Types.PlanId
dpppfPlanId = Lens.field @"planId"
{-# DEPRECATED dpppfPlanId "Use generic-lens or generic-optics with 'planId' instead." #-}

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
dpppfAcceptLanguage :: Lens.Lens' DeleteProvisionedProductPlan (Core.Maybe Types.AcceptLanguage)
dpppfAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dpppfAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
--
-- /Note:/ Consider using 'ignoreErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppfIgnoreErrors :: Lens.Lens' DeleteProvisionedProductPlan (Core.Maybe Core.Bool)
dpppfIgnoreErrors = Lens.field @"ignoreErrors"
{-# DEPRECATED dpppfIgnoreErrors "Use generic-lens or generic-optics with 'ignoreErrors' instead." #-}

instance Core.FromJSON DeleteProvisionedProductPlan where
  toJSON DeleteProvisionedProductPlan {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PlanId" Core..= planId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("IgnoreErrors" Core..=) Core.<$> ignoreErrors
          ]
      )

instance Core.AWSRequest DeleteProvisionedProductPlan where
  type
    Rs DeleteProvisionedProductPlan =
      DeleteProvisionedProductPlanResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DeleteProvisionedProductPlan"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProvisionedProductPlanResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteProvisionedProductPlanResponse' smart constructor.
newtype DeleteProvisionedProductPlanResponse = DeleteProvisionedProductPlanResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProvisionedProductPlanResponse' value with any optional fields omitted.
mkDeleteProvisionedProductPlanResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteProvisionedProductPlanResponse
mkDeleteProvisionedProductPlanResponse responseStatus =
  DeleteProvisionedProductPlanResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppprfrsResponseStatus :: Lens.Lens' DeleteProvisionedProductPlanResponse Core.Int
dppprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dppprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
