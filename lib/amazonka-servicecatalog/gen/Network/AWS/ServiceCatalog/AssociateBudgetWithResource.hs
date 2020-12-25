{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AssociateBudgetWithResource (..),
    mkAssociateBudgetWithResource,

    -- ** Request lenses
    abwrBudgetName,
    abwrResourceId,

    -- * Destructuring the response
    AssociateBudgetWithResourceResponse (..),
    mkAssociateBudgetWithResourceResponse,

    -- ** Response lenses
    abwrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkAssociateBudgetWithResource' smart constructor.
data AssociateBudgetWithResource = AssociateBudgetWithResource'
  { -- | The name of the budget you want to associate.
    budgetName :: Types.BudgetName,
    -- | The resource identifier. Either a portfolio-id or a product-id.
    resourceId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateBudgetWithResource' value with any optional fields omitted.
mkAssociateBudgetWithResource ::
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'resourceId'
  Types.ResourceId ->
  AssociateBudgetWithResource
mkAssociateBudgetWithResource budgetName resourceId =
  AssociateBudgetWithResource' {budgetName, resourceId}

-- | The name of the budget you want to associate.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abwrBudgetName :: Lens.Lens' AssociateBudgetWithResource Types.BudgetName
abwrBudgetName = Lens.field @"budgetName"
{-# DEPRECATED abwrBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The resource identifier. Either a portfolio-id or a product-id.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abwrResourceId :: Lens.Lens' AssociateBudgetWithResource Types.ResourceId
abwrResourceId = Lens.field @"resourceId"
{-# DEPRECATED abwrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Core.FromJSON AssociateBudgetWithResource where
  toJSON AssociateBudgetWithResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.AWSRequest AssociateBudgetWithResource where
  type
    Rs AssociateBudgetWithResource =
      AssociateBudgetWithResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.AssociateBudgetWithResource"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateBudgetWithResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateBudgetWithResourceResponse' smart constructor.
newtype AssociateBudgetWithResourceResponse = AssociateBudgetWithResourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateBudgetWithResourceResponse' value with any optional fields omitted.
mkAssociateBudgetWithResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateBudgetWithResourceResponse
mkAssociateBudgetWithResourceResponse responseStatus =
  AssociateBudgetWithResourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abwrrrsResponseStatus :: Lens.Lens' AssociateBudgetWithResourceResponse Core.Int
abwrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED abwrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
