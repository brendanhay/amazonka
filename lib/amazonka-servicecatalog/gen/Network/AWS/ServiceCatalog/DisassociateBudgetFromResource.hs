{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DisassociateBudgetFromResource (..),
    mkDisassociateBudgetFromResource,

    -- ** Request lenses
    dbfrBudgetName,
    dbfrResourceId,

    -- * Destructuring the response
    DisassociateBudgetFromResourceResponse (..),
    mkDisassociateBudgetFromResourceResponse,

    -- ** Response lenses
    dbfrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDisassociateBudgetFromResource' smart constructor.
data DisassociateBudgetFromResource = DisassociateBudgetFromResource'
  { -- | The name of the budget you want to disassociate.
    budgetName :: Types.BudgetName,
    -- | The resource identifier you want to disassociate from. Either a portfolio-id or a product-id.
    resourceId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateBudgetFromResource' value with any optional fields omitted.
mkDisassociateBudgetFromResource ::
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'resourceId'
  Types.ResourceId ->
  DisassociateBudgetFromResource
mkDisassociateBudgetFromResource budgetName resourceId =
  DisassociateBudgetFromResource' {budgetName, resourceId}

-- | The name of the budget you want to disassociate.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfrBudgetName :: Lens.Lens' DisassociateBudgetFromResource Types.BudgetName
dbfrBudgetName = Lens.field @"budgetName"
{-# DEPRECATED dbfrBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The resource identifier you want to disassociate from. Either a portfolio-id or a product-id.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfrResourceId :: Lens.Lens' DisassociateBudgetFromResource Types.ResourceId
dbfrResourceId = Lens.field @"resourceId"
{-# DEPRECATED dbfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Core.FromJSON DisassociateBudgetFromResource where
  toJSON DisassociateBudgetFromResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.AWSRequest DisassociateBudgetFromResource where
  type
    Rs DisassociateBudgetFromResource =
      DisassociateBudgetFromResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DisassociateBudgetFromResource"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateBudgetFromResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateBudgetFromResourceResponse' smart constructor.
newtype DisassociateBudgetFromResourceResponse = DisassociateBudgetFromResourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateBudgetFromResourceResponse' value with any optional fields omitted.
mkDisassociateBudgetFromResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateBudgetFromResourceResponse
mkDisassociateBudgetFromResourceResponse responseStatus =
  DisassociateBudgetFromResourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfrrrsResponseStatus :: Lens.Lens' DisassociateBudgetFromResourceResponse Core.Int
dbfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
