{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeServiceActionExecutionParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds the default parameters for a specific self-service action on a specific provisioned product and returns a map of the results to the user.
module Network.AWS.ServiceCatalog.DescribeServiceActionExecutionParameters
  ( -- * Creating a request
    DescribeServiceActionExecutionParameters (..),
    mkDescribeServiceActionExecutionParameters,

    -- ** Request lenses
    dsaepProvisionedProductId,
    dsaepServiceActionId,
    dsaepAcceptLanguage,

    -- * Destructuring the response
    DescribeServiceActionExecutionParametersResponse (..),
    mkDescribeServiceActionExecutionParametersResponse,

    -- ** Response lenses
    dsaeprrsServiceActionParameters,
    dsaeprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeServiceActionExecutionParameters' smart constructor.
data DescribeServiceActionExecutionParameters = DescribeServiceActionExecutionParameters'
  { -- | The identifier of the provisioned product.
    provisionedProductId :: Types.ProvisionedProductId,
    -- | The self-service action identifier.
    serviceActionId :: Types.ServiceActionId,
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

-- | Creates a 'DescribeServiceActionExecutionParameters' value with any optional fields omitted.
mkDescribeServiceActionExecutionParameters ::
  -- | 'provisionedProductId'
  Types.ProvisionedProductId ->
  -- | 'serviceActionId'
  Types.ServiceActionId ->
  DescribeServiceActionExecutionParameters
mkDescribeServiceActionExecutionParameters
  provisionedProductId
  serviceActionId =
    DescribeServiceActionExecutionParameters'
      { provisionedProductId,
        serviceActionId,
        acceptLanguage = Core.Nothing
      }

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaepProvisionedProductId :: Lens.Lens' DescribeServiceActionExecutionParameters Types.ProvisionedProductId
dsaepProvisionedProductId = Lens.field @"provisionedProductId"
{-# DEPRECATED dsaepProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

-- | The self-service action identifier.
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaepServiceActionId :: Lens.Lens' DescribeServiceActionExecutionParameters Types.ServiceActionId
dsaepServiceActionId = Lens.field @"serviceActionId"
{-# DEPRECATED dsaepServiceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead." #-}

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
dsaepAcceptLanguage :: Lens.Lens' DescribeServiceActionExecutionParameters (Core.Maybe Types.AcceptLanguage)
dsaepAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dsaepAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON DescribeServiceActionExecutionParameters where
  toJSON DescribeServiceActionExecutionParameters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProvisionedProductId" Core..= provisionedProductId),
            Core.Just ("ServiceActionId" Core..= serviceActionId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest DescribeServiceActionExecutionParameters where
  type
    Rs DescribeServiceActionExecutionParameters =
      DescribeServiceActionExecutionParametersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DescribeServiceActionExecutionParameters"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceActionExecutionParametersResponse'
            Core.<$> (x Core..:? "ServiceActionParameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeServiceActionExecutionParametersResponse' smart constructor.
data DescribeServiceActionExecutionParametersResponse = DescribeServiceActionExecutionParametersResponse'
  { -- | The parameters of the self-service action.
    serviceActionParameters :: Core.Maybe [Types.ExecutionParameter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServiceActionExecutionParametersResponse' value with any optional fields omitted.
mkDescribeServiceActionExecutionParametersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeServiceActionExecutionParametersResponse
mkDescribeServiceActionExecutionParametersResponse responseStatus =
  DescribeServiceActionExecutionParametersResponse'
    { serviceActionParameters =
        Core.Nothing,
      responseStatus
    }

-- | The parameters of the self-service action.
--
-- /Note:/ Consider using 'serviceActionParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaeprrsServiceActionParameters :: Lens.Lens' DescribeServiceActionExecutionParametersResponse (Core.Maybe [Types.ExecutionParameter])
dsaeprrsServiceActionParameters = Lens.field @"serviceActionParameters"
{-# DEPRECATED dsaeprrsServiceActionParameters "Use generic-lens or generic-optics with 'serviceActionParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaeprrsResponseStatus :: Lens.Lens' DescribeServiceActionExecutionParametersResponse Core.Int
dsaeprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsaeprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
