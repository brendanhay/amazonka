{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioned product.
module Network.AWS.ServiceCatalog.DescribeProvisionedProduct
  ( -- * Creating a request
    DescribeProvisionedProduct (..),
    mkDescribeProvisionedProduct,

    -- ** Request lenses
    dppfAcceptLanguage,
    dppfId,
    dppfName,

    -- * Destructuring the response
    DescribeProvisionedProductResponse (..),
    mkDescribeProvisionedProductResponse,

    -- ** Response lenses
    dpprfrsCloudWatchDashboards,
    dpprfrsProvisionedProductDetail,
    dpprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | DescribeProvisionedProductAPI input structure. AcceptLanguage - [Optional] The language code for localization. Id - [Optional] The provisioned product identifier. Name - [Optional] Another provisioned product identifier. Customers must provide either Id or Name.
--
-- /See:/ 'mkDescribeProvisionedProduct' smart constructor.
data DescribeProvisionedProduct = DescribeProvisionedProduct'
  { -- | The language code.
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
    -- | The provisioned product identifier. You must provide the name or ID, but not both.
    --
    -- If you do not provide a name or ID, or you provide both name and ID, an @InvalidParametersException@ will occur.
    id :: Core.Maybe Types.Id,
    -- | The name of the provisioned product. You must provide the name or ID, but not both.
    --
    -- If you do not provide a name or ID, or you provide both name and ID, an @InvalidParametersException@ will occur.
    name :: Core.Maybe Types.ProvisionedProductName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProvisionedProduct' value with any optional fields omitted.
mkDescribeProvisionedProduct ::
  DescribeProvisionedProduct
mkDescribeProvisionedProduct =
  DescribeProvisionedProduct'
    { acceptLanguage = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

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
dppfAcceptLanguage :: Lens.Lens' DescribeProvisionedProduct (Core.Maybe Types.AcceptLanguage)
dppfAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dppfAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The provisioned product identifier. You must provide the name or ID, but not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an @InvalidParametersException@ will occur.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppfId :: Lens.Lens' DescribeProvisionedProduct (Core.Maybe Types.Id)
dppfId = Lens.field @"id"
{-# DEPRECATED dppfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the provisioned product. You must provide the name or ID, but not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an @InvalidParametersException@ will occur.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppfName :: Lens.Lens' DescribeProvisionedProduct (Core.Maybe Types.ProvisionedProductName)
dppfName = Lens.field @"name"
{-# DEPRECATED dppfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DescribeProvisionedProduct where
  toJSON DescribeProvisionedProduct {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("Id" Core..=) Core.<$> id,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest DescribeProvisionedProduct where
  type
    Rs DescribeProvisionedProduct =
      DescribeProvisionedProductResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DescribeProvisionedProduct"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisionedProductResponse'
            Core.<$> (x Core..:? "CloudWatchDashboards")
            Core.<*> (x Core..:? "ProvisionedProductDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProvisionedProductResponse' smart constructor.
data DescribeProvisionedProductResponse = DescribeProvisionedProductResponse'
  { -- | Any CloudWatch dashboards that were created when provisioning the product.
    cloudWatchDashboards :: Core.Maybe [Types.CloudWatchDashboard],
    -- | Information about the provisioned product.
    provisionedProductDetail :: Core.Maybe Types.ProvisionedProductDetail,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeProvisionedProductResponse' value with any optional fields omitted.
mkDescribeProvisionedProductResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeProvisionedProductResponse
mkDescribeProvisionedProductResponse responseStatus =
  DescribeProvisionedProductResponse'
    { cloudWatchDashboards =
        Core.Nothing,
      provisionedProductDetail = Core.Nothing,
      responseStatus
    }

-- | Any CloudWatch dashboards that were created when provisioning the product.
--
-- /Note:/ Consider using 'cloudWatchDashboards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprfrsCloudWatchDashboards :: Lens.Lens' DescribeProvisionedProductResponse (Core.Maybe [Types.CloudWatchDashboard])
dpprfrsCloudWatchDashboards = Lens.field @"cloudWatchDashboards"
{-# DEPRECATED dpprfrsCloudWatchDashboards "Use generic-lens or generic-optics with 'cloudWatchDashboards' instead." #-}

-- | Information about the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprfrsProvisionedProductDetail :: Lens.Lens' DescribeProvisionedProductResponse (Core.Maybe Types.ProvisionedProductDetail)
dpprfrsProvisionedProductDetail = Lens.field @"provisionedProductDetail"
{-# DEPRECATED dpprfrsProvisionedProductDetail "Use generic-lens or generic-optics with 'provisionedProductDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprfrsResponseStatus :: Lens.Lens' DescribeProvisionedProductResponse Core.Int
dpprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
