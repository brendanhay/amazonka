{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.TerminateProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified provisioned product.
--
-- This operation does not delete any records associated with the provisioned product.
-- You can check the status of this request using 'DescribeRecord' .
module Network.AWS.ServiceCatalog.TerminateProvisionedProduct
  ( -- * Creating a request
    TerminateProvisionedProduct (..),
    mkTerminateProvisionedProduct,

    -- ** Request lenses
    tppTerminateToken,
    tppAcceptLanguage,
    tppIgnoreErrors,
    tppProvisionedProductId,
    tppProvisionedProductName,
    tppRetainPhysicalResources,

    -- * Destructuring the response
    TerminateProvisionedProductResponse (..),
    mkTerminateProvisionedProductResponse,

    -- ** Response lenses
    tpprrsRecordDetail,
    tpprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkTerminateProvisionedProduct' smart constructor.
data TerminateProvisionedProduct = TerminateProvisionedProduct'
  { -- | An idempotency token that uniquely identifies the termination request. This token is only valid during the termination process. After the provisioned product is terminated, subsequent requests to terminate the same provisioned product always return __ResourceNotFound__ .
    terminateToken :: Types.IdempotencyToken,
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
    ignoreErrors :: Core.Maybe Core.Bool,
    -- | The identifier of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
    provisionedProductId :: Core.Maybe Types.ProvisionedProductId,
    -- | The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
    provisionedProductName :: Core.Maybe Types.ProvisionedProductNameOrArn,
    -- | When this boolean parameter is set to true, the TerminateProvisionedProduct API deletes the Service Catalog provisioned product. However, it does not remove the CloudFormation stack, stack set, or the underlying resources of the deleted provisioned product. The default value is false.
    retainPhysicalResources :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateProvisionedProduct' value with any optional fields omitted.
mkTerminateProvisionedProduct ::
  -- | 'terminateToken'
  Types.IdempotencyToken ->
  TerminateProvisionedProduct
mkTerminateProvisionedProduct terminateToken =
  TerminateProvisionedProduct'
    { terminateToken,
      acceptLanguage = Core.Nothing,
      ignoreErrors = Core.Nothing,
      provisionedProductId = Core.Nothing,
      provisionedProductName = Core.Nothing,
      retainPhysicalResources = Core.Nothing
    }

-- | An idempotency token that uniquely identifies the termination request. This token is only valid during the termination process. After the provisioned product is terminated, subsequent requests to terminate the same provisioned product always return __ResourceNotFound__ .
--
-- /Note:/ Consider using 'terminateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppTerminateToken :: Lens.Lens' TerminateProvisionedProduct Types.IdempotencyToken
tppTerminateToken = Lens.field @"terminateToken"
{-# DEPRECATED tppTerminateToken "Use generic-lens or generic-optics with 'terminateToken' instead." #-}

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
tppAcceptLanguage :: Lens.Lens' TerminateProvisionedProduct (Core.Maybe Types.AcceptLanguage)
tppAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED tppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
--
-- /Note:/ Consider using 'ignoreErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppIgnoreErrors :: Lens.Lens' TerminateProvisionedProduct (Core.Maybe Core.Bool)
tppIgnoreErrors = Lens.field @"ignoreErrors"
{-# DEPRECATED tppIgnoreErrors "Use generic-lens or generic-optics with 'ignoreErrors' instead." #-}

-- | The identifier of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppProvisionedProductId :: Lens.Lens' TerminateProvisionedProduct (Core.Maybe Types.ProvisionedProductId)
tppProvisionedProductId = Lens.field @"provisionedProductId"
{-# DEPRECATED tppProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

-- | The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppProvisionedProductName :: Lens.Lens' TerminateProvisionedProduct (Core.Maybe Types.ProvisionedProductNameOrArn)
tppProvisionedProductName = Lens.field @"provisionedProductName"
{-# DEPRECATED tppProvisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead." #-}

-- | When this boolean parameter is set to true, the TerminateProvisionedProduct API deletes the Service Catalog provisioned product. However, it does not remove the CloudFormation stack, stack set, or the underlying resources of the deleted provisioned product. The default value is false.
--
-- /Note:/ Consider using 'retainPhysicalResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppRetainPhysicalResources :: Lens.Lens' TerminateProvisionedProduct (Core.Maybe Core.Bool)
tppRetainPhysicalResources = Lens.field @"retainPhysicalResources"
{-# DEPRECATED tppRetainPhysicalResources "Use generic-lens or generic-optics with 'retainPhysicalResources' instead." #-}

instance Core.FromJSON TerminateProvisionedProduct where
  toJSON TerminateProvisionedProduct {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TerminateToken" Core..= terminateToken),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("IgnoreErrors" Core..=) Core.<$> ignoreErrors,
            ("ProvisionedProductId" Core..=) Core.<$> provisionedProductId,
            ("ProvisionedProductName" Core..=) Core.<$> provisionedProductName,
            ("RetainPhysicalResources" Core..=)
              Core.<$> retainPhysicalResources
          ]
      )

instance Core.AWSRequest TerminateProvisionedProduct where
  type
    Rs TerminateProvisionedProduct =
      TerminateProvisionedProductResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.TerminateProvisionedProduct"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TerminateProvisionedProductResponse'
            Core.<$> (x Core..:? "RecordDetail") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTerminateProvisionedProductResponse' smart constructor.
data TerminateProvisionedProductResponse = TerminateProvisionedProductResponse'
  { -- | Information about the result of this request.
    recordDetail :: Core.Maybe Types.RecordDetail,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TerminateProvisionedProductResponse' value with any optional fields omitted.
mkTerminateProvisionedProductResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TerminateProvisionedProductResponse
mkTerminateProvisionedProductResponse responseStatus =
  TerminateProvisionedProductResponse'
    { recordDetail = Core.Nothing,
      responseStatus
    }

-- | Information about the result of this request.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpprrsRecordDetail :: Lens.Lens' TerminateProvisionedProductResponse (Core.Maybe Types.RecordDetail)
tpprrsRecordDetail = Lens.field @"recordDetail"
{-# DEPRECATED tpprrsRecordDetail "Use generic-lens or generic-optics with 'recordDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpprrsResponseStatus :: Lens.Lens' TerminateProvisionedProductResponse Core.Int
tpprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tpprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
