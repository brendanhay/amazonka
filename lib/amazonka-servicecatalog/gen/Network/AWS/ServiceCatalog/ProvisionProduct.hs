{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ProvisionProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions the specified product.
--
-- A provisioned product is a resourced instance of a product. For example, provisioning a product based on a CloudFormation template launches a CloudFormation stack and its underlying resources. You can check the status of this request using 'DescribeRecord' .
-- If the request contains a tag key with an empty list of values, there is a tag conflict for that key. Do not include conflicted keys as tags, or this causes the error "Parameter validation failed: Missing required parameter in Tags[/N/ ]:/Value/ ".
module Network.AWS.ServiceCatalog.ProvisionProduct
  ( -- * Creating a request
    ProvisionProduct (..),
    mkProvisionProduct,

    -- ** Request lenses
    ppProvisionedProductName,
    ppProvisionToken,
    ppAcceptLanguage,
    ppNotificationArns,
    ppPathId,
    ppPathName,
    ppProductId,
    ppProductName,
    ppProvisioningArtifactId,
    ppProvisioningArtifactName,
    ppProvisioningParameters,
    ppProvisioningPreferences,
    ppTags,

    -- * Destructuring the response
    ProvisionProductResponse (..),
    mkProvisionProductResponse,

    -- ** Response lenses
    pprrsRecordDetail,
    pprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkProvisionProduct' smart constructor.
data ProvisionProduct = ProvisionProduct'
  { -- | A user-friendly name for the provisioned product. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
    provisionedProductName :: Types.ProvisionedProductName,
    -- | An idempotency token that uniquely identifies the provisioning request.
    provisionToken :: Types.IdempotencyToken,
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
    -- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
    notificationArns :: Core.Maybe [Types.NotificationArn],
    -- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' . You must provide the name or ID, but not both.
    pathId :: Core.Maybe Types.PathId,
    -- | The name of the path. You must provide the name or ID, but not both.
    pathName :: Core.Maybe Types.PathName,
    -- | The product identifier. You must provide the name or ID, but not both.
    productId :: Core.Maybe Types.ProductId,
    -- | The name of the product. You must provide the name or ID, but not both.
    productName :: Core.Maybe Types.ProductName,
    -- | The identifier of the provisioning artifact. You must provide the name or ID, but not both.
    provisioningArtifactId :: Core.Maybe Types.ProvisioningArtifactId,
    -- | The name of the provisioning artifact. You must provide the name or ID, but not both.
    provisioningArtifactName :: Core.Maybe Types.ProvisioningArtifactName,
    -- | Parameters specified by the administrator that are required for provisioning the product.
    provisioningParameters :: Core.Maybe [Types.ProvisioningParameter],
    -- | An object that contains information about the provisioning preferences for a stack set.
    provisioningPreferences :: Core.Maybe Types.ProvisioningPreferences,
    -- | One or more tags.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisionProduct' value with any optional fields omitted.
mkProvisionProduct ::
  -- | 'provisionedProductName'
  Types.ProvisionedProductName ->
  -- | 'provisionToken'
  Types.IdempotencyToken ->
  ProvisionProduct
mkProvisionProduct provisionedProductName provisionToken =
  ProvisionProduct'
    { provisionedProductName,
      provisionToken,
      acceptLanguage = Core.Nothing,
      notificationArns = Core.Nothing,
      pathId = Core.Nothing,
      pathName = Core.Nothing,
      productId = Core.Nothing,
      productName = Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      provisioningArtifactName = Core.Nothing,
      provisioningParameters = Core.Nothing,
      provisioningPreferences = Core.Nothing,
      tags = Core.Nothing
    }

-- | A user-friendly name for the provisioned product. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisionedProductName :: Lens.Lens' ProvisionProduct Types.ProvisionedProductName
ppProvisionedProductName = Lens.field @"provisionedProductName"
{-# DEPRECATED ppProvisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead." #-}

-- | An idempotency token that uniquely identifies the provisioning request.
--
-- /Note:/ Consider using 'provisionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisionToken :: Lens.Lens' ProvisionProduct Types.IdempotencyToken
ppProvisionToken = Lens.field @"provisionToken"
{-# DEPRECATED ppProvisionToken "Use generic-lens or generic-optics with 'provisionToken' instead." #-}

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
ppAcceptLanguage :: Lens.Lens' ProvisionProduct (Core.Maybe Types.AcceptLanguage)
ppAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED ppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
--
-- /Note:/ Consider using 'notificationArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppNotificationArns :: Lens.Lens' ProvisionProduct (Core.Maybe [Types.NotificationArn])
ppNotificationArns = Lens.field @"notificationArns"
{-# DEPRECATED ppNotificationArns "Use generic-lens or generic-optics with 'notificationArns' instead." #-}

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' . You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPathId :: Lens.Lens' ProvisionProduct (Core.Maybe Types.PathId)
ppPathId = Lens.field @"pathId"
{-# DEPRECATED ppPathId "Use generic-lens or generic-optics with 'pathId' instead." #-}

-- | The name of the path. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPathName :: Lens.Lens' ProvisionProduct (Core.Maybe Types.PathName)
ppPathName = Lens.field @"pathName"
{-# DEPRECATED ppPathName "Use generic-lens or generic-optics with 'pathName' instead." #-}

-- | The product identifier. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProductId :: Lens.Lens' ProvisionProduct (Core.Maybe Types.ProductId)
ppProductId = Lens.field @"productId"
{-# DEPRECATED ppProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The name of the product. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'productName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProductName :: Lens.Lens' ProvisionProduct (Core.Maybe Types.ProductName)
ppProductName = Lens.field @"productName"
{-# DEPRECATED ppProductName "Use generic-lens or generic-optics with 'productName' instead." #-}

-- | The identifier of the provisioning artifact. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisioningArtifactId :: Lens.Lens' ProvisionProduct (Core.Maybe Types.ProvisioningArtifactId)
ppProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# DEPRECATED ppProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The name of the provisioning artifact. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisioningArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisioningArtifactName :: Lens.Lens' ProvisionProduct (Core.Maybe Types.ProvisioningArtifactName)
ppProvisioningArtifactName = Lens.field @"provisioningArtifactName"
{-# DEPRECATED ppProvisioningArtifactName "Use generic-lens or generic-optics with 'provisioningArtifactName' instead." #-}

-- | Parameters specified by the administrator that are required for provisioning the product.
--
-- /Note:/ Consider using 'provisioningParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisioningParameters :: Lens.Lens' ProvisionProduct (Core.Maybe [Types.ProvisioningParameter])
ppProvisioningParameters = Lens.field @"provisioningParameters"
{-# DEPRECATED ppProvisioningParameters "Use generic-lens or generic-optics with 'provisioningParameters' instead." #-}

-- | An object that contains information about the provisioning preferences for a stack set.
--
-- /Note:/ Consider using 'provisioningPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisioningPreferences :: Lens.Lens' ProvisionProduct (Core.Maybe Types.ProvisioningPreferences)
ppProvisioningPreferences = Lens.field @"provisioningPreferences"
{-# DEPRECATED ppProvisioningPreferences "Use generic-lens or generic-optics with 'provisioningPreferences' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppTags :: Lens.Lens' ProvisionProduct (Core.Maybe [Types.Tag])
ppTags = Lens.field @"tags"
{-# DEPRECATED ppTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON ProvisionProduct where
  toJSON ProvisionProduct {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ProvisionedProductName" Core..= provisionedProductName),
            Core.Just ("ProvisionToken" Core..= provisionToken),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("NotificationArns" Core..=) Core.<$> notificationArns,
            ("PathId" Core..=) Core.<$> pathId,
            ("PathName" Core..=) Core.<$> pathName,
            ("ProductId" Core..=) Core.<$> productId,
            ("ProductName" Core..=) Core.<$> productName,
            ("ProvisioningArtifactId" Core..=) Core.<$> provisioningArtifactId,
            ("ProvisioningArtifactName" Core..=)
              Core.<$> provisioningArtifactName,
            ("ProvisioningParameters" Core..=) Core.<$> provisioningParameters,
            ("ProvisioningPreferences" Core..=)
              Core.<$> provisioningPreferences,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest ProvisionProduct where
  type Rs ProvisionProduct = ProvisionProductResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.ProvisionProduct")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ProvisionProductResponse'
            Core.<$> (x Core..:? "RecordDetail") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkProvisionProductResponse' smart constructor.
data ProvisionProductResponse = ProvisionProductResponse'
  { -- | Information about the result of provisioning the product.
    recordDetail :: Core.Maybe Types.RecordDetail,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ProvisionProductResponse' value with any optional fields omitted.
mkProvisionProductResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ProvisionProductResponse
mkProvisionProductResponse responseStatus =
  ProvisionProductResponse'
    { recordDetail = Core.Nothing,
      responseStatus
    }

-- | Information about the result of provisioning the product.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprrsRecordDetail :: Lens.Lens' ProvisionProductResponse (Core.Maybe Types.RecordDetail)
pprrsRecordDetail = Lens.field @"recordDetail"
{-# DEPRECATED pprrsRecordDetail "Use generic-lens or generic-optics with 'recordDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprrsResponseStatus :: Lens.Lens' ProvisionProductResponse Core.Int
pprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
