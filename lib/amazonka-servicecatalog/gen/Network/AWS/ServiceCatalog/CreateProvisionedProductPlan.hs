{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateProvisionedProductPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a plan. A plan includes the list of resources to be created (when provisioning a new product) or modified (when updating a provisioned product) when the plan is executed.
--
-- You can create one plan per provisioned product. To create a plan for an existing provisioned product, the product status must be AVAILBLE or TAINTED.
-- To view the resource changes in the change set, use 'DescribeProvisionedProductPlan' . To create or modify the provisioned product, use 'ExecuteProvisionedProductPlan' .
module Network.AWS.ServiceCatalog.CreateProvisionedProductPlan
    (
    -- * Creating a request
      CreateProvisionedProductPlan (..)
    , mkCreateProvisionedProductPlan
    -- ** Request lenses
    , cpppPlanName
    , cpppPlanType
    , cpppProductId
    , cpppProvisionedProductName
    , cpppProvisioningArtifactId
    , cpppIdempotencyToken
    , cpppAcceptLanguage
    , cpppNotificationArns
    , cpppPathId
    , cpppProvisioningParameters
    , cpppTags

    -- * Destructuring the response
    , CreateProvisionedProductPlanResponse (..)
    , mkCreateProvisionedProductPlanResponse
    -- ** Response lenses
    , cppprrsPlanId
    , cppprrsPlanName
    , cppprrsProvisionProductId
    , cppprrsProvisionedProductName
    , cppprrsProvisioningArtifactId
    , cppprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCreateProvisionedProductPlan' smart constructor.
data CreateProvisionedProductPlan = CreateProvisionedProductPlan'
  { planName :: Types.ProvisionedProductPlanName
    -- ^ The name of the plan.
  , planType :: Types.ProvisionedProductPlanType
    -- ^ The plan type.
  , productId :: Types.ProductId
    -- ^ The product identifier.
  , provisionedProductName :: Types.ProvisionedProductName
    -- ^ A user-friendly name for the provisioned product. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
  , provisioningArtifactId :: Types.ProvisioningArtifactId
    -- ^ The identifier of the provisioning artifact.
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
  , notificationArns :: Core.Maybe [Types.NotificationArn]
    -- ^ Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
  , pathId :: Core.Maybe Types.PathId
    -- ^ The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
  , provisioningParameters :: Core.Maybe [Types.UpdateProvisioningParameter]
    -- ^ Parameters specified by the administrator that are required for provisioning the product.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ One or more tags.
--
-- If the plan is for an existing provisioned product, the product must have a @RESOURCE_UPDATE@ constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProvisionedProductPlan' value with any optional fields omitted.
mkCreateProvisionedProductPlan
    :: Types.ProvisionedProductPlanName -- ^ 'planName'
    -> Types.ProvisionedProductPlanType -- ^ 'planType'
    -> Types.ProductId -- ^ 'productId'
    -> Types.ProvisionedProductName -- ^ 'provisionedProductName'
    -> Types.ProvisioningArtifactId -- ^ 'provisioningArtifactId'
    -> Types.IdempotencyToken -- ^ 'idempotencyToken'
    -> CreateProvisionedProductPlan
mkCreateProvisionedProductPlan planName planType productId
  provisionedProductName provisioningArtifactId idempotencyToken
  = CreateProvisionedProductPlan'{planName, planType, productId,
                                  provisionedProductName, provisioningArtifactId, idempotencyToken,
                                  acceptLanguage = Core.Nothing, notificationArns = Core.Nothing,
                                  pathId = Core.Nothing, provisioningParameters = Core.Nothing,
                                  tags = Core.Nothing}

-- | The name of the plan.
--
-- /Note:/ Consider using 'planName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpppPlanName :: Lens.Lens' CreateProvisionedProductPlan Types.ProvisionedProductPlanName
cpppPlanName = Lens.field @"planName"
{-# INLINEABLE cpppPlanName #-}
{-# DEPRECATED planName "Use generic-lens or generic-optics with 'planName' instead"  #-}

-- | The plan type.
--
-- /Note:/ Consider using 'planType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpppPlanType :: Lens.Lens' CreateProvisionedProductPlan Types.ProvisionedProductPlanType
cpppPlanType = Lens.field @"planType"
{-# INLINEABLE cpppPlanType #-}
{-# DEPRECATED planType "Use generic-lens or generic-optics with 'planType' instead"  #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpppProductId :: Lens.Lens' CreateProvisionedProductPlan Types.ProductId
cpppProductId = Lens.field @"productId"
{-# INLINEABLE cpppProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | A user-friendly name for the provisioned product. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpppProvisionedProductName :: Lens.Lens' CreateProvisionedProductPlan Types.ProvisionedProductName
cpppProvisionedProductName = Lens.field @"provisionedProductName"
{-# INLINEABLE cpppProvisionedProductName #-}
{-# DEPRECATED provisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead"  #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpppProvisioningArtifactId :: Lens.Lens' CreateProvisionedProductPlan Types.ProvisioningArtifactId
cpppProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE cpppProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpppIdempotencyToken :: Lens.Lens' CreateProvisionedProductPlan Types.IdempotencyToken
cpppIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE cpppIdempotencyToken #-}
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
cpppAcceptLanguage :: Lens.Lens' CreateProvisionedProductPlan (Core.Maybe Types.AcceptLanguage)
cpppAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE cpppAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
--
-- /Note:/ Consider using 'notificationArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpppNotificationArns :: Lens.Lens' CreateProvisionedProductPlan (Core.Maybe [Types.NotificationArn])
cpppNotificationArns = Lens.field @"notificationArns"
{-# INLINEABLE cpppNotificationArns #-}
{-# DEPRECATED notificationArns "Use generic-lens or generic-optics with 'notificationArns' instead"  #-}

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpppPathId :: Lens.Lens' CreateProvisionedProductPlan (Core.Maybe Types.PathId)
cpppPathId = Lens.field @"pathId"
{-# INLINEABLE cpppPathId #-}
{-# DEPRECATED pathId "Use generic-lens or generic-optics with 'pathId' instead"  #-}

-- | Parameters specified by the administrator that are required for provisioning the product.
--
-- /Note:/ Consider using 'provisioningParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpppProvisioningParameters :: Lens.Lens' CreateProvisionedProductPlan (Core.Maybe [Types.UpdateProvisioningParameter])
cpppProvisioningParameters = Lens.field @"provisioningParameters"
{-# INLINEABLE cpppProvisioningParameters #-}
{-# DEPRECATED provisioningParameters "Use generic-lens or generic-optics with 'provisioningParameters' instead"  #-}

-- | One or more tags.
--
-- If the plan is for an existing provisioned product, the product must have a @RESOURCE_UPDATE@ constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpppTags :: Lens.Lens' CreateProvisionedProductPlan (Core.Maybe [Types.Tag])
cpppTags = Lens.field @"tags"
{-# INLINEABLE cpppTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateProvisionedProductPlan where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateProvisionedProductPlan where
        toHeaders CreateProvisionedProductPlan{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.CreateProvisionedProductPlan")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateProvisionedProductPlan where
        toJSON CreateProvisionedProductPlan{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PlanName" Core..= planName),
                  Core.Just ("PlanType" Core..= planType),
                  Core.Just ("ProductId" Core..= productId),
                  Core.Just
                    ("ProvisionedProductName" Core..= provisionedProductName),
                  Core.Just
                    ("ProvisioningArtifactId" Core..= provisioningArtifactId),
                  Core.Just ("IdempotencyToken" Core..= idempotencyToken),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("NotificationArns" Core..=) Core.<$> notificationArns,
                  ("PathId" Core..=) Core.<$> pathId,
                  ("ProvisioningParameters" Core..=) Core.<$> provisioningParameters,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateProvisionedProductPlan where
        type Rs CreateProvisionedProductPlan =
             CreateProvisionedProductPlanResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateProvisionedProductPlanResponse' Core.<$>
                   (x Core..:? "PlanId") Core.<*> x Core..:? "PlanName" Core.<*>
                     x Core..:? "ProvisionProductId"
                     Core.<*> x Core..:? "ProvisionedProductName"
                     Core.<*> x Core..:? "ProvisioningArtifactId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateProvisionedProductPlanResponse' smart constructor.
data CreateProvisionedProductPlanResponse = CreateProvisionedProductPlanResponse'
  { planId :: Core.Maybe Types.Id
    -- ^ The plan identifier.
  , planName :: Core.Maybe Types.ProvisionedProductPlanName
    -- ^ The name of the plan.
  , provisionProductId :: Core.Maybe Types.Id
    -- ^ The product identifier.
  , provisionedProductName :: Core.Maybe Types.ProvisionedProductName
    -- ^ The user-friendly name of the provisioned product.
  , provisioningArtifactId :: Core.Maybe Types.Id
    -- ^ The identifier of the provisioning artifact.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProvisionedProductPlanResponse' value with any optional fields omitted.
mkCreateProvisionedProductPlanResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateProvisionedProductPlanResponse
mkCreateProvisionedProductPlanResponse responseStatus
  = CreateProvisionedProductPlanResponse'{planId = Core.Nothing,
                                          planName = Core.Nothing,
                                          provisionProductId = Core.Nothing,
                                          provisionedProductName = Core.Nothing,
                                          provisioningArtifactId = Core.Nothing, responseStatus}

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cppprrsPlanId :: Lens.Lens' CreateProvisionedProductPlanResponse (Core.Maybe Types.Id)
cppprrsPlanId = Lens.field @"planId"
{-# INLINEABLE cppprrsPlanId #-}
{-# DEPRECATED planId "Use generic-lens or generic-optics with 'planId' instead"  #-}

-- | The name of the plan.
--
-- /Note:/ Consider using 'planName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cppprrsPlanName :: Lens.Lens' CreateProvisionedProductPlanResponse (Core.Maybe Types.ProvisionedProductPlanName)
cppprrsPlanName = Lens.field @"planName"
{-# INLINEABLE cppprrsPlanName #-}
{-# DEPRECATED planName "Use generic-lens or generic-optics with 'planName' instead"  #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'provisionProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cppprrsProvisionProductId :: Lens.Lens' CreateProvisionedProductPlanResponse (Core.Maybe Types.Id)
cppprrsProvisionProductId = Lens.field @"provisionProductId"
{-# INLINEABLE cppprrsProvisionProductId #-}
{-# DEPRECATED provisionProductId "Use generic-lens or generic-optics with 'provisionProductId' instead"  #-}

-- | The user-friendly name of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cppprrsProvisionedProductName :: Lens.Lens' CreateProvisionedProductPlanResponse (Core.Maybe Types.ProvisionedProductName)
cppprrsProvisionedProductName = Lens.field @"provisionedProductName"
{-# INLINEABLE cppprrsProvisionedProductName #-}
{-# DEPRECATED provisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead"  #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cppprrsProvisioningArtifactId :: Lens.Lens' CreateProvisionedProductPlanResponse (Core.Maybe Types.Id)
cppprrsProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE cppprrsProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cppprrsResponseStatus :: Lens.Lens' CreateProvisionedProductPlanResponse Core.Int
cppprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cppprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
