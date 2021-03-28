{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisioningParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the configuration required to provision the specified product using the specified provisioning artifact.
--
-- If the output contains a TagOption key with an empty list of values, there is a TagOption conflict for that key. The end user cannot take action to fix the conflict, and launch is not blocked. In subsequent calls to 'ProvisionProduct' , do not include conflicted TagOption keys as tags, or this causes the error "Parameter validation failed: Missing required parameter in Tags[/N/ ]:/Value/ ". Tag the provisioned product with the value @sc-tagoption-conflict-portfolioId-productId@ .
module Network.AWS.ServiceCatalog.DescribeProvisioningParameters
    (
    -- * Creating a request
      DescribeProvisioningParameters (..)
    , mkDescribeProvisioningParameters
    -- ** Request lenses
    , dppAcceptLanguage
    , dppPathId
    , dppPathName
    , dppProductId
    , dppProductName
    , dppProvisioningArtifactId
    , dppProvisioningArtifactName

    -- * Destructuring the response
    , DescribeProvisioningParametersResponse (..)
    , mkDescribeProvisioningParametersResponse
    -- ** Response lenses
    , dpprrsConstraintSummaries
    , dpprrsProvisioningArtifactOutputs
    , dpprrsProvisioningArtifactParameters
    , dpprrsProvisioningArtifactPreferences
    , dpprrsTagOptions
    , dpprrsUsageInstructions
    , dpprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeProvisioningParameters' smart constructor.
data DescribeProvisioningParameters = DescribeProvisioningParameters'
  { acceptLanguage :: Core.Maybe Types.AcceptLanguage
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
  , pathId :: Core.Maybe Types.Id
    -- ^ The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' . You must provide the name or ID, but not both.
  , pathName :: Core.Maybe Types.PortfolioDisplayName
    -- ^ The name of the path. You must provide the name or ID, but not both.
  , productId :: Core.Maybe Types.Id
    -- ^ The product identifier. You must provide the product name or ID, but not both.
  , productName :: Core.Maybe Types.ProductName
    -- ^ The name of the product. You must provide the name or ID, but not both.
  , provisioningArtifactId :: Core.Maybe Types.Id
    -- ^ The identifier of the provisioning artifact. You must provide the name or ID, but not both.
  , provisioningArtifactName :: Core.Maybe Types.ProvisioningArtifactName
    -- ^ The name of the provisioning artifact. You must provide the name or ID, but not both.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProvisioningParameters' value with any optional fields omitted.
mkDescribeProvisioningParameters
    :: DescribeProvisioningParameters
mkDescribeProvisioningParameters
  = DescribeProvisioningParameters'{acceptLanguage = Core.Nothing,
                                    pathId = Core.Nothing, pathName = Core.Nothing,
                                    productId = Core.Nothing, productName = Core.Nothing,
                                    provisioningArtifactId = Core.Nothing,
                                    provisioningArtifactName = Core.Nothing}

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
dppAcceptLanguage :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Types.AcceptLanguage)
dppAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE dppAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' . You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppPathId :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Types.Id)
dppPathId = Lens.field @"pathId"
{-# INLINEABLE dppPathId #-}
{-# DEPRECATED pathId "Use generic-lens or generic-optics with 'pathId' instead"  #-}

-- | The name of the path. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppPathName :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Types.PortfolioDisplayName)
dppPathName = Lens.field @"pathName"
{-# INLINEABLE dppPathName #-}
{-# DEPRECATED pathName "Use generic-lens or generic-optics with 'pathName' instead"  #-}

-- | The product identifier. You must provide the product name or ID, but not both.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppProductId :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Types.Id)
dppProductId = Lens.field @"productId"
{-# INLINEABLE dppProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The name of the product. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'productName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppProductName :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Types.ProductName)
dppProductName = Lens.field @"productName"
{-# INLINEABLE dppProductName #-}
{-# DEPRECATED productName "Use generic-lens or generic-optics with 'productName' instead"  #-}

-- | The identifier of the provisioning artifact. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppProvisioningArtifactId :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Types.Id)
dppProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE dppProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

-- | The name of the provisioning artifact. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisioningArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppProvisioningArtifactName :: Lens.Lens' DescribeProvisioningParameters (Core.Maybe Types.ProvisioningArtifactName)
dppProvisioningArtifactName = Lens.field @"provisioningArtifactName"
{-# INLINEABLE dppProvisioningArtifactName #-}
{-# DEPRECATED provisioningArtifactName "Use generic-lens or generic-optics with 'provisioningArtifactName' instead"  #-}

instance Core.ToQuery DescribeProvisioningParameters where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeProvisioningParameters where
        toHeaders DescribeProvisioningParameters{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DescribeProvisioningParameters")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeProvisioningParameters where
        toJSON DescribeProvisioningParameters{..}
          = Core.object
              (Core.catMaybes
                 [("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PathId" Core..=) Core.<$> pathId,
                  ("PathName" Core..=) Core.<$> pathName,
                  ("ProductId" Core..=) Core.<$> productId,
                  ("ProductName" Core..=) Core.<$> productName,
                  ("ProvisioningArtifactId" Core..=) Core.<$> provisioningArtifactId,
                  ("ProvisioningArtifactName" Core..=) Core.<$>
                    provisioningArtifactName])

instance Core.AWSRequest DescribeProvisioningParameters where
        type Rs DescribeProvisioningParameters =
             DescribeProvisioningParametersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeProvisioningParametersResponse' Core.<$>
                   (x Core..:? "ConstraintSummaries") Core.<*>
                     x Core..:? "ProvisioningArtifactOutputs"
                     Core.<*> x Core..:? "ProvisioningArtifactParameters"
                     Core.<*> x Core..:? "ProvisioningArtifactPreferences"
                     Core.<*> x Core..:? "TagOptions"
                     Core.<*> x Core..:? "UsageInstructions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeProvisioningParametersResponse' smart constructor.
data DescribeProvisioningParametersResponse = DescribeProvisioningParametersResponse'
  { constraintSummaries :: Core.Maybe [Types.ConstraintSummary]
    -- ^ Information about the constraints used to provision the product.
  , provisioningArtifactOutputs :: Core.Maybe [Types.ProvisioningArtifactOutput]
    -- ^ The output of the provisioning artifact.
  , provisioningArtifactParameters :: Core.Maybe [Types.ProvisioningArtifactParameter]
    -- ^ Information about the parameters used to provision the product.
  , provisioningArtifactPreferences :: Core.Maybe Types.ProvisioningArtifactPreferences
    -- ^ An object that contains information about preferences, such as regions and accounts, for the provisioning artifact.
  , tagOptions :: Core.Maybe [Types.TagOptionSummary]
    -- ^ Information about the TagOptions associated with the resource.
  , usageInstructions :: Core.Maybe [Types.UsageInstruction]
    -- ^ Any additional metadata specifically related to the provisioning of the product. For example, see the @Version@ field of the CloudFormation template.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProvisioningParametersResponse' value with any optional fields omitted.
mkDescribeProvisioningParametersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeProvisioningParametersResponse
mkDescribeProvisioningParametersResponse responseStatus
  = DescribeProvisioningParametersResponse'{constraintSummaries =
                                              Core.Nothing,
                                            provisioningArtifactOutputs = Core.Nothing,
                                            provisioningArtifactParameters = Core.Nothing,
                                            provisioningArtifactPreferences = Core.Nothing,
                                            tagOptions = Core.Nothing,
                                            usageInstructions = Core.Nothing, responseStatus}

-- | Information about the constraints used to provision the product.
--
-- /Note:/ Consider using 'constraintSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsConstraintSummaries :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe [Types.ConstraintSummary])
dpprrsConstraintSummaries = Lens.field @"constraintSummaries"
{-# INLINEABLE dpprrsConstraintSummaries #-}
{-# DEPRECATED constraintSummaries "Use generic-lens or generic-optics with 'constraintSummaries' instead"  #-}

-- | The output of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsProvisioningArtifactOutputs :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe [Types.ProvisioningArtifactOutput])
dpprrsProvisioningArtifactOutputs = Lens.field @"provisioningArtifactOutputs"
{-# INLINEABLE dpprrsProvisioningArtifactOutputs #-}
{-# DEPRECATED provisioningArtifactOutputs "Use generic-lens or generic-optics with 'provisioningArtifactOutputs' instead"  #-}

-- | Information about the parameters used to provision the product.
--
-- /Note:/ Consider using 'provisioningArtifactParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsProvisioningArtifactParameters :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe [Types.ProvisioningArtifactParameter])
dpprrsProvisioningArtifactParameters = Lens.field @"provisioningArtifactParameters"
{-# INLINEABLE dpprrsProvisioningArtifactParameters #-}
{-# DEPRECATED provisioningArtifactParameters "Use generic-lens or generic-optics with 'provisioningArtifactParameters' instead"  #-}

-- | An object that contains information about preferences, such as regions and accounts, for the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsProvisioningArtifactPreferences :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe Types.ProvisioningArtifactPreferences)
dpprrsProvisioningArtifactPreferences = Lens.field @"provisioningArtifactPreferences"
{-# INLINEABLE dpprrsProvisioningArtifactPreferences #-}
{-# DEPRECATED provisioningArtifactPreferences "Use generic-lens or generic-optics with 'provisioningArtifactPreferences' instead"  #-}

-- | Information about the TagOptions associated with the resource.
--
-- /Note:/ Consider using 'tagOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsTagOptions :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe [Types.TagOptionSummary])
dpprrsTagOptions = Lens.field @"tagOptions"
{-# INLINEABLE dpprrsTagOptions #-}
{-# DEPRECATED tagOptions "Use generic-lens or generic-optics with 'tagOptions' instead"  #-}

-- | Any additional metadata specifically related to the provisioning of the product. For example, see the @Version@ field of the CloudFormation template.
--
-- /Note:/ Consider using 'usageInstructions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsUsageInstructions :: Lens.Lens' DescribeProvisioningParametersResponse (Core.Maybe [Types.UsageInstruction])
dpprrsUsageInstructions = Lens.field @"usageInstructions"
{-# INLINEABLE dpprrsUsageInstructions #-}
{-# DEPRECATED usageInstructions "Use generic-lens or generic-optics with 'usageInstructions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsResponseStatus :: Lens.Lens' DescribeProvisioningParametersResponse Core.Int
dpprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
