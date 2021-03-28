{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioning artifact (also known as a version) for the specified product.
module Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
    (
    -- * Creating a request
      DescribeProvisioningArtifact (..)
    , mkDescribeProvisioningArtifact
    -- ** Request lenses
    , dpaAcceptLanguage
    , dpaProductId
    , dpaProductName
    , dpaProvisioningArtifactId
    , dpaProvisioningArtifactName
    , dpaVerbose

    -- * Destructuring the response
    , DescribeProvisioningArtifactResponse (..)
    , mkDescribeProvisioningArtifactResponse
    -- ** Response lenses
    , dparrsInfo
    , dparrsProvisioningArtifactDetail
    , dparrsStatus
    , dparrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeProvisioningArtifact' smart constructor.
data DescribeProvisioningArtifact = DescribeProvisioningArtifact'
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
  , productId :: Core.Maybe Types.ProductId
    -- ^ The product identifier.
  , productName :: Core.Maybe Types.ProductName
    -- ^ The product name.
  , provisioningArtifactId :: Core.Maybe Types.ProvisioningArtifactId
    -- ^ The identifier of the provisioning artifact.
  , provisioningArtifactName :: Core.Maybe Types.ProvisioningArtifactName
    -- ^ The provisioning artifact name.
  , verbose :: Core.Maybe Core.Bool
    -- ^ Indicates whether a verbose level of detail is enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProvisioningArtifact' value with any optional fields omitted.
mkDescribeProvisioningArtifact
    :: DescribeProvisioningArtifact
mkDescribeProvisioningArtifact
  = DescribeProvisioningArtifact'{acceptLanguage = Core.Nothing,
                                  productId = Core.Nothing, productName = Core.Nothing,
                                  provisioningArtifactId = Core.Nothing,
                                  provisioningArtifactName = Core.Nothing, verbose = Core.Nothing}

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
dpaAcceptLanguage :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Types.AcceptLanguage)
dpaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE dpaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaProductId :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Types.ProductId)
dpaProductId = Lens.field @"productId"
{-# INLINEABLE dpaProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The product name.
--
-- /Note:/ Consider using 'productName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaProductName :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Types.ProductName)
dpaProductName = Lens.field @"productName"
{-# INLINEABLE dpaProductName #-}
{-# DEPRECATED productName "Use generic-lens or generic-optics with 'productName' instead"  #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaProvisioningArtifactId :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Types.ProvisioningArtifactId)
dpaProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE dpaProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

-- | The provisioning artifact name.
--
-- /Note:/ Consider using 'provisioningArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaProvisioningArtifactName :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Types.ProvisioningArtifactName)
dpaProvisioningArtifactName = Lens.field @"provisioningArtifactName"
{-# INLINEABLE dpaProvisioningArtifactName #-}
{-# DEPRECATED provisioningArtifactName "Use generic-lens or generic-optics with 'provisioningArtifactName' instead"  #-}

-- | Indicates whether a verbose level of detail is enabled.
--
-- /Note:/ Consider using 'verbose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaVerbose :: Lens.Lens' DescribeProvisioningArtifact (Core.Maybe Core.Bool)
dpaVerbose = Lens.field @"verbose"
{-# INLINEABLE dpaVerbose #-}
{-# DEPRECATED verbose "Use generic-lens or generic-optics with 'verbose' instead"  #-}

instance Core.ToQuery DescribeProvisioningArtifact where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeProvisioningArtifact where
        toHeaders DescribeProvisioningArtifact{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DescribeProvisioningArtifact")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeProvisioningArtifact where
        toJSON DescribeProvisioningArtifact{..}
          = Core.object
              (Core.catMaybes
                 [("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("ProductId" Core..=) Core.<$> productId,
                  ("ProductName" Core..=) Core.<$> productName,
                  ("ProvisioningArtifactId" Core..=) Core.<$> provisioningArtifactId,
                  ("ProvisioningArtifactName" Core..=) Core.<$>
                    provisioningArtifactName,
                  ("Verbose" Core..=) Core.<$> verbose])

instance Core.AWSRequest DescribeProvisioningArtifact where
        type Rs DescribeProvisioningArtifact =
             DescribeProvisioningArtifactResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeProvisioningArtifactResponse' Core.<$>
                   (x Core..:? "Info") Core.<*>
                     x Core..:? "ProvisioningArtifactDetail"
                     Core.<*> x Core..:? "Status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeProvisioningArtifactResponse' smart constructor.
data DescribeProvisioningArtifactResponse = DescribeProvisioningArtifactResponse'
  { info :: Core.Maybe (Core.HashMap Types.ProvisioningArtifactInfoKey Types.ProvisioningArtifactInfoValue)
    -- ^ The URL of the CloudFormation template in Amazon S3.
  , provisioningArtifactDetail :: Core.Maybe Types.ProvisioningArtifactDetail
    -- ^ Information about the provisioning artifact.
  , status :: Core.Maybe Types.RequestStatus
    -- ^ The status of the current request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeProvisioningArtifactResponse' value with any optional fields omitted.
mkDescribeProvisioningArtifactResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeProvisioningArtifactResponse
mkDescribeProvisioningArtifactResponse responseStatus
  = DescribeProvisioningArtifactResponse'{info = Core.Nothing,
                                          provisioningArtifactDetail = Core.Nothing,
                                          status = Core.Nothing, responseStatus}

-- | The URL of the CloudFormation template in Amazon S3.
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparrsInfo :: Lens.Lens' DescribeProvisioningArtifactResponse (Core.Maybe (Core.HashMap Types.ProvisioningArtifactInfoKey Types.ProvisioningArtifactInfoValue))
dparrsInfo = Lens.field @"info"
{-# INLINEABLE dparrsInfo #-}
{-# DEPRECATED info "Use generic-lens or generic-optics with 'info' instead"  #-}

-- | Information about the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparrsProvisioningArtifactDetail :: Lens.Lens' DescribeProvisioningArtifactResponse (Core.Maybe Types.ProvisioningArtifactDetail)
dparrsProvisioningArtifactDetail = Lens.field @"provisioningArtifactDetail"
{-# INLINEABLE dparrsProvisioningArtifactDetail #-}
{-# DEPRECATED provisioningArtifactDetail "Use generic-lens or generic-optics with 'provisioningArtifactDetail' instead"  #-}

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparrsStatus :: Lens.Lens' DescribeProvisioningArtifactResponse (Core.Maybe Types.RequestStatus)
dparrsStatus = Lens.field @"status"
{-# INLINEABLE dparrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparrsResponseStatus :: Lens.Lens' DescribeProvisioningArtifactResponse Core.Int
dparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
