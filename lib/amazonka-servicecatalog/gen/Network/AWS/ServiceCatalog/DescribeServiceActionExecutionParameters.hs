{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeServiceActionExecutionParameters (..)
    , mkDescribeServiceActionExecutionParameters
    -- ** Request lenses
    , dsaepProvisionedProductId
    , dsaepServiceActionId
    , dsaepAcceptLanguage

    -- * Destructuring the response
    , DescribeServiceActionExecutionParametersResponse (..)
    , mkDescribeServiceActionExecutionParametersResponse
    -- ** Response lenses
    , dsaeprrsServiceActionParameters
    , dsaeprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeServiceActionExecutionParameters' smart constructor.
data DescribeServiceActionExecutionParameters = DescribeServiceActionExecutionParameters'
  { provisionedProductId :: Types.ProvisionedProductId
    -- ^ The identifier of the provisioned product.
  , serviceActionId :: Types.ServiceActionId
    -- ^ The self-service action identifier.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServiceActionExecutionParameters' value with any optional fields omitted.
mkDescribeServiceActionExecutionParameters
    :: Types.ProvisionedProductId -- ^ 'provisionedProductId'
    -> Types.ServiceActionId -- ^ 'serviceActionId'
    -> DescribeServiceActionExecutionParameters
mkDescribeServiceActionExecutionParameters provisionedProductId
  serviceActionId
  = DescribeServiceActionExecutionParameters'{provisionedProductId,
                                              serviceActionId, acceptLanguage = Core.Nothing}

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaepProvisionedProductId :: Lens.Lens' DescribeServiceActionExecutionParameters Types.ProvisionedProductId
dsaepProvisionedProductId = Lens.field @"provisionedProductId"
{-# INLINEABLE dsaepProvisionedProductId #-}
{-# DEPRECATED provisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead"  #-}

-- | The self-service action identifier.
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaepServiceActionId :: Lens.Lens' DescribeServiceActionExecutionParameters Types.ServiceActionId
dsaepServiceActionId = Lens.field @"serviceActionId"
{-# INLINEABLE dsaepServiceActionId #-}
{-# DEPRECATED serviceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead"  #-}

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
{-# INLINEABLE dsaepAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery DescribeServiceActionExecutionParameters
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeServiceActionExecutionParameters
         where
        toHeaders DescribeServiceActionExecutionParameters{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DescribeServiceActionExecutionParameters")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeServiceActionExecutionParameters
         where
        toJSON DescribeServiceActionExecutionParameters{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProvisionedProductId" Core..= provisionedProductId),
                  Core.Just ("ServiceActionId" Core..= serviceActionId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest DescribeServiceActionExecutionParameters
         where
        type Rs DescribeServiceActionExecutionParameters =
             DescribeServiceActionExecutionParametersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeServiceActionExecutionParametersResponse' Core.<$>
                   (x Core..:? "ServiceActionParameters") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeServiceActionExecutionParametersResponse' smart constructor.
data DescribeServiceActionExecutionParametersResponse = DescribeServiceActionExecutionParametersResponse'
  { serviceActionParameters :: Core.Maybe [Types.ExecutionParameter]
    -- ^ The parameters of the self-service action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServiceActionExecutionParametersResponse' value with any optional fields omitted.
mkDescribeServiceActionExecutionParametersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeServiceActionExecutionParametersResponse
mkDescribeServiceActionExecutionParametersResponse responseStatus
  = DescribeServiceActionExecutionParametersResponse'{serviceActionParameters
                                                        = Core.Nothing,
                                                      responseStatus}

-- | The parameters of the self-service action.
--
-- /Note:/ Consider using 'serviceActionParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaeprrsServiceActionParameters :: Lens.Lens' DescribeServiceActionExecutionParametersResponse (Core.Maybe [Types.ExecutionParameter])
dsaeprrsServiceActionParameters = Lens.field @"serviceActionParameters"
{-# INLINEABLE dsaeprrsServiceActionParameters #-}
{-# DEPRECATED serviceActionParameters "Use generic-lens or generic-optics with 'serviceActionParameters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaeprrsResponseStatus :: Lens.Lens' DescribeServiceActionExecutionParametersResponse Core.Int
dsaeprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsaeprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
