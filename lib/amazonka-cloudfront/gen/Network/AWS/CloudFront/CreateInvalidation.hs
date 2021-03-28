{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateInvalidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new invalidation. 
module Network.AWS.CloudFront.CreateInvalidation
    (
    -- * Creating a request
      CreateInvalidation (..)
    , mkCreateInvalidation
    -- ** Request lenses
    , ciDistributionId
    , ciInvalidationBatch

    -- * Destructuring the response
    , CreateInvalidationResponse (..)
    , mkCreateInvalidationResponse
    -- ** Response lenses
    , cirrsInvalidation
    , cirrsLocation
    , cirrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create an invalidation.
--
-- /See:/ 'mkCreateInvalidation' smart constructor.
data CreateInvalidation = CreateInvalidation'
  { distributionId :: Core.Text
    -- ^ The distribution's id.
  , invalidationBatch :: Types.InvalidationBatch
    -- ^ The batch information for the invalidation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInvalidation' value with any optional fields omitted.
mkCreateInvalidation
    :: Core.Text -- ^ 'distributionId'
    -> Types.InvalidationBatch -- ^ 'invalidationBatch'
    -> CreateInvalidation
mkCreateInvalidation distributionId invalidationBatch
  = CreateInvalidation'{distributionId, invalidationBatch}

-- | The distribution's id.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDistributionId :: Lens.Lens' CreateInvalidation Core.Text
ciDistributionId = Lens.field @"distributionId"
{-# INLINEABLE ciDistributionId #-}
{-# DEPRECATED distributionId "Use generic-lens or generic-optics with 'distributionId' instead"  #-}

-- | The batch information for the invalidation.
--
-- /Note:/ Consider using 'invalidationBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInvalidationBatch :: Lens.Lens' CreateInvalidation Types.InvalidationBatch
ciInvalidationBatch = Lens.field @"invalidationBatch"
{-# INLINEABLE ciInvalidationBatch #-}
{-# DEPRECATED invalidationBatch "Use generic-lens or generic-optics with 'invalidationBatch' instead"  #-}

instance Core.ToQuery CreateInvalidation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateInvalidation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateInvalidation where
        type Rs CreateInvalidation = CreateInvalidationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2020-05-31/distribution/" Core.<> Core.toText distributionId
                             Core.<> "/invalidation",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateInvalidationResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.parseHeaderMaybe "Location" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateInvalidationResponse' smart constructor.
data CreateInvalidationResponse = CreateInvalidationResponse'
  { invalidation :: Core.Maybe Types.Invalidation
    -- ^ The invalidation's information.
  , location :: Core.Maybe Core.Text
    -- ^ The fully qualified URI of the distribution and invalidation batch request, including the @Invalidation ID@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateInvalidationResponse' value with any optional fields omitted.
mkCreateInvalidationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateInvalidationResponse
mkCreateInvalidationResponse responseStatus
  = CreateInvalidationResponse'{invalidation = Core.Nothing,
                                location = Core.Nothing, responseStatus}

-- | The invalidation's information.
--
-- /Note:/ Consider using 'invalidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsInvalidation :: Lens.Lens' CreateInvalidationResponse (Core.Maybe Types.Invalidation)
cirrsInvalidation = Lens.field @"invalidation"
{-# INLINEABLE cirrsInvalidation #-}
{-# DEPRECATED invalidation "Use generic-lens or generic-optics with 'invalidation' instead"  #-}

-- | The fully qualified URI of the distribution and invalidation batch request, including the @Invalidation ID@ .
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsLocation :: Lens.Lens' CreateInvalidationResponse (Core.Maybe Core.Text)
cirrsLocation = Lens.field @"location"
{-# INLINEABLE cirrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CreateInvalidationResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
