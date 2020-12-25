{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateInvalidation (..),
    mkCreateInvalidation,

    -- ** Request lenses
    ciDistributionId,
    ciInvalidationBatch,

    -- * Destructuring the response
    CreateInvalidationResponse (..),
    mkCreateInvalidationResponse,

    -- ** Response lenses
    cirrsInvalidation,
    cirrsLocation,
    cirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create an invalidation.
--
-- /See:/ 'mkCreateInvalidation' smart constructor.
data CreateInvalidation = CreateInvalidation'
  { -- | The distribution's id.
    distributionId :: Types.String,
    -- | The batch information for the invalidation.
    invalidationBatch :: Types.InvalidationBatch
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInvalidation' value with any optional fields omitted.
mkCreateInvalidation ::
  -- | 'distributionId'
  Types.String ->
  -- | 'invalidationBatch'
  Types.InvalidationBatch ->
  CreateInvalidation
mkCreateInvalidation distributionId invalidationBatch =
  CreateInvalidation' {distributionId, invalidationBatch}

-- | The distribution's id.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDistributionId :: Lens.Lens' CreateInvalidation Types.String
ciDistributionId = Lens.field @"distributionId"
{-# DEPRECATED ciDistributionId "Use generic-lens or generic-optics with 'distributionId' instead." #-}

-- | The batch information for the invalidation.
--
-- /Note:/ Consider using 'invalidationBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInvalidationBatch :: Lens.Lens' CreateInvalidation Types.InvalidationBatch
ciInvalidationBatch = Lens.field @"invalidationBatch"
{-# DEPRECATED ciInvalidationBatch "Use generic-lens or generic-optics with 'invalidationBatch' instead." #-}

instance Core.AWSRequest CreateInvalidation where
  type Rs CreateInvalidation = CreateInvalidationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/distribution/" Core.<> (Core.toText distributionId)
                Core.<> ("/invalidation")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInvalidationResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateInvalidationResponse' smart constructor.
data CreateInvalidationResponse = CreateInvalidationResponse'
  { -- | The invalidation's information.
    invalidation :: Core.Maybe Types.Invalidation,
    -- | The fully qualified URI of the distribution and invalidation batch request, including the @Invalidation ID@ .
    location :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateInvalidationResponse' value with any optional fields omitted.
mkCreateInvalidationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateInvalidationResponse
mkCreateInvalidationResponse responseStatus =
  CreateInvalidationResponse'
    { invalidation = Core.Nothing,
      location = Core.Nothing,
      responseStatus
    }

-- | The invalidation's information.
--
-- /Note:/ Consider using 'invalidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsInvalidation :: Lens.Lens' CreateInvalidationResponse (Core.Maybe Types.Invalidation)
cirrsInvalidation = Lens.field @"invalidation"
{-# DEPRECATED cirrsInvalidation "Use generic-lens or generic-optics with 'invalidation' instead." #-}

-- | The fully qualified URI of the distribution and invalidation batch request, including the @Invalidation ID@ .
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsLocation :: Lens.Lens' CreateInvalidationResponse (Core.Maybe Types.String)
cirrsLocation = Lens.field @"location"
{-# DEPRECATED cirrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CreateInvalidationResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
