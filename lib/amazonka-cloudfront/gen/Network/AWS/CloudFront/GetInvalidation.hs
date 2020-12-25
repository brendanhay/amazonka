{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetInvalidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an invalidation.
module Network.AWS.CloudFront.GetInvalidation
  ( -- * Creating a request
    GetInvalidation (..),
    mkGetInvalidation,

    -- ** Request lenses
    giDistributionId,
    giId,

    -- * Destructuring the response
    GetInvalidationResponse (..),
    mkGetInvalidationResponse,

    -- ** Response lenses
    girrsInvalidation,
    girrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get an invalidation's information.
--
-- /See:/ 'mkGetInvalidation' smart constructor.
data GetInvalidation = GetInvalidation'
  { -- | The distribution's ID.
    distributionId :: Types.String,
    -- | The identifier for the invalidation request, for example, @IDFDVBD632BHDS5@ .
    id :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInvalidation' value with any optional fields omitted.
mkGetInvalidation ::
  -- | 'distributionId'
  Types.String ->
  -- | 'id'
  Types.String ->
  GetInvalidation
mkGetInvalidation distributionId id =
  GetInvalidation' {distributionId, id}

-- | The distribution's ID.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giDistributionId :: Lens.Lens' GetInvalidation Types.String
giDistributionId = Lens.field @"distributionId"
{-# DEPRECATED giDistributionId "Use generic-lens or generic-optics with 'distributionId' instead." #-}

-- | The identifier for the invalidation request, for example, @IDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giId :: Lens.Lens' GetInvalidation Types.String
giId = Lens.field @"id"
{-# DEPRECATED giId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetInvalidation where
  type Rs GetInvalidation = GetInvalidationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/distribution/" Core.<> (Core.toText distributionId)
                Core.<> ("/invalidation/")
                Core.<> (Core.toText id)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetInvalidationResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetInvalidationResponse' smart constructor.
data GetInvalidationResponse = GetInvalidationResponse'
  { -- | The invalidation's information. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/InvalidationDatatype.html Invalidation Complex Type> .
    invalidation :: Core.Maybe Types.Invalidation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInvalidationResponse' value with any optional fields omitted.
mkGetInvalidationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInvalidationResponse
mkGetInvalidationResponse responseStatus =
  GetInvalidationResponse'
    { invalidation = Core.Nothing,
      responseStatus
    }

-- | The invalidation's information. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/InvalidationDatatype.html Invalidation Complex Type> .
--
-- /Note:/ Consider using 'invalidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsInvalidation :: Lens.Lens' GetInvalidationResponse (Core.Maybe Types.Invalidation)
girrsInvalidation = Lens.field @"invalidation"
{-# DEPRECATED girrsInvalidation "Use generic-lens or generic-optics with 'invalidation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResponseStatus :: Lens.Lens' GetInvalidationResponse Core.Int
girrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED girrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
