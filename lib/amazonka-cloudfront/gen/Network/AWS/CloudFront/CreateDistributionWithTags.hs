{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateDistributionWithTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new distribution with tags.
module Network.AWS.CloudFront.CreateDistributionWithTags
  ( -- * Creating a request
    CreateDistributionWithTags (..),
    mkCreateDistributionWithTags,

    -- ** Request lenses
    cdwtDistributionConfigWithTags,

    -- * Destructuring the response
    CreateDistributionWithTagsResponse (..),
    mkCreateDistributionWithTagsResponse,

    -- ** Response lenses
    cdwtrrsDistribution,
    cdwtrrsETag,
    cdwtrrsLocation,
    cdwtrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create a new distribution with tags.
--
-- /See:/ 'mkCreateDistributionWithTags' smart constructor.
newtype CreateDistributionWithTags = CreateDistributionWithTags'
  { -- | The distribution's configuration information.
    distributionConfigWithTags :: Types.DistributionConfigWithTags
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDistributionWithTags' value with any optional fields omitted.
mkCreateDistributionWithTags ::
  -- | 'distributionConfigWithTags'
  Types.DistributionConfigWithTags ->
  CreateDistributionWithTags
mkCreateDistributionWithTags distributionConfigWithTags =
  CreateDistributionWithTags' {distributionConfigWithTags}

-- | The distribution's configuration information.
--
-- /Note:/ Consider using 'distributionConfigWithTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtDistributionConfigWithTags :: Lens.Lens' CreateDistributionWithTags Types.DistributionConfigWithTags
cdwtDistributionConfigWithTags = Lens.field @"distributionConfigWithTags"
{-# DEPRECATED cdwtDistributionConfigWithTags "Use generic-lens or generic-optics with 'distributionConfigWithTags' instead." #-}

instance Core.AWSRequest CreateDistributionWithTags where
  type
    Rs CreateDistributionWithTags =
      CreateDistributionWithTagsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2020-05-31/distribution",
        Core._rqQuery = Core.pure ("WithTags", ""),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateDistributionWithTagsResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateDistributionWithTagsResponse' smart constructor.
data CreateDistributionWithTagsResponse = CreateDistributionWithTagsResponse'
  { -- | The distribution's information.
    distribution :: Core.Maybe Types.Distribution,
    -- | The current version of the distribution created.
    eTag :: Core.Maybe Types.ETag,
    -- | The fully qualified URI of the new distribution resource just created.
    location :: Core.Maybe Types.Location,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateDistributionWithTagsResponse' value with any optional fields omitted.
mkCreateDistributionWithTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDistributionWithTagsResponse
mkCreateDistributionWithTagsResponse responseStatus =
  CreateDistributionWithTagsResponse'
    { distribution = Core.Nothing,
      eTag = Core.Nothing,
      location = Core.Nothing,
      responseStatus
    }

-- | The distribution's information.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrrsDistribution :: Lens.Lens' CreateDistributionWithTagsResponse (Core.Maybe Types.Distribution)
cdwtrrsDistribution = Lens.field @"distribution"
{-# DEPRECATED cdwtrrsDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The current version of the distribution created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrrsETag :: Lens.Lens' CreateDistributionWithTagsResponse (Core.Maybe Types.ETag)
cdwtrrsETag = Lens.field @"eTag"
{-# DEPRECATED cdwtrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the new distribution resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrrsLocation :: Lens.Lens' CreateDistributionWithTagsResponse (Core.Maybe Types.Location)
cdwtrrsLocation = Lens.field @"location"
{-# DEPRECATED cdwtrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrrsResponseStatus :: Lens.Lens' CreateDistributionWithTagsResponse Core.Int
cdwtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdwtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
