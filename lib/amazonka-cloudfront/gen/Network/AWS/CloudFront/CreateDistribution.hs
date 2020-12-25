{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new web distribution. You create a CloudFront distribution to tell CloudFront where you want content to be delivered from, and the details about how to track and manage content delivery. Send a @POST@ request to the @//CloudFront API version/ /distribution@ /@distribution ID@ resource.
--
-- /Important:/ When you update a distribution, there are more required fields than when you create a distribution. When you update your distribution by using <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_UpdateDistribution.html UpdateDistribution> , follow the steps included in the documentation to get the current configuration and then make your updates. This helps to make sure that you include all of the required fields. To view a summary, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-overview-required-fields.html Required Fields for Create Distribution and Update Distribution> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateDistribution
  ( -- * Creating a request
    CreateDistribution (..),
    mkCreateDistribution,

    -- ** Request lenses
    cdDistributionConfig,

    -- * Destructuring the response
    CreateDistributionResponse (..),
    mkCreateDistributionResponse,

    -- ** Response lenses
    cdrrsDistribution,
    cdrrsETag,
    cdrrsLocation,
    cdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create a new distribution.
--
-- /See:/ 'mkCreateDistribution' smart constructor.
newtype CreateDistribution = CreateDistribution'
  { -- | The distribution's configuration information.
    distributionConfig :: Types.DistributionConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDistribution' value with any optional fields omitted.
mkCreateDistribution ::
  -- | 'distributionConfig'
  Types.DistributionConfig ->
  CreateDistribution
mkCreateDistribution distributionConfig =
  CreateDistribution' {distributionConfig}

-- | The distribution's configuration information.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDistributionConfig :: Lens.Lens' CreateDistribution Types.DistributionConfig
cdDistributionConfig = Lens.field @"distributionConfig"
{-# DEPRECATED cdDistributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead." #-}

instance Core.AWSRequest CreateDistribution where
  type Rs CreateDistribution = CreateDistributionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2020-05-31/distribution",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateDistributionResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateDistributionResponse' smart constructor.
data CreateDistributionResponse = CreateDistributionResponse'
  { -- | The distribution's information.
    distribution :: Core.Maybe Types.Distribution,
    -- | The current version of the distribution created.
    eTag :: Core.Maybe Types.String,
    -- | The fully qualified URI of the new distribution resource just created.
    location :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateDistributionResponse' value with any optional fields omitted.
mkCreateDistributionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDistributionResponse
mkCreateDistributionResponse responseStatus =
  CreateDistributionResponse'
    { distribution = Core.Nothing,
      eTag = Core.Nothing,
      location = Core.Nothing,
      responseStatus
    }

-- | The distribution's information.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDistribution :: Lens.Lens' CreateDistributionResponse (Core.Maybe Types.Distribution)
cdrrsDistribution = Lens.field @"distribution"
{-# DEPRECATED cdrrsDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The current version of the distribution created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsETag :: Lens.Lens' CreateDistributionResponse (Core.Maybe Types.String)
cdrrsETag = Lens.field @"eTag"
{-# DEPRECATED cdrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the new distribution resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsLocation :: Lens.Lens' CreateDistributionResponse (Core.Maybe Types.String)
cdrrsLocation = Lens.field @"location"
{-# DEPRECATED cdrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDistributionResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
