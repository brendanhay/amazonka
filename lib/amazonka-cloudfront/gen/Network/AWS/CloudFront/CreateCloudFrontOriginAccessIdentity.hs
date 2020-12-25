{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new origin access identity. If you're using Amazon S3 for your origin, you can use an origin access identity to require users to access your content using a CloudFront URL instead of the Amazon S3 URL. For more information about how to use origin access identities, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
  ( -- * Creating a request
    CreateCloudFrontOriginAccessIdentity (..),
    mkCreateCloudFrontOriginAccessIdentity,

    -- ** Request lenses
    ccfoaiCloudFrontOriginAccessIdentityConfig,

    -- * Destructuring the response
    CreateCloudFrontOriginAccessIdentityResponse (..),
    mkCreateCloudFrontOriginAccessIdentityResponse,

    -- ** Response lenses
    ccfoairrsCloudFrontOriginAccessIdentity,
    ccfoairrsETag,
    ccfoairrsLocation,
    ccfoairrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create a new origin access identity (OAI). An origin access identity is a special CloudFront user that you can associate with Amazon S3 origins, so that you can secure all or just some of your Amazon S3 content. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Restricting Access to Amazon S3 Content by Using an Origin Access Identity> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkCreateCloudFrontOriginAccessIdentity' smart constructor.
newtype CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity'
  { -- | The current configuration information for the identity.
    cloudFrontOriginAccessIdentityConfig :: Types.CloudFrontOriginAccessIdentityConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCloudFrontOriginAccessIdentity' value with any optional fields omitted.
mkCreateCloudFrontOriginAccessIdentity ::
  -- | 'cloudFrontOriginAccessIdentityConfig'
  Types.CloudFrontOriginAccessIdentityConfig ->
  CreateCloudFrontOriginAccessIdentity
mkCreateCloudFrontOriginAccessIdentity
  cloudFrontOriginAccessIdentityConfig =
    CreateCloudFrontOriginAccessIdentity' {cloudFrontOriginAccessIdentityConfig}

-- | The current configuration information for the identity.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfoaiCloudFrontOriginAccessIdentityConfig :: Lens.Lens' CreateCloudFrontOriginAccessIdentity Types.CloudFrontOriginAccessIdentityConfig
ccfoaiCloudFrontOriginAccessIdentityConfig = Lens.field @"cloudFrontOriginAccessIdentityConfig"
{-# DEPRECATED ccfoaiCloudFrontOriginAccessIdentityConfig "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityConfig' instead." #-}

instance Core.AWSRequest CreateCloudFrontOriginAccessIdentity where
  type
    Rs CreateCloudFrontOriginAccessIdentity =
      CreateCloudFrontOriginAccessIdentityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/2020-05-31/origin-access-identity/cloudfront",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCloudFrontOriginAccessIdentityResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateCloudFrontOriginAccessIdentityResponse' smart constructor.
data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse'
  { -- | The origin access identity's information.
    cloudFrontOriginAccessIdentity :: Core.Maybe Types.CloudFrontOriginAccessIdentity,
    -- | The current version of the origin access identity created.
    eTag :: Core.Maybe Types.String,
    -- | The fully qualified URI of the new origin access identity just created.
    location :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCloudFrontOriginAccessIdentityResponse' value with any optional fields omitted.
mkCreateCloudFrontOriginAccessIdentityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCloudFrontOriginAccessIdentityResponse
mkCreateCloudFrontOriginAccessIdentityResponse responseStatus =
  CreateCloudFrontOriginAccessIdentityResponse'
    { cloudFrontOriginAccessIdentity =
        Core.Nothing,
      eTag = Core.Nothing,
      location = Core.Nothing,
      responseStatus
    }

-- | The origin access identity's information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfoairrsCloudFrontOriginAccessIdentity :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse (Core.Maybe Types.CloudFrontOriginAccessIdentity)
ccfoairrsCloudFrontOriginAccessIdentity = Lens.field @"cloudFrontOriginAccessIdentity"
{-# DEPRECATED ccfoairrsCloudFrontOriginAccessIdentity "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentity' instead." #-}

-- | The current version of the origin access identity created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfoairrsETag :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse (Core.Maybe Types.String)
ccfoairrsETag = Lens.field @"eTag"
{-# DEPRECATED ccfoairrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the new origin access identity just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfoairrsLocation :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse (Core.Maybe Types.String)
ccfoairrsLocation = Lens.field @"location"
{-# DEPRECATED ccfoairrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfoairrsResponseStatus :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse Core.Int
ccfoairrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccfoairrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
