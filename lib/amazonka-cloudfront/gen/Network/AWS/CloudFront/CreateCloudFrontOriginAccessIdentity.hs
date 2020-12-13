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
    ccfoairsETag,
    ccfoairsLocation,
    ccfoairsCloudFrontOriginAccessIdentity,
    ccfoairsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to create a new origin access identity (OAI). An origin access identity is a special CloudFront user that you can associate with Amazon S3 origins, so that you can secure all or just some of your Amazon S3 content. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Restricting Access to Amazon S3 Content by Using an Origin Access Identity> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkCreateCloudFrontOriginAccessIdentity' smart constructor.
newtype CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity'
  { -- | The current configuration information for the identity.
    cloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- * 'cloudFrontOriginAccessIdentityConfig' - The current configuration information for the identity.
mkCreateCloudFrontOriginAccessIdentity ::
  -- | 'cloudFrontOriginAccessIdentityConfig'
  CloudFrontOriginAccessIdentityConfig ->
  CreateCloudFrontOriginAccessIdentity
mkCreateCloudFrontOriginAccessIdentity
  pCloudFrontOriginAccessIdentityConfig_ =
    CreateCloudFrontOriginAccessIdentity'
      { cloudFrontOriginAccessIdentityConfig =
          pCloudFrontOriginAccessIdentityConfig_
      }

-- | The current configuration information for the identity.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfoaiCloudFrontOriginAccessIdentityConfig :: Lens.Lens' CreateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
ccfoaiCloudFrontOriginAccessIdentityConfig = Lens.lens (cloudFrontOriginAccessIdentityConfig :: CreateCloudFrontOriginAccessIdentity -> CloudFrontOriginAccessIdentityConfig) (\s a -> s {cloudFrontOriginAccessIdentityConfig = a} :: CreateCloudFrontOriginAccessIdentity)
{-# DEPRECATED ccfoaiCloudFrontOriginAccessIdentityConfig "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityConfig' instead." #-}

instance Lude.AWSRequest CreateCloudFrontOriginAccessIdentity where
  type
    Rs CreateCloudFrontOriginAccessIdentity =
      CreateCloudFrontOriginAccessIdentityResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateCloudFrontOriginAccessIdentityResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateCloudFrontOriginAccessIdentity where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CloudFrontOriginAccessIdentityConfig"
      Lude.. cloudFrontOriginAccessIdentityConfig

instance Lude.ToHeaders CreateCloudFrontOriginAccessIdentity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCloudFrontOriginAccessIdentity where
  toPath = Lude.const "/2020-05-31/origin-access-identity/cloudfront"

instance Lude.ToQuery CreateCloudFrontOriginAccessIdentity where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateCloudFrontOriginAccessIdentityResponse' smart constructor.
data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse'
  { -- | The current version of the origin access identity created.
    eTag :: Lude.Maybe Lude.Text,
    -- | The fully qualified URI of the new origin access identity just created.
    location :: Lude.Maybe Lude.Text,
    -- | The origin access identity's information.
    cloudFrontOriginAccessIdentity :: Lude.Maybe CloudFrontOriginAccessIdentity,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCloudFrontOriginAccessIdentityResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the origin access identity created.
-- * 'location' - The fully qualified URI of the new origin access identity just created.
-- * 'cloudFrontOriginAccessIdentity' - The origin access identity's information.
-- * 'responseStatus' - The response status code.
mkCreateCloudFrontOriginAccessIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCloudFrontOriginAccessIdentityResponse
mkCreateCloudFrontOriginAccessIdentityResponse pResponseStatus_ =
  CreateCloudFrontOriginAccessIdentityResponse'
    { eTag =
        Lude.Nothing,
      location = Lude.Nothing,
      cloudFrontOriginAccessIdentity = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the origin access identity created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfoairsETag :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse (Lude.Maybe Lude.Text)
ccfoairsETag = Lens.lens (eTag :: CreateCloudFrontOriginAccessIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreateCloudFrontOriginAccessIdentityResponse)
{-# DEPRECATED ccfoairsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the new origin access identity just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfoairsLocation :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse (Lude.Maybe Lude.Text)
ccfoairsLocation = Lens.lens (location :: CreateCloudFrontOriginAccessIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateCloudFrontOriginAccessIdentityResponse)
{-# DEPRECATED ccfoairsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The origin access identity's information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfoairsCloudFrontOriginAccessIdentity :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse (Lude.Maybe CloudFrontOriginAccessIdentity)
ccfoairsCloudFrontOriginAccessIdentity = Lens.lens (cloudFrontOriginAccessIdentity :: CreateCloudFrontOriginAccessIdentityResponse -> Lude.Maybe CloudFrontOriginAccessIdentity) (\s a -> s {cloudFrontOriginAccessIdentity = a} :: CreateCloudFrontOriginAccessIdentityResponse)
{-# DEPRECATED ccfoairsCloudFrontOriginAccessIdentity "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentity' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfoairsResponseStatus :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse Lude.Int
ccfoairsResponseStatus = Lens.lens (responseStatus :: CreateCloudFrontOriginAccessIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCloudFrontOriginAccessIdentityResponse)
{-# DEPRECATED ccfoairsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
