{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about an origin access identity.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
  ( -- * Creating a request
    GetCloudFrontOriginAccessIdentityConfig (..),
    mkGetCloudFrontOriginAccessIdentityConfig,

    -- ** Request lenses
    gcfoaicId,

    -- * Destructuring the response
    GetCloudFrontOriginAccessIdentityConfigResponse (..),
    mkGetCloudFrontOriginAccessIdentityConfigResponse,

    -- ** Response lenses
    gcfoaicrsCloudFrontOriginAccessIdentityConfig,
    gcfoaicrsETag,
    gcfoaicrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The origin access identity's configuration information. For more information, see <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_CloudFrontOriginAccessIdentityConfig.html CloudFrontOriginAccessIdentityConfig> .
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentityConfig' smart constructor.
newtype GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig'
  { -- | The identity's ID.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCloudFrontOriginAccessIdentityConfig' with the minimum fields required to make a request.
--
-- * 'id' - The identity's ID.
mkGetCloudFrontOriginAccessIdentityConfig ::
  -- | 'id'
  Lude.Text ->
  GetCloudFrontOriginAccessIdentityConfig
mkGetCloudFrontOriginAccessIdentityConfig pId_ =
  GetCloudFrontOriginAccessIdentityConfig' {id = pId_}

-- | The identity's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicId :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfig Lude.Text
gcfoaicId = Lens.lens (id :: GetCloudFrontOriginAccessIdentityConfig -> Lude.Text) (\s a -> s {id = a} :: GetCloudFrontOriginAccessIdentityConfig)
{-# DEPRECATED gcfoaicId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetCloudFrontOriginAccessIdentityConfig where
  type
    Rs GetCloudFrontOriginAccessIdentityConfig =
      GetCloudFrontOriginAccessIdentityConfigResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetCloudFrontOriginAccessIdentityConfigResponse'
            Lude.<$> (Lude.parseXML x)
            Lude.<*> (h Lude..#? "ETag")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCloudFrontOriginAccessIdentityConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCloudFrontOriginAccessIdentityConfig where
  toPath GetCloudFrontOriginAccessIdentityConfig' {..} =
    Lude.mconcat
      [ "/2020-05-31/origin-access-identity/cloudfront/",
        Lude.toBS id,
        "/config"
      ]

instance Lude.ToQuery GetCloudFrontOriginAccessIdentityConfig where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentityConfigResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse'
  { -- | The origin access identity's configuration information.
    cloudFrontOriginAccessIdentityConfig :: Lude.Maybe CloudFrontOriginAccessIdentityConfig,
    -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCloudFrontOriginAccessIdentityConfigResponse' with the minimum fields required to make a request.
--
-- * 'cloudFrontOriginAccessIdentityConfig' - The origin access identity's configuration information.
-- * 'eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'responseStatus' - The response status code.
mkGetCloudFrontOriginAccessIdentityConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCloudFrontOriginAccessIdentityConfigResponse
mkGetCloudFrontOriginAccessIdentityConfigResponse pResponseStatus_ =
  GetCloudFrontOriginAccessIdentityConfigResponse'
    { cloudFrontOriginAccessIdentityConfig =
        Lude.Nothing,
      eTag = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The origin access identity's configuration information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicrsCloudFrontOriginAccessIdentityConfig :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Lude.Maybe CloudFrontOriginAccessIdentityConfig)
gcfoaicrsCloudFrontOriginAccessIdentityConfig = Lens.lens (cloudFrontOriginAccessIdentityConfig :: GetCloudFrontOriginAccessIdentityConfigResponse -> Lude.Maybe CloudFrontOriginAccessIdentityConfig) (\s a -> s {cloudFrontOriginAccessIdentityConfig = a} :: GetCloudFrontOriginAccessIdentityConfigResponse)
{-# DEPRECATED gcfoaicrsCloudFrontOriginAccessIdentityConfig "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityConfig' instead." #-}

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicrsETag :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Lude.Maybe Lude.Text)
gcfoaicrsETag = Lens.lens (eTag :: GetCloudFrontOriginAccessIdentityConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetCloudFrontOriginAccessIdentityConfigResponse)
{-# DEPRECATED gcfoaicrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaicrsResponseStatus :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse Lude.Int
gcfoaicrsResponseStatus = Lens.lens (responseStatus :: GetCloudFrontOriginAccessIdentityConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCloudFrontOriginAccessIdentityConfigResponse)
{-# DEPRECATED gcfoaicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
