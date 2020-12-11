{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an origin access identity.
module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
  ( -- * Creating a request
    UpdateCloudFrontOriginAccessIdentity (..),
    mkUpdateCloudFrontOriginAccessIdentity,

    -- ** Request lenses
    ucfoaiIfMatch,
    ucfoaiCloudFrontOriginAccessIdentityConfig,
    ucfoaiId,

    -- * Destructuring the response
    UpdateCloudFrontOriginAccessIdentityResponse (..),
    mkUpdateCloudFrontOriginAccessIdentityResponse,

    -- ** Response lenses
    ucfoairsETag,
    ucfoairsCloudFrontOriginAccessIdentity,
    ucfoairsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to update an origin access identity.
--
-- /See:/ 'mkUpdateCloudFrontOriginAccessIdentity' smart constructor.
data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity'
  { ifMatch ::
      Lude.Maybe
        Lude.Text,
    cloudFrontOriginAccessIdentityConfig ::
      CloudFrontOriginAccessIdentityConfig,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- * 'cloudFrontOriginAccessIdentityConfig' - The identity's configuration information.
-- * 'id' - The identity's id.
-- * 'ifMatch' - The value of the @ETag@ header that you received when retrieving the identity's configuration. For example: @E2QWRUHAPOMQZL@ .
mkUpdateCloudFrontOriginAccessIdentity ::
  -- | 'cloudFrontOriginAccessIdentityConfig'
  CloudFrontOriginAccessIdentityConfig ->
  -- | 'id'
  Lude.Text ->
  UpdateCloudFrontOriginAccessIdentity
mkUpdateCloudFrontOriginAccessIdentity
  pCloudFrontOriginAccessIdentityConfig_
  pId_ =
    UpdateCloudFrontOriginAccessIdentity'
      { ifMatch = Lude.Nothing,
        cloudFrontOriginAccessIdentityConfig =
          pCloudFrontOriginAccessIdentityConfig_,
        id = pId_
      }

-- | The value of the @ETag@ header that you received when retrieving the identity's configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoaiIfMatch :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity (Lude.Maybe Lude.Text)
ucfoaiIfMatch = Lens.lens (ifMatch :: UpdateCloudFrontOriginAccessIdentity -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: UpdateCloudFrontOriginAccessIdentity)
{-# DEPRECATED ucfoaiIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The identity's configuration information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoaiCloudFrontOriginAccessIdentityConfig :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
ucfoaiCloudFrontOriginAccessIdentityConfig = Lens.lens (cloudFrontOriginAccessIdentityConfig :: UpdateCloudFrontOriginAccessIdentity -> CloudFrontOriginAccessIdentityConfig) (\s a -> s {cloudFrontOriginAccessIdentityConfig = a} :: UpdateCloudFrontOriginAccessIdentity)
{-# DEPRECATED ucfoaiCloudFrontOriginAccessIdentityConfig "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityConfig' instead." #-}

-- | The identity's id.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoaiId :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity Lude.Text
ucfoaiId = Lens.lens (id :: UpdateCloudFrontOriginAccessIdentity -> Lude.Text) (\s a -> s {id = a} :: UpdateCloudFrontOriginAccessIdentity)
{-# DEPRECATED ucfoaiId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateCloudFrontOriginAccessIdentity where
  type
    Rs UpdateCloudFrontOriginAccessIdentity =
      UpdateCloudFrontOriginAccessIdentityResponse
  request = Req.putXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateCloudFrontOriginAccessIdentityResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateCloudFrontOriginAccessIdentity where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CloudFrontOriginAccessIdentityConfig"
      Lude.. cloudFrontOriginAccessIdentityConfig

instance Lude.ToHeaders UpdateCloudFrontOriginAccessIdentity where
  toHeaders UpdateCloudFrontOriginAccessIdentity' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath UpdateCloudFrontOriginAccessIdentity where
  toPath UpdateCloudFrontOriginAccessIdentity' {..} =
    Lude.mconcat
      [ "/2020-05-31/origin-access-identity/cloudfront/",
        Lude.toBS id,
        "/config"
      ]

instance Lude.ToQuery UpdateCloudFrontOriginAccessIdentity where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkUpdateCloudFrontOriginAccessIdentityResponse' smart constructor.
data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse'
  { eTag ::
      Lude.Maybe
        Lude.Text,
    cloudFrontOriginAccessIdentity ::
      Lude.Maybe
        CloudFrontOriginAccessIdentity,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCloudFrontOriginAccessIdentityResponse' with the minimum fields required to make a request.
--
-- * 'cloudFrontOriginAccessIdentity' - The origin access identity's information.
-- * 'eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'responseStatus' - The response status code.
mkUpdateCloudFrontOriginAccessIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCloudFrontOriginAccessIdentityResponse
mkUpdateCloudFrontOriginAccessIdentityResponse pResponseStatus_ =
  UpdateCloudFrontOriginAccessIdentityResponse'
    { eTag =
        Lude.Nothing,
      cloudFrontOriginAccessIdentity = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoairsETag :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse (Lude.Maybe Lude.Text)
ucfoairsETag = Lens.lens (eTag :: UpdateCloudFrontOriginAccessIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: UpdateCloudFrontOriginAccessIdentityResponse)
{-# DEPRECATED ucfoairsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The origin access identity's information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoairsCloudFrontOriginAccessIdentity :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse (Lude.Maybe CloudFrontOriginAccessIdentity)
ucfoairsCloudFrontOriginAccessIdentity = Lens.lens (cloudFrontOriginAccessIdentity :: UpdateCloudFrontOriginAccessIdentityResponse -> Lude.Maybe CloudFrontOriginAccessIdentity) (\s a -> s {cloudFrontOriginAccessIdentity = a} :: UpdateCloudFrontOriginAccessIdentityResponse)
{-# DEPRECATED ucfoairsCloudFrontOriginAccessIdentity "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentity' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfoairsResponseStatus :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse Lude.Int
ucfoairsResponseStatus = Lens.lens (responseStatus :: UpdateCloudFrontOriginAccessIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCloudFrontOriginAccessIdentityResponse)
{-# DEPRECATED ucfoairsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
