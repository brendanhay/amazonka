{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an origin access identity.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
  ( -- * Creating a request
    GetCloudFrontOriginAccessIdentity (..),
    mkGetCloudFrontOriginAccessIdentity,

    -- ** Request lenses
    gcfoaiId,

    -- * Destructuring the response
    GetCloudFrontOriginAccessIdentityResponse (..),
    mkGetCloudFrontOriginAccessIdentityResponse,

    -- ** Response lenses
    gcfoairsETag,
    gcfoairsCloudFrontOriginAccessIdentity,
    gcfoairsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to get an origin access identity's information.
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentity' smart constructor.
newtype GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity'
  { id ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- * 'id' - The identity's ID.
mkGetCloudFrontOriginAccessIdentity ::
  -- | 'id'
  Lude.Text ->
  GetCloudFrontOriginAccessIdentity
mkGetCloudFrontOriginAccessIdentity pId_ =
  GetCloudFrontOriginAccessIdentity' {id = pId_}

-- | The identity's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoaiId :: Lens.Lens' GetCloudFrontOriginAccessIdentity Lude.Text
gcfoaiId = Lens.lens (id :: GetCloudFrontOriginAccessIdentity -> Lude.Text) (\s a -> s {id = a} :: GetCloudFrontOriginAccessIdentity)
{-# DEPRECATED gcfoaiId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetCloudFrontOriginAccessIdentity where
  type
    Rs GetCloudFrontOriginAccessIdentity =
      GetCloudFrontOriginAccessIdentityResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetCloudFrontOriginAccessIdentityResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCloudFrontOriginAccessIdentity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCloudFrontOriginAccessIdentity where
  toPath GetCloudFrontOriginAccessIdentity' {..} =
    Lude.mconcat
      ["/2020-05-31/origin-access-identity/cloudfront/", Lude.toBS id]

instance Lude.ToQuery GetCloudFrontOriginAccessIdentity where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetCloudFrontOriginAccessIdentityResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse'
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

-- | Creates a value of 'GetCloudFrontOriginAccessIdentityResponse' with the minimum fields required to make a request.
--
-- * 'cloudFrontOriginAccessIdentity' - The origin access identity's information.
-- * 'eTag' - The current version of the origin access identity's information. For example: @E2QWRUHAPOMQZL@ .
-- * 'responseStatus' - The response status code.
mkGetCloudFrontOriginAccessIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCloudFrontOriginAccessIdentityResponse
mkGetCloudFrontOriginAccessIdentityResponse pResponseStatus_ =
  GetCloudFrontOriginAccessIdentityResponse'
    { eTag = Lude.Nothing,
      cloudFrontOriginAccessIdentity = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the origin access identity's information. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoairsETag :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse (Lude.Maybe Lude.Text)
gcfoairsETag = Lens.lens (eTag :: GetCloudFrontOriginAccessIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetCloudFrontOriginAccessIdentityResponse)
{-# DEPRECATED gcfoairsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The origin access identity's information.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoairsCloudFrontOriginAccessIdentity :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse (Lude.Maybe CloudFrontOriginAccessIdentity)
gcfoairsCloudFrontOriginAccessIdentity = Lens.lens (cloudFrontOriginAccessIdentity :: GetCloudFrontOriginAccessIdentityResponse -> Lude.Maybe CloudFrontOriginAccessIdentity) (\s a -> s {cloudFrontOriginAccessIdentity = a} :: GetCloudFrontOriginAccessIdentityResponse)
{-# DEPRECATED gcfoairsCloudFrontOriginAccessIdentity "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentity' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfoairsResponseStatus :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse Lude.Int
gcfoairsResponseStatus = Lens.lens (responseStatus :: GetCloudFrontOriginAccessIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCloudFrontOriginAccessIdentityResponse)
{-# DEPRECATED gcfoairsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
