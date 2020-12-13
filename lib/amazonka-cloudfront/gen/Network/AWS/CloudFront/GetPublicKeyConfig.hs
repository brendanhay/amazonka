{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetPublicKeyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a public key configuration.
module Network.AWS.CloudFront.GetPublicKeyConfig
  ( -- * Creating a request
    GetPublicKeyConfig (..),
    mkGetPublicKeyConfig,

    -- ** Request lenses
    gpkcId,

    -- * Destructuring the response
    GetPublicKeyConfigResponse (..),
    mkGetPublicKeyConfigResponse,

    -- ** Response lenses
    gpkcrsETag,
    gpkcrsPublicKeyConfig,
    gpkcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPublicKeyConfig' smart constructor.
newtype GetPublicKeyConfig = GetPublicKeyConfig'
  { -- | The identifier of the public key whose configuration you are getting.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPublicKeyConfig' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the public key whose configuration you are getting.
mkGetPublicKeyConfig ::
  -- | 'id'
  Lude.Text ->
  GetPublicKeyConfig
mkGetPublicKeyConfig pId_ = GetPublicKeyConfig' {id = pId_}

-- | The identifier of the public key whose configuration you are getting.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcId :: Lens.Lens' GetPublicKeyConfig Lude.Text
gpkcId = Lens.lens (id :: GetPublicKeyConfig -> Lude.Text) (\s a -> s {id = a} :: GetPublicKeyConfig)
{-# DEPRECATED gpkcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetPublicKeyConfig where
  type Rs GetPublicKeyConfig = GetPublicKeyConfigResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetPublicKeyConfigResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPublicKeyConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPublicKeyConfig where
  toPath GetPublicKeyConfig' {..} =
    Lude.mconcat ["/2020-05-31/public-key/", Lude.toBS id, "/config"]

instance Lude.ToQuery GetPublicKeyConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPublicKeyConfigResponse' smart constructor.
data GetPublicKeyConfigResponse = GetPublicKeyConfigResponse'
  { -- | The identifier for this version of the public key configuration.
    eTag :: Lude.Maybe Lude.Text,
    -- | A public key configuration.
    publicKeyConfig :: Lude.Maybe PublicKeyConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPublicKeyConfigResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The identifier for this version of the public key configuration.
-- * 'publicKeyConfig' - A public key configuration.
-- * 'responseStatus' - The response status code.
mkGetPublicKeyConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPublicKeyConfigResponse
mkGetPublicKeyConfigResponse pResponseStatus_ =
  GetPublicKeyConfigResponse'
    { eTag = Lude.Nothing,
      publicKeyConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for this version of the public key configuration.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcrsETag :: Lens.Lens' GetPublicKeyConfigResponse (Lude.Maybe Lude.Text)
gpkcrsETag = Lens.lens (eTag :: GetPublicKeyConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetPublicKeyConfigResponse)
{-# DEPRECATED gpkcrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | A public key configuration.
--
-- /Note:/ Consider using 'publicKeyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcrsPublicKeyConfig :: Lens.Lens' GetPublicKeyConfigResponse (Lude.Maybe PublicKeyConfig)
gpkcrsPublicKeyConfig = Lens.lens (publicKeyConfig :: GetPublicKeyConfigResponse -> Lude.Maybe PublicKeyConfig) (\s a -> s {publicKeyConfig = a} :: GetPublicKeyConfigResponse)
{-# DEPRECATED gpkcrsPublicKeyConfig "Use generic-lens or generic-optics with 'publicKeyConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcrsResponseStatus :: Lens.Lens' GetPublicKeyConfigResponse Lude.Int
gpkcrsResponseStatus = Lens.lens (responseStatus :: GetPublicKeyConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPublicKeyConfigResponse)
{-# DEPRECATED gpkcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
