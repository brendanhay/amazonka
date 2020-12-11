{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a public key.
module Network.AWS.CloudFront.GetPublicKey
  ( -- * Creating a request
    GetPublicKey (..),
    mkGetPublicKey,

    -- ** Request lenses
    gpkId,

    -- * Destructuring the response
    GetPublicKeyResponse (..),
    mkGetPublicKeyResponse,

    -- ** Response lenses
    gpkrsETag,
    gpkrsPublicKey,
    gpkrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPublicKey' smart constructor.
newtype GetPublicKey = GetPublicKey' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPublicKey' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the public key you are getting.
mkGetPublicKey ::
  -- | 'id'
  Lude.Text ->
  GetPublicKey
mkGetPublicKey pId_ = GetPublicKey' {id = pId_}

-- | The identifier of the public key you are getting.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkId :: Lens.Lens' GetPublicKey Lude.Text
gpkId = Lens.lens (id :: GetPublicKey -> Lude.Text) (\s a -> s {id = a} :: GetPublicKey)
{-# DEPRECATED gpkId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetPublicKey where
  type Rs GetPublicKey = GetPublicKeyResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetPublicKeyResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPublicKey where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPublicKey where
  toPath GetPublicKey' {..} =
    Lude.mconcat ["/2020-05-31/public-key/", Lude.toBS id]

instance Lude.ToQuery GetPublicKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPublicKeyResponse' smart constructor.
data GetPublicKeyResponse = GetPublicKeyResponse'
  { eTag ::
      Lude.Maybe Lude.Text,
    publicKey :: Lude.Maybe PublicKey,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPublicKeyResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The identifier for this version of the public key.
-- * 'publicKey' - The public key.
-- * 'responseStatus' - The response status code.
mkGetPublicKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPublicKeyResponse
mkGetPublicKeyResponse pResponseStatus_ =
  GetPublicKeyResponse'
    { eTag = Lude.Nothing,
      publicKey = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for this version of the public key.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrsETag :: Lens.Lens' GetPublicKeyResponse (Lude.Maybe Lude.Text)
gpkrsETag = Lens.lens (eTag :: GetPublicKeyResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetPublicKeyResponse)
{-# DEPRECATED gpkrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The public key.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrsPublicKey :: Lens.Lens' GetPublicKeyResponse (Lude.Maybe PublicKey)
gpkrsPublicKey = Lens.lens (publicKey :: GetPublicKeyResponse -> Lude.Maybe PublicKey) (\s a -> s {publicKey = a} :: GetPublicKeyResponse)
{-# DEPRECATED gpkrsPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrsResponseStatus :: Lens.Lens' GetPublicKeyResponse Lude.Int
gpkrsResponseStatus = Lens.lens (responseStatus :: GetPublicKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPublicKeyResponse)
{-# DEPRECATED gpkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
