{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdatePublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update public key information. Note that the only value you can change is the comment.
module Network.AWS.CloudFront.UpdatePublicKey
  ( -- * Creating a request
    UpdatePublicKey (..),
    mkUpdatePublicKey,

    -- ** Request lenses
    upkIfMatch,
    upkPublicKeyConfig,
    upkId,

    -- * Destructuring the response
    UpdatePublicKeyResponse (..),
    mkUpdatePublicKeyResponse,

    -- ** Response lenses
    upkrsETag,
    upkrsPublicKey,
    upkrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePublicKey' smart constructor.
data UpdatePublicKey = UpdatePublicKey'
  { ifMatch ::
      Lude.Maybe Lude.Text,
    publicKeyConfig :: PublicKeyConfig,
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

-- | Creates a value of 'UpdatePublicKey' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the public key that you are updating.
-- * 'ifMatch' - The value of the @ETag@ header that you received when retrieving the public key to update. For example: @E2QWRUHAPOMQZL@ .
-- * 'publicKeyConfig' - A public key configuration.
mkUpdatePublicKey ::
  -- | 'publicKeyConfig'
  PublicKeyConfig ->
  -- | 'id'
  Lude.Text ->
  UpdatePublicKey
mkUpdatePublicKey pPublicKeyConfig_ pId_ =
  UpdatePublicKey'
    { ifMatch = Lude.Nothing,
      publicKeyConfig = pPublicKeyConfig_,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the public key to update. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkIfMatch :: Lens.Lens' UpdatePublicKey (Lude.Maybe Lude.Text)
upkIfMatch = Lens.lens (ifMatch :: UpdatePublicKey -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: UpdatePublicKey)
{-# DEPRECATED upkIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | A public key configuration.
--
-- /Note:/ Consider using 'publicKeyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkPublicKeyConfig :: Lens.Lens' UpdatePublicKey PublicKeyConfig
upkPublicKeyConfig = Lens.lens (publicKeyConfig :: UpdatePublicKey -> PublicKeyConfig) (\s a -> s {publicKeyConfig = a} :: UpdatePublicKey)
{-# DEPRECATED upkPublicKeyConfig "Use generic-lens or generic-optics with 'publicKeyConfig' instead." #-}

-- | The identifier of the public key that you are updating.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkId :: Lens.Lens' UpdatePublicKey Lude.Text
upkId = Lens.lens (id :: UpdatePublicKey -> Lude.Text) (\s a -> s {id = a} :: UpdatePublicKey)
{-# DEPRECATED upkId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdatePublicKey where
  type Rs UpdatePublicKey = UpdatePublicKeyResponse
  request = Req.putXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          UpdatePublicKeyResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdatePublicKey where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}PublicKeyConfig"
      Lude.. publicKeyConfig

instance Lude.ToHeaders UpdatePublicKey where
  toHeaders UpdatePublicKey' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath UpdatePublicKey where
  toPath UpdatePublicKey' {..} =
    Lude.mconcat ["/2020-05-31/public-key/", Lude.toBS id, "/config"]

instance Lude.ToQuery UpdatePublicKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePublicKeyResponse' smart constructor.
data UpdatePublicKeyResponse = UpdatePublicKeyResponse'
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

-- | Creates a value of 'UpdatePublicKeyResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The identifier of the current version of the public key.
-- * 'publicKey' - The public key.
-- * 'responseStatus' - The response status code.
mkUpdatePublicKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePublicKeyResponse
mkUpdatePublicKeyResponse pResponseStatus_ =
  UpdatePublicKeyResponse'
    { eTag = Lude.Nothing,
      publicKey = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the current version of the public key.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkrsETag :: Lens.Lens' UpdatePublicKeyResponse (Lude.Maybe Lude.Text)
upkrsETag = Lens.lens (eTag :: UpdatePublicKeyResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: UpdatePublicKeyResponse)
{-# DEPRECATED upkrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The public key.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkrsPublicKey :: Lens.Lens' UpdatePublicKeyResponse (Lude.Maybe PublicKey)
upkrsPublicKey = Lens.lens (publicKey :: UpdatePublicKeyResponse -> Lude.Maybe PublicKey) (\s a -> s {publicKey = a} :: UpdatePublicKeyResponse)
{-# DEPRECATED upkrsPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upkrsResponseStatus :: Lens.Lens' UpdatePublicKeyResponse Lude.Int
upkrsResponseStatus = Lens.lens (responseStatus :: UpdatePublicKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePublicKeyResponse)
{-# DEPRECATED upkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
