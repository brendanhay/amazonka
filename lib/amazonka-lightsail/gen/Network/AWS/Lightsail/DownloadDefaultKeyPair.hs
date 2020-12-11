{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DownloadDefaultKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the default SSH key pair from the user's account.
module Network.AWS.Lightsail.DownloadDefaultKeyPair
  ( -- * Creating a request
    DownloadDefaultKeyPair (..),
    mkDownloadDefaultKeyPair,

    -- * Destructuring the response
    DownloadDefaultKeyPairResponse (..),
    mkDownloadDefaultKeyPairResponse,

    -- ** Response lenses
    ddkprsPublicKeyBase64,
    ddkprsPrivateKeyBase64,
    ddkprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDownloadDefaultKeyPair' smart constructor.
data DownloadDefaultKeyPair = DownloadDefaultKeyPair'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DownloadDefaultKeyPair' with the minimum fields required to make a request.
mkDownloadDefaultKeyPair ::
  DownloadDefaultKeyPair
mkDownloadDefaultKeyPair = DownloadDefaultKeyPair'

instance Lude.AWSRequest DownloadDefaultKeyPair where
  type Rs DownloadDefaultKeyPair = DownloadDefaultKeyPairResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DownloadDefaultKeyPairResponse'
            Lude.<$> (x Lude..?> "publicKeyBase64")
            Lude.<*> (x Lude..?> "privateKeyBase64")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DownloadDefaultKeyPair where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DownloadDefaultKeyPair" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DownloadDefaultKeyPair where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DownloadDefaultKeyPair where
  toPath = Lude.const "/"

instance Lude.ToQuery DownloadDefaultKeyPair where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDownloadDefaultKeyPairResponse' smart constructor.
data DownloadDefaultKeyPairResponse = DownloadDefaultKeyPairResponse'
  { publicKeyBase64 ::
      Lude.Maybe Lude.Text,
    privateKeyBase64 ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DownloadDefaultKeyPairResponse' with the minimum fields required to make a request.
--
-- * 'privateKeyBase64' - A base64-encoded RSA private key.
-- * 'publicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
-- * 'responseStatus' - The response status code.
mkDownloadDefaultKeyPairResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DownloadDefaultKeyPairResponse
mkDownloadDefaultKeyPairResponse pResponseStatus_ =
  DownloadDefaultKeyPairResponse'
    { publicKeyBase64 = Lude.Nothing,
      privateKeyBase64 = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A base64-encoded public key of the @ssh-rsa@ type.
--
-- /Note:/ Consider using 'publicKeyBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddkprsPublicKeyBase64 :: Lens.Lens' DownloadDefaultKeyPairResponse (Lude.Maybe Lude.Text)
ddkprsPublicKeyBase64 = Lens.lens (publicKeyBase64 :: DownloadDefaultKeyPairResponse -> Lude.Maybe Lude.Text) (\s a -> s {publicKeyBase64 = a} :: DownloadDefaultKeyPairResponse)
{-# DEPRECATED ddkprsPublicKeyBase64 "Use generic-lens or generic-optics with 'publicKeyBase64' instead." #-}

-- | A base64-encoded RSA private key.
--
-- /Note:/ Consider using 'privateKeyBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddkprsPrivateKeyBase64 :: Lens.Lens' DownloadDefaultKeyPairResponse (Lude.Maybe Lude.Text)
ddkprsPrivateKeyBase64 = Lens.lens (privateKeyBase64 :: DownloadDefaultKeyPairResponse -> Lude.Maybe Lude.Text) (\s a -> s {privateKeyBase64 = a} :: DownloadDefaultKeyPairResponse)
{-# DEPRECATED ddkprsPrivateKeyBase64 "Use generic-lens or generic-optics with 'privateKeyBase64' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddkprsResponseStatus :: Lens.Lens' DownloadDefaultKeyPairResponse Lude.Int
ddkprsResponseStatus = Lens.lens (responseStatus :: DownloadDefaultKeyPairResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DownloadDefaultKeyPairResponse)
{-# DEPRECATED ddkprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
