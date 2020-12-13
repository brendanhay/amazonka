{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.ImportKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a public SSH key from a specific key pair.
module Network.AWS.Lightsail.ImportKeyPair
  ( -- * Creating a request
    ImportKeyPair (..),
    mkImportKeyPair,

    -- ** Request lenses
    ikpPublicKeyBase64,
    ikpKeyPairName,

    -- * Destructuring the response
    ImportKeyPairResponse (..),
    mkImportKeyPairResponse,

    -- ** Response lenses
    ikprsOperation,
    ikprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { -- | A base64-encoded public key of the @ssh-rsa@ type.
    publicKeyBase64 :: Lude.Text,
    -- | The name of the key pair for which you want to import the public key.
    keyPairName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportKeyPair' with the minimum fields required to make a request.
--
-- * 'publicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
-- * 'keyPairName' - The name of the key pair for which you want to import the public key.
mkImportKeyPair ::
  -- | 'publicKeyBase64'
  Lude.Text ->
  -- | 'keyPairName'
  Lude.Text ->
  ImportKeyPair
mkImportKeyPair pPublicKeyBase64_ pKeyPairName_ =
  ImportKeyPair'
    { publicKeyBase64 = pPublicKeyBase64_,
      keyPairName = pKeyPairName_
    }

-- | A base64-encoded public key of the @ssh-rsa@ type.
--
-- /Note:/ Consider using 'publicKeyBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpPublicKeyBase64 :: Lens.Lens' ImportKeyPair Lude.Text
ikpPublicKeyBase64 = Lens.lens (publicKeyBase64 :: ImportKeyPair -> Lude.Text) (\s a -> s {publicKeyBase64 = a} :: ImportKeyPair)
{-# DEPRECATED ikpPublicKeyBase64 "Use generic-lens or generic-optics with 'publicKeyBase64' instead." #-}

-- | The name of the key pair for which you want to import the public key.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpKeyPairName :: Lens.Lens' ImportKeyPair Lude.Text
ikpKeyPairName = Lens.lens (keyPairName :: ImportKeyPair -> Lude.Text) (\s a -> s {keyPairName = a} :: ImportKeyPair)
{-# DEPRECATED ikpKeyPairName "Use generic-lens or generic-optics with 'keyPairName' instead." #-}

instance Lude.AWSRequest ImportKeyPair where
  type Rs ImportKeyPair = ImportKeyPairResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ImportKeyPairResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportKeyPair where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.ImportKeyPair" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportKeyPair where
  toJSON ImportKeyPair' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("publicKeyBase64" Lude..= publicKeyBase64),
            Lude.Just ("keyPairName" Lude..= keyPairName)
          ]
      )

instance Lude.ToPath ImportKeyPair where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportKeyPair where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportKeyPairResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkImportKeyPairResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportKeyPairResponse
mkImportKeyPairResponse pResponseStatus_ =
  ImportKeyPairResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprsOperation :: Lens.Lens' ImportKeyPairResponse (Lude.Maybe Operation)
ikprsOperation = Lens.lens (operation :: ImportKeyPairResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: ImportKeyPairResponse)
{-# DEPRECATED ikprsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprsResponseStatus :: Lens.Lens' ImportKeyPairResponse Lude.Int
ikprsResponseStatus = Lens.lens (responseStatus :: ImportKeyPairResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportKeyPairResponse)
{-# DEPRECATED ikprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
