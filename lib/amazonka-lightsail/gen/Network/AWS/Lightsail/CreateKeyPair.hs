{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SSH key pair.
--
-- The @create key pair@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateKeyPair
  ( -- * Creating a request
    CreateKeyPair (..),
    mkCreateKeyPair,

    -- ** Request lenses
    ckpTags,
    ckpKeyPairName,

    -- * Destructuring the response
    CreateKeyPairResponse (..),
    mkCreateKeyPairResponse,

    -- ** Response lenses
    ckprsKeyPair,
    ckprsOperation,
    ckprsPublicKeyBase64,
    ckprsPrivateKeyBase64,
    ckprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateKeyPair' smart constructor.
data CreateKeyPair = CreateKeyPair'
  { tags :: Lude.Maybe [Tag],
    keyPairName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateKeyPair' with the minimum fields required to make a request.
--
-- * 'keyPairName' - The name for your new key pair.
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
mkCreateKeyPair ::
  -- | 'keyPairName'
  Lude.Text ->
  CreateKeyPair
mkCreateKeyPair pKeyPairName_ =
  CreateKeyPair' {tags = Lude.Nothing, keyPairName = pKeyPairName_}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckpTags :: Lens.Lens' CreateKeyPair (Lude.Maybe [Tag])
ckpTags = Lens.lens (tags :: CreateKeyPair -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateKeyPair)
{-# DEPRECATED ckpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name for your new key pair.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckpKeyPairName :: Lens.Lens' CreateKeyPair Lude.Text
ckpKeyPairName = Lens.lens (keyPairName :: CreateKeyPair -> Lude.Text) (\s a -> s {keyPairName = a} :: CreateKeyPair)
{-# DEPRECATED ckpKeyPairName "Use generic-lens or generic-optics with 'keyPairName' instead." #-}

instance Lude.AWSRequest CreateKeyPair where
  type Rs CreateKeyPair = CreateKeyPairResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateKeyPairResponse'
            Lude.<$> (x Lude..?> "keyPair")
            Lude.<*> (x Lude..?> "operation")
            Lude.<*> (x Lude..?> "publicKeyBase64")
            Lude.<*> (x Lude..?> "privateKeyBase64")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateKeyPair where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateKeyPair" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateKeyPair where
  toJSON CreateKeyPair' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("keyPairName" Lude..= keyPairName)
          ]
      )

instance Lude.ToPath CreateKeyPair where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateKeyPair where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateKeyPairResponse' smart constructor.
data CreateKeyPairResponse = CreateKeyPairResponse'
  { keyPair ::
      Lude.Maybe KeyPair,
    operation :: Lude.Maybe Operation,
    publicKeyBase64 :: Lude.Maybe Lude.Text,
    privateKeyBase64 :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateKeyPairResponse' with the minimum fields required to make a request.
--
-- * 'keyPair' - An array of key-value pairs containing information about the new key pair you just created.
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'privateKeyBase64' - A base64-encoded RSA private key.
-- * 'publicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
-- * 'responseStatus' - The response status code.
mkCreateKeyPairResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateKeyPairResponse
mkCreateKeyPairResponse pResponseStatus_ =
  CreateKeyPairResponse'
    { keyPair = Lude.Nothing,
      operation = Lude.Nothing,
      publicKeyBase64 = Lude.Nothing,
      privateKeyBase64 = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of key-value pairs containing information about the new key pair you just created.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprsKeyPair :: Lens.Lens' CreateKeyPairResponse (Lude.Maybe KeyPair)
ckprsKeyPair = Lens.lens (keyPair :: CreateKeyPairResponse -> Lude.Maybe KeyPair) (\s a -> s {keyPair = a} :: CreateKeyPairResponse)
{-# DEPRECATED ckprsKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprsOperation :: Lens.Lens' CreateKeyPairResponse (Lude.Maybe Operation)
ckprsOperation = Lens.lens (operation :: CreateKeyPairResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: CreateKeyPairResponse)
{-# DEPRECATED ckprsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | A base64-encoded public key of the @ssh-rsa@ type.
--
-- /Note:/ Consider using 'publicKeyBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprsPublicKeyBase64 :: Lens.Lens' CreateKeyPairResponse (Lude.Maybe Lude.Text)
ckprsPublicKeyBase64 = Lens.lens (publicKeyBase64 :: CreateKeyPairResponse -> Lude.Maybe Lude.Text) (\s a -> s {publicKeyBase64 = a} :: CreateKeyPairResponse)
{-# DEPRECATED ckprsPublicKeyBase64 "Use generic-lens or generic-optics with 'publicKeyBase64' instead." #-}

-- | A base64-encoded RSA private key.
--
-- /Note:/ Consider using 'privateKeyBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprsPrivateKeyBase64 :: Lens.Lens' CreateKeyPairResponse (Lude.Maybe Lude.Text)
ckprsPrivateKeyBase64 = Lens.lens (privateKeyBase64 :: CreateKeyPairResponse -> Lude.Maybe Lude.Text) (\s a -> s {privateKeyBase64 = a} :: CreateKeyPairResponse)
{-# DEPRECATED ckprsPrivateKeyBase64 "Use generic-lens or generic-optics with 'privateKeyBase64' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckprsResponseStatus :: Lens.Lens' CreateKeyPairResponse Lude.Int
ckprsResponseStatus = Lens.lens (responseStatus :: CreateKeyPairResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateKeyPairResponse)
{-# DEPRECATED ckprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
