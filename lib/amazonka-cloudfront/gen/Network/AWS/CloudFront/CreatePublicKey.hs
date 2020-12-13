{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreatePublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a public key to CloudFront that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
module Network.AWS.CloudFront.CreatePublicKey
  ( -- * Creating a request
    CreatePublicKey (..),
    mkCreatePublicKey,

    -- ** Request lenses
    cpkPublicKeyConfig,

    -- * Destructuring the response
    CreatePublicKeyResponse (..),
    mkCreatePublicKeyResponse,

    -- ** Response lenses
    cpkrsETag,
    cpkrsLocation,
    cpkrsPublicKey,
    cpkrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePublicKey' smart constructor.
newtype CreatePublicKey = CreatePublicKey'
  { -- | A CloudFront public key configuration.
    publicKeyConfig :: PublicKeyConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePublicKey' with the minimum fields required to make a request.
--
-- * 'publicKeyConfig' - A CloudFront public key configuration.
mkCreatePublicKey ::
  -- | 'publicKeyConfig'
  PublicKeyConfig ->
  CreatePublicKey
mkCreatePublicKey pPublicKeyConfig_ =
  CreatePublicKey' {publicKeyConfig = pPublicKeyConfig_}

-- | A CloudFront public key configuration.
--
-- /Note:/ Consider using 'publicKeyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkPublicKeyConfig :: Lens.Lens' CreatePublicKey PublicKeyConfig
cpkPublicKeyConfig = Lens.lens (publicKeyConfig :: CreatePublicKey -> PublicKeyConfig) (\s a -> s {publicKeyConfig = a} :: CreatePublicKey)
{-# DEPRECATED cpkPublicKeyConfig "Use generic-lens or generic-optics with 'publicKeyConfig' instead." #-}

instance Lude.AWSRequest CreatePublicKey where
  type Rs CreatePublicKey = CreatePublicKeyResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreatePublicKeyResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreatePublicKey where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}PublicKeyConfig"
      Lude.. publicKeyConfig

instance Lude.ToHeaders CreatePublicKey where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreatePublicKey where
  toPath = Lude.const "/2020-05-31/public-key"

instance Lude.ToQuery CreatePublicKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePublicKeyResponse' smart constructor.
data CreatePublicKeyResponse = CreatePublicKeyResponse'
  { -- | The identifier for this version of the public key.
    eTag :: Lude.Maybe Lude.Text,
    -- | The URL of the public key.
    location :: Lude.Maybe Lude.Text,
    -- | The public key.
    publicKey :: Lude.Maybe PublicKey,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePublicKeyResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The identifier for this version of the public key.
-- * 'location' - The URL of the public key.
-- * 'publicKey' - The public key.
-- * 'responseStatus' - The response status code.
mkCreatePublicKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePublicKeyResponse
mkCreatePublicKeyResponse pResponseStatus_ =
  CreatePublicKeyResponse'
    { eTag = Lude.Nothing,
      location = Lude.Nothing,
      publicKey = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for this version of the public key.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrsETag :: Lens.Lens' CreatePublicKeyResponse (Lude.Maybe Lude.Text)
cpkrsETag = Lens.lens (eTag :: CreatePublicKeyResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreatePublicKeyResponse)
{-# DEPRECATED cpkrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The URL of the public key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrsLocation :: Lens.Lens' CreatePublicKeyResponse (Lude.Maybe Lude.Text)
cpkrsLocation = Lens.lens (location :: CreatePublicKeyResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreatePublicKeyResponse)
{-# DEPRECATED cpkrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The public key.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrsPublicKey :: Lens.Lens' CreatePublicKeyResponse (Lude.Maybe PublicKey)
cpkrsPublicKey = Lens.lens (publicKey :: CreatePublicKeyResponse -> Lude.Maybe PublicKey) (\s a -> s {publicKey = a} :: CreatePublicKeyResponse)
{-# DEPRECATED cpkrsPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpkrsResponseStatus :: Lens.Lens' CreatePublicKeyResponse Lude.Int
cpkrsResponseStatus = Lens.lens (responseStatus :: CreatePublicKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePublicKeyResponse)
{-# DEPRECATED cpkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
