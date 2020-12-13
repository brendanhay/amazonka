{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.PutEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the encryption configuration for X-Ray data.
module Network.AWS.XRay.PutEncryptionConfig
  ( -- * Creating a request
    PutEncryptionConfig (..),
    mkPutEncryptionConfig,

    -- ** Request lenses
    pecKeyId,
    pecType,

    -- * Destructuring the response
    PutEncryptionConfigResponse (..),
    mkPutEncryptionConfigResponse,

    -- ** Response lenses
    pecrsEncryptionConfig,
    pecrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkPutEncryptionConfig' smart constructor.
data PutEncryptionConfig = PutEncryptionConfig'
  { -- | An AWS KMS customer master key (CMK) in one of the following formats:
    --
    --
    --     * __Alias__ - The name of the key. For example, @alias/MyKey@ .
    --
    --
    --     * __Key ID__ - The KMS key ID of the key. For example, @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . AWS X-Ray does not support asymmetric CMKs.
    --
    --
    --     * __ARN__ - The full Amazon Resource Name of the key ID or alias. For example, @arn:aws:kms:us-east-2:123456789012:key/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . Use this format to specify a key in a different account.
    --
    --
    -- Omit this key if you set @Type@ to @NONE@ .
    keyId :: Lude.Maybe Lude.Text,
    -- | The type of encryption. Set to @KMS@ to use your own key for encryption. Set to @NONE@ for default encryption.
    type' :: EncryptionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutEncryptionConfig' with the minimum fields required to make a request.
--
-- * 'keyId' - An AWS KMS customer master key (CMK) in one of the following formats:
--
--
--     * __Alias__ - The name of the key. For example, @alias/MyKey@ .
--
--
--     * __Key ID__ - The KMS key ID of the key. For example, @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . AWS X-Ray does not support asymmetric CMKs.
--
--
--     * __ARN__ - The full Amazon Resource Name of the key ID or alias. For example, @arn:aws:kms:us-east-2:123456789012:key/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . Use this format to specify a key in a different account.
--
--
-- Omit this key if you set @Type@ to @NONE@ .
-- * 'type'' - The type of encryption. Set to @KMS@ to use your own key for encryption. Set to @NONE@ for default encryption.
mkPutEncryptionConfig ::
  -- | 'type''
  EncryptionType ->
  PutEncryptionConfig
mkPutEncryptionConfig pType_ =
  PutEncryptionConfig' {keyId = Lude.Nothing, type' = pType_}

-- | An AWS KMS customer master key (CMK) in one of the following formats:
--
--
--     * __Alias__ - The name of the key. For example, @alias/MyKey@ .
--
--
--     * __Key ID__ - The KMS key ID of the key. For example, @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . AWS X-Ray does not support asymmetric CMKs.
--
--
--     * __ARN__ - The full Amazon Resource Name of the key ID or alias. For example, @arn:aws:kms:us-east-2:123456789012:key/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . Use this format to specify a key in a different account.
--
--
-- Omit this key if you set @Type@ to @NONE@ .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pecKeyId :: Lens.Lens' PutEncryptionConfig (Lude.Maybe Lude.Text)
pecKeyId = Lens.lens (keyId :: PutEncryptionConfig -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: PutEncryptionConfig)
{-# DEPRECATED pecKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The type of encryption. Set to @KMS@ to use your own key for encryption. Set to @NONE@ for default encryption.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pecType :: Lens.Lens' PutEncryptionConfig EncryptionType
pecType = Lens.lens (type' :: PutEncryptionConfig -> EncryptionType) (\s a -> s {type' = a} :: PutEncryptionConfig)
{-# DEPRECATED pecType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest PutEncryptionConfig where
  type Rs PutEncryptionConfig = PutEncryptionConfigResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutEncryptionConfigResponse'
            Lude.<$> (x Lude..?> "EncryptionConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutEncryptionConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutEncryptionConfig where
  toJSON PutEncryptionConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyId" Lude..=) Lude.<$> keyId,
            Lude.Just ("Type" Lude..= type')
          ]
      )

instance Lude.ToPath PutEncryptionConfig where
  toPath = Lude.const "/PutEncryptionConfig"

instance Lude.ToQuery PutEncryptionConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutEncryptionConfigResponse' smart constructor.
data PutEncryptionConfigResponse = PutEncryptionConfigResponse'
  { -- | The new encryption configuration.
    encryptionConfig :: Lude.Maybe EncryptionConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutEncryptionConfigResponse' with the minimum fields required to make a request.
--
-- * 'encryptionConfig' - The new encryption configuration.
-- * 'responseStatus' - The response status code.
mkPutEncryptionConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutEncryptionConfigResponse
mkPutEncryptionConfigResponse pResponseStatus_ =
  PutEncryptionConfigResponse'
    { encryptionConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The new encryption configuration.
--
-- /Note:/ Consider using 'encryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pecrsEncryptionConfig :: Lens.Lens' PutEncryptionConfigResponse (Lude.Maybe EncryptionConfig)
pecrsEncryptionConfig = Lens.lens (encryptionConfig :: PutEncryptionConfigResponse -> Lude.Maybe EncryptionConfig) (\s a -> s {encryptionConfig = a} :: PutEncryptionConfigResponse)
{-# DEPRECATED pecrsEncryptionConfig "Use generic-lens or generic-optics with 'encryptionConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pecrsResponseStatus :: Lens.Lens' PutEncryptionConfigResponse Lude.Int
pecrsResponseStatus = Lens.lens (responseStatus :: PutEncryptionConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutEncryptionConfigResponse)
{-# DEPRECATED pecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
