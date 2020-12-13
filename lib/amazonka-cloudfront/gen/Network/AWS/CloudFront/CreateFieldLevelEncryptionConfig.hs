{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new field-level encryption configuration.
module Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
  ( -- * Creating a request
    CreateFieldLevelEncryptionConfig (..),
    mkCreateFieldLevelEncryptionConfig,

    -- ** Request lenses
    cflecFieldLevelEncryptionConfig,

    -- * Destructuring the response
    CreateFieldLevelEncryptionConfigResponse (..),
    mkCreateFieldLevelEncryptionConfigResponse,

    -- ** Response lenses
    cflecrsETag,
    cflecrsLocation,
    cflecrsFieldLevelEncryption,
    cflecrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFieldLevelEncryptionConfig' smart constructor.
newtype CreateFieldLevelEncryptionConfig = CreateFieldLevelEncryptionConfig'
  { -- | The request to create a new field-level encryption configuration.
    fieldLevelEncryptionConfig :: FieldLevelEncryptionConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFieldLevelEncryptionConfig' with the minimum fields required to make a request.
--
-- * 'fieldLevelEncryptionConfig' - The request to create a new field-level encryption configuration.
mkCreateFieldLevelEncryptionConfig ::
  -- | 'fieldLevelEncryptionConfig'
  FieldLevelEncryptionConfig ->
  CreateFieldLevelEncryptionConfig
mkCreateFieldLevelEncryptionConfig pFieldLevelEncryptionConfig_ =
  CreateFieldLevelEncryptionConfig'
    { fieldLevelEncryptionConfig =
        pFieldLevelEncryptionConfig_
    }

-- | The request to create a new field-level encryption configuration.
--
-- /Note:/ Consider using 'fieldLevelEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecFieldLevelEncryptionConfig :: Lens.Lens' CreateFieldLevelEncryptionConfig FieldLevelEncryptionConfig
cflecFieldLevelEncryptionConfig = Lens.lens (fieldLevelEncryptionConfig :: CreateFieldLevelEncryptionConfig -> FieldLevelEncryptionConfig) (\s a -> s {fieldLevelEncryptionConfig = a} :: CreateFieldLevelEncryptionConfig)
{-# DEPRECATED cflecFieldLevelEncryptionConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionConfig' instead." #-}

instance Lude.AWSRequest CreateFieldLevelEncryptionConfig where
  type
    Rs CreateFieldLevelEncryptionConfig =
      CreateFieldLevelEncryptionConfigResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateFieldLevelEncryptionConfigResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateFieldLevelEncryptionConfig where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}FieldLevelEncryptionConfig"
      Lude.. fieldLevelEncryptionConfig

instance Lude.ToHeaders CreateFieldLevelEncryptionConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateFieldLevelEncryptionConfig where
  toPath = Lude.const "/2020-05-31/field-level-encryption"

instance Lude.ToQuery CreateFieldLevelEncryptionConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateFieldLevelEncryptionConfigResponse' smart constructor.
data CreateFieldLevelEncryptionConfigResponse = CreateFieldLevelEncryptionConfigResponse'
  { -- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Lude.Maybe Lude.Text,
    -- | The fully qualified URI of the new configuration resource just created.
    location :: Lude.Maybe Lude.Text,
    -- | Returned when you create a new field-level encryption configuration.
    fieldLevelEncryption :: Lude.Maybe FieldLevelEncryption,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFieldLevelEncryptionConfigResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'location' - The fully qualified URI of the new configuration resource just created.
-- * 'fieldLevelEncryption' - Returned when you create a new field-level encryption configuration.
-- * 'responseStatus' - The response status code.
mkCreateFieldLevelEncryptionConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFieldLevelEncryptionConfigResponse
mkCreateFieldLevelEncryptionConfigResponse pResponseStatus_ =
  CreateFieldLevelEncryptionConfigResponse'
    { eTag = Lude.Nothing,
      location = Lude.Nothing,
      fieldLevelEncryption = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrsETag :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Lude.Maybe Lude.Text)
cflecrsETag = Lens.lens (eTag :: CreateFieldLevelEncryptionConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreateFieldLevelEncryptionConfigResponse)
{-# DEPRECATED cflecrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the new configuration resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrsLocation :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Lude.Maybe Lude.Text)
cflecrsLocation = Lens.lens (location :: CreateFieldLevelEncryptionConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateFieldLevelEncryptionConfigResponse)
{-# DEPRECATED cflecrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | Returned when you create a new field-level encryption configuration.
--
-- /Note:/ Consider using 'fieldLevelEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrsFieldLevelEncryption :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Lude.Maybe FieldLevelEncryption)
cflecrsFieldLevelEncryption = Lens.lens (fieldLevelEncryption :: CreateFieldLevelEncryptionConfigResponse -> Lude.Maybe FieldLevelEncryption) (\s a -> s {fieldLevelEncryption = a} :: CreateFieldLevelEncryptionConfigResponse)
{-# DEPRECATED cflecrsFieldLevelEncryption "Use generic-lens or generic-optics with 'fieldLevelEncryption' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrsResponseStatus :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse Lude.Int
cflecrsResponseStatus = Lens.lens (responseStatus :: CreateFieldLevelEncryptionConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFieldLevelEncryptionConfigResponse)
{-# DEPRECATED cflecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
