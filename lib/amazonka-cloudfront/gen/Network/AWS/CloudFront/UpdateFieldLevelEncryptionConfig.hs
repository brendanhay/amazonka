{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a field-level encryption configuration.
module Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig
  ( -- * Creating a request
    UpdateFieldLevelEncryptionConfig (..),
    mkUpdateFieldLevelEncryptionConfig,

    -- ** Request lenses
    uflecIfMatch,
    uflecId,
    uflecFieldLevelEncryptionConfig,

    -- * Destructuring the response
    UpdateFieldLevelEncryptionConfigResponse (..),
    mkUpdateFieldLevelEncryptionConfigResponse,

    -- ** Response lenses
    uflecrsETag,
    uflecrsFieldLevelEncryption,
    uflecrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateFieldLevelEncryptionConfig' smart constructor.
data UpdateFieldLevelEncryptionConfig = UpdateFieldLevelEncryptionConfig'
  { -- | The value of the @ETag@ header that you received when retrieving the configuration identity to update. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Lude.Maybe Lude.Text,
    -- | The ID of the configuration you want to update.
    id :: Lude.Text,
    -- | Request to update a field-level encryption configuration.
    fieldLevelEncryptionConfig :: FieldLevelEncryptionConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFieldLevelEncryptionConfig' with the minimum fields required to make a request.
--
-- * 'ifMatch' - The value of the @ETag@ header that you received when retrieving the configuration identity to update. For example: @E2QWRUHAPOMQZL@ .
-- * 'id' - The ID of the configuration you want to update.
-- * 'fieldLevelEncryptionConfig' - Request to update a field-level encryption configuration.
mkUpdateFieldLevelEncryptionConfig ::
  -- | 'id'
  Lude.Text ->
  -- | 'fieldLevelEncryptionConfig'
  FieldLevelEncryptionConfig ->
  UpdateFieldLevelEncryptionConfig
mkUpdateFieldLevelEncryptionConfig
  pId_
  pFieldLevelEncryptionConfig_ =
    UpdateFieldLevelEncryptionConfig'
      { ifMatch = Lude.Nothing,
        id = pId_,
        fieldLevelEncryptionConfig = pFieldLevelEncryptionConfig_
      }

-- | The value of the @ETag@ header that you received when retrieving the configuration identity to update. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecIfMatch :: Lens.Lens' UpdateFieldLevelEncryptionConfig (Lude.Maybe Lude.Text)
uflecIfMatch = Lens.lens (ifMatch :: UpdateFieldLevelEncryptionConfig -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: UpdateFieldLevelEncryptionConfig)
{-# DEPRECATED uflecIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The ID of the configuration you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecId :: Lens.Lens' UpdateFieldLevelEncryptionConfig Lude.Text
uflecId = Lens.lens (id :: UpdateFieldLevelEncryptionConfig -> Lude.Text) (\s a -> s {id = a} :: UpdateFieldLevelEncryptionConfig)
{-# DEPRECATED uflecId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Request to update a field-level encryption configuration.
--
-- /Note:/ Consider using 'fieldLevelEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecFieldLevelEncryptionConfig :: Lens.Lens' UpdateFieldLevelEncryptionConfig FieldLevelEncryptionConfig
uflecFieldLevelEncryptionConfig = Lens.lens (fieldLevelEncryptionConfig :: UpdateFieldLevelEncryptionConfig -> FieldLevelEncryptionConfig) (\s a -> s {fieldLevelEncryptionConfig = a} :: UpdateFieldLevelEncryptionConfig)
{-# DEPRECATED uflecFieldLevelEncryptionConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionConfig' instead." #-}

instance Lude.AWSRequest UpdateFieldLevelEncryptionConfig where
  type
    Rs UpdateFieldLevelEncryptionConfig =
      UpdateFieldLevelEncryptionConfigResponse
  request = Req.putXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateFieldLevelEncryptionConfigResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateFieldLevelEncryptionConfig where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}FieldLevelEncryptionConfig"
      Lude.. fieldLevelEncryptionConfig

instance Lude.ToHeaders UpdateFieldLevelEncryptionConfig where
  toHeaders UpdateFieldLevelEncryptionConfig' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath UpdateFieldLevelEncryptionConfig where
  toPath UpdateFieldLevelEncryptionConfig' {..} =
    Lude.mconcat
      ["/2020-05-31/field-level-encryption/", Lude.toBS id, "/config"]

instance Lude.ToQuery UpdateFieldLevelEncryptionConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateFieldLevelEncryptionConfigResponse' smart constructor.
data UpdateFieldLevelEncryptionConfigResponse = UpdateFieldLevelEncryptionConfigResponse'
  { -- | The value of the @ETag@ header that you received when updating the configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Lude.Maybe Lude.Text,
    -- | Return the results of updating the configuration.
    fieldLevelEncryption :: Lude.Maybe FieldLevelEncryption,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFieldLevelEncryptionConfigResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The value of the @ETag@ header that you received when updating the configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'fieldLevelEncryption' - Return the results of updating the configuration.
-- * 'responseStatus' - The response status code.
mkUpdateFieldLevelEncryptionConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateFieldLevelEncryptionConfigResponse
mkUpdateFieldLevelEncryptionConfigResponse pResponseStatus_ =
  UpdateFieldLevelEncryptionConfigResponse'
    { eTag = Lude.Nothing,
      fieldLevelEncryption = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The value of the @ETag@ header that you received when updating the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecrsETag :: Lens.Lens' UpdateFieldLevelEncryptionConfigResponse (Lude.Maybe Lude.Text)
uflecrsETag = Lens.lens (eTag :: UpdateFieldLevelEncryptionConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: UpdateFieldLevelEncryptionConfigResponse)
{-# DEPRECATED uflecrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Return the results of updating the configuration.
--
-- /Note:/ Consider using 'fieldLevelEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecrsFieldLevelEncryption :: Lens.Lens' UpdateFieldLevelEncryptionConfigResponse (Lude.Maybe FieldLevelEncryption)
uflecrsFieldLevelEncryption = Lens.lens (fieldLevelEncryption :: UpdateFieldLevelEncryptionConfigResponse -> Lude.Maybe FieldLevelEncryption) (\s a -> s {fieldLevelEncryption = a} :: UpdateFieldLevelEncryptionConfigResponse)
{-# DEPRECATED uflecrsFieldLevelEncryption "Use generic-lens or generic-optics with 'fieldLevelEncryption' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecrsResponseStatus :: Lens.Lens' UpdateFieldLevelEncryptionConfigResponse Lude.Int
uflecrsResponseStatus = Lens.lens (responseStatus :: UpdateFieldLevelEncryptionConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFieldLevelEncryptionConfigResponse)
{-# DEPRECATED uflecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
