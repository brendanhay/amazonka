{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption configuration information.
module Network.AWS.CloudFront.GetFieldLevelEncryptionConfig
  ( -- * Creating a request
    GetFieldLevelEncryptionConfig (..),
    mkGetFieldLevelEncryptionConfig,

    -- ** Request lenses
    gflecId,

    -- * Destructuring the response
    GetFieldLevelEncryptionConfigResponse (..),
    mkGetFieldLevelEncryptionConfigResponse,

    -- ** Response lenses
    gflecrsETag,
    gflecrsFieldLevelEncryptionConfig,
    gflecrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFieldLevelEncryptionConfig' smart constructor.
newtype GetFieldLevelEncryptionConfig = GetFieldLevelEncryptionConfig'
  { -- | Request the ID for the field-level encryption configuration information.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFieldLevelEncryptionConfig' with the minimum fields required to make a request.
--
-- * 'id' - Request the ID for the field-level encryption configuration information.
mkGetFieldLevelEncryptionConfig ::
  -- | 'id'
  Lude.Text ->
  GetFieldLevelEncryptionConfig
mkGetFieldLevelEncryptionConfig pId_ =
  GetFieldLevelEncryptionConfig' {id = pId_}

-- | Request the ID for the field-level encryption configuration information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflecId :: Lens.Lens' GetFieldLevelEncryptionConfig Lude.Text
gflecId = Lens.lens (id :: GetFieldLevelEncryptionConfig -> Lude.Text) (\s a -> s {id = a} :: GetFieldLevelEncryptionConfig)
{-# DEPRECATED gflecId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetFieldLevelEncryptionConfig where
  type
    Rs GetFieldLevelEncryptionConfig =
      GetFieldLevelEncryptionConfigResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionConfigResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFieldLevelEncryptionConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetFieldLevelEncryptionConfig where
  toPath GetFieldLevelEncryptionConfig' {..} =
    Lude.mconcat
      ["/2020-05-31/field-level-encryption/", Lude.toBS id, "/config"]

instance Lude.ToQuery GetFieldLevelEncryptionConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFieldLevelEncryptionConfigResponse' smart constructor.
data GetFieldLevelEncryptionConfigResponse = GetFieldLevelEncryptionConfigResponse'
  { -- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Lude.Maybe Lude.Text,
    -- | Return the field-level encryption configuration information.
    fieldLevelEncryptionConfig :: Lude.Maybe FieldLevelEncryptionConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFieldLevelEncryptionConfigResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'fieldLevelEncryptionConfig' - Return the field-level encryption configuration information.
-- * 'responseStatus' - The response status code.
mkGetFieldLevelEncryptionConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFieldLevelEncryptionConfigResponse
mkGetFieldLevelEncryptionConfigResponse pResponseStatus_ =
  GetFieldLevelEncryptionConfigResponse'
    { eTag = Lude.Nothing,
      fieldLevelEncryptionConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflecrsETag :: Lens.Lens' GetFieldLevelEncryptionConfigResponse (Lude.Maybe Lude.Text)
gflecrsETag = Lens.lens (eTag :: GetFieldLevelEncryptionConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetFieldLevelEncryptionConfigResponse)
{-# DEPRECATED gflecrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Return the field-level encryption configuration information.
--
-- /Note:/ Consider using 'fieldLevelEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflecrsFieldLevelEncryptionConfig :: Lens.Lens' GetFieldLevelEncryptionConfigResponse (Lude.Maybe FieldLevelEncryptionConfig)
gflecrsFieldLevelEncryptionConfig = Lens.lens (fieldLevelEncryptionConfig :: GetFieldLevelEncryptionConfigResponse -> Lude.Maybe FieldLevelEncryptionConfig) (\s a -> s {fieldLevelEncryptionConfig = a} :: GetFieldLevelEncryptionConfigResponse)
{-# DEPRECATED gflecrsFieldLevelEncryptionConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflecrsResponseStatus :: Lens.Lens' GetFieldLevelEncryptionConfigResponse Lude.Int
gflecrsResponseStatus = Lens.lens (responseStatus :: GetFieldLevelEncryptionConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFieldLevelEncryptionConfigResponse)
{-# DEPRECATED gflecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
