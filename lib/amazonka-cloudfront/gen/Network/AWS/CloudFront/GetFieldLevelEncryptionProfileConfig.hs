{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption profile configuration information.
module Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig
  ( -- * Creating a request
    GetFieldLevelEncryptionProfileConfig (..),
    mkGetFieldLevelEncryptionProfileConfig,

    -- ** Request lenses
    gflepcId,

    -- * Destructuring the response
    GetFieldLevelEncryptionProfileConfigResponse (..),
    mkGetFieldLevelEncryptionProfileConfigResponse,

    -- ** Response lenses
    gflepcrsETag,
    gflepcrsFieldLevelEncryptionProfileConfig,
    gflepcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFieldLevelEncryptionProfileConfig' smart constructor.
newtype GetFieldLevelEncryptionProfileConfig = GetFieldLevelEncryptionProfileConfig'
  { -- | Get the ID for the field-level encryption profile configuration information.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFieldLevelEncryptionProfileConfig' with the minimum fields required to make a request.
--
-- * 'id' - Get the ID for the field-level encryption profile configuration information.
mkGetFieldLevelEncryptionProfileConfig ::
  -- | 'id'
  Lude.Text ->
  GetFieldLevelEncryptionProfileConfig
mkGetFieldLevelEncryptionProfileConfig pId_ =
  GetFieldLevelEncryptionProfileConfig' {id = pId_}

-- | Get the ID for the field-level encryption profile configuration information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflepcId :: Lens.Lens' GetFieldLevelEncryptionProfileConfig Lude.Text
gflepcId = Lens.lens (id :: GetFieldLevelEncryptionProfileConfig -> Lude.Text) (\s a -> s {id = a} :: GetFieldLevelEncryptionProfileConfig)
{-# DEPRECATED gflepcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetFieldLevelEncryptionProfileConfig where
  type
    Rs GetFieldLevelEncryptionProfileConfig =
      GetFieldLevelEncryptionProfileConfigResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionProfileConfigResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFieldLevelEncryptionProfileConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetFieldLevelEncryptionProfileConfig where
  toPath GetFieldLevelEncryptionProfileConfig' {..} =
    Lude.mconcat
      [ "/2020-05-31/field-level-encryption-profile/",
        Lude.toBS id,
        "/config"
      ]

instance Lude.ToQuery GetFieldLevelEncryptionProfileConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFieldLevelEncryptionProfileConfigResponse' smart constructor.
data GetFieldLevelEncryptionProfileConfigResponse = GetFieldLevelEncryptionProfileConfigResponse'
  { -- | The current version of the field-level encryption profile configuration result. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Lude.Maybe Lude.Text,
    -- | Return the field-level encryption profile configuration information.
    fieldLevelEncryptionProfileConfig :: Lude.Maybe FieldLevelEncryptionProfileConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFieldLevelEncryptionProfileConfigResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the field-level encryption profile configuration result. For example: @E2QWRUHAPOMQZL@ .
-- * 'fieldLevelEncryptionProfileConfig' - Return the field-level encryption profile configuration information.
-- * 'responseStatus' - The response status code.
mkGetFieldLevelEncryptionProfileConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFieldLevelEncryptionProfileConfigResponse
mkGetFieldLevelEncryptionProfileConfigResponse pResponseStatus_ =
  GetFieldLevelEncryptionProfileConfigResponse'
    { eTag =
        Lude.Nothing,
      fieldLevelEncryptionProfileConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the field-level encryption profile configuration result. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflepcrsETag :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse (Lude.Maybe Lude.Text)
gflepcrsETag = Lens.lens (eTag :: GetFieldLevelEncryptionProfileConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetFieldLevelEncryptionProfileConfigResponse)
{-# DEPRECATED gflepcrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Return the field-level encryption profile configuration information.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflepcrsFieldLevelEncryptionProfileConfig :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse (Lude.Maybe FieldLevelEncryptionProfileConfig)
gflepcrsFieldLevelEncryptionProfileConfig = Lens.lens (fieldLevelEncryptionProfileConfig :: GetFieldLevelEncryptionProfileConfigResponse -> Lude.Maybe FieldLevelEncryptionProfileConfig) (\s a -> s {fieldLevelEncryptionProfileConfig = a} :: GetFieldLevelEncryptionProfileConfigResponse)
{-# DEPRECATED gflepcrsFieldLevelEncryptionProfileConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflepcrsResponseStatus :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse Lude.Int
gflepcrsResponseStatus = Lens.lens (responseStatus :: GetFieldLevelEncryptionProfileConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFieldLevelEncryptionProfileConfigResponse)
{-# DEPRECATED gflepcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
