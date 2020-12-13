{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption configuration information.
module Network.AWS.CloudFront.GetFieldLevelEncryption
  ( -- * Creating a request
    GetFieldLevelEncryption (..),
    mkGetFieldLevelEncryption,

    -- ** Request lenses
    gfleId,

    -- * Destructuring the response
    GetFieldLevelEncryptionResponse (..),
    mkGetFieldLevelEncryptionResponse,

    -- ** Response lenses
    gflersETag,
    gflersFieldLevelEncryption,
    gflersResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFieldLevelEncryption' smart constructor.
newtype GetFieldLevelEncryption = GetFieldLevelEncryption'
  { -- | Request the ID for the field-level encryption configuration information.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFieldLevelEncryption' with the minimum fields required to make a request.
--
-- * 'id' - Request the ID for the field-level encryption configuration information.
mkGetFieldLevelEncryption ::
  -- | 'id'
  Lude.Text ->
  GetFieldLevelEncryption
mkGetFieldLevelEncryption pId_ =
  GetFieldLevelEncryption' {id = pId_}

-- | Request the ID for the field-level encryption configuration information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfleId :: Lens.Lens' GetFieldLevelEncryption Lude.Text
gfleId = Lens.lens (id :: GetFieldLevelEncryption -> Lude.Text) (\s a -> s {id = a} :: GetFieldLevelEncryption)
{-# DEPRECATED gfleId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetFieldLevelEncryption where
  type Rs GetFieldLevelEncryption = GetFieldLevelEncryptionResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFieldLevelEncryption where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetFieldLevelEncryption where
  toPath GetFieldLevelEncryption' {..} =
    Lude.mconcat
      ["/2020-05-31/field-level-encryption/", Lude.toBS id]

instance Lude.ToQuery GetFieldLevelEncryption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFieldLevelEncryptionResponse' smart constructor.
data GetFieldLevelEncryptionResponse = GetFieldLevelEncryptionResponse'
  { -- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Lude.Maybe Lude.Text,
    -- | Return the field-level encryption configuration information.
    fieldLevelEncryption :: Lude.Maybe FieldLevelEncryption,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFieldLevelEncryptionResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'fieldLevelEncryption' - Return the field-level encryption configuration information.
-- * 'responseStatus' - The response status code.
mkGetFieldLevelEncryptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFieldLevelEncryptionResponse
mkGetFieldLevelEncryptionResponse pResponseStatus_ =
  GetFieldLevelEncryptionResponse'
    { eTag = Lude.Nothing,
      fieldLevelEncryption = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflersETag :: Lens.Lens' GetFieldLevelEncryptionResponse (Lude.Maybe Lude.Text)
gflersETag = Lens.lens (eTag :: GetFieldLevelEncryptionResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetFieldLevelEncryptionResponse)
{-# DEPRECATED gflersETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Return the field-level encryption configuration information.
--
-- /Note:/ Consider using 'fieldLevelEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflersFieldLevelEncryption :: Lens.Lens' GetFieldLevelEncryptionResponse (Lude.Maybe FieldLevelEncryption)
gflersFieldLevelEncryption = Lens.lens (fieldLevelEncryption :: GetFieldLevelEncryptionResponse -> Lude.Maybe FieldLevelEncryption) (\s a -> s {fieldLevelEncryption = a} :: GetFieldLevelEncryptionResponse)
{-# DEPRECATED gflersFieldLevelEncryption "Use generic-lens or generic-optics with 'fieldLevelEncryption' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflersResponseStatus :: Lens.Lens' GetFieldLevelEncryptionResponse Lude.Int
gflersResponseStatus = Lens.lens (responseStatus :: GetFieldLevelEncryptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFieldLevelEncryptionResponse)
{-# DEPRECATED gflersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
