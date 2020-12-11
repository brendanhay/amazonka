{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a field-level encryption profile.
module Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile
  ( -- * Creating a request
    UpdateFieldLevelEncryptionProfile (..),
    mkUpdateFieldLevelEncryptionProfile,

    -- ** Request lenses
    uflepIfMatch,
    uflepFieldLevelEncryptionProfileConfig,
    uflepId,

    -- * Destructuring the response
    UpdateFieldLevelEncryptionProfileResponse (..),
    mkUpdateFieldLevelEncryptionProfileResponse,

    -- ** Response lenses
    ufleprsETag,
    ufleprsFieldLevelEncryptionProfile,
    ufleprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateFieldLevelEncryptionProfile' smart constructor.
data UpdateFieldLevelEncryptionProfile = UpdateFieldLevelEncryptionProfile'
  { ifMatch ::
      Lude.Maybe Lude.Text,
    fieldLevelEncryptionProfileConfig ::
      FieldLevelEncryptionProfileConfig,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- * 'fieldLevelEncryptionProfileConfig' - Request to update a field-level encryption profile.
-- * 'id' - The ID of the field-level encryption profile request.
-- * 'ifMatch' - The value of the @ETag@ header that you received when retrieving the profile identity to update. For example: @E2QWRUHAPOMQZL@ .
mkUpdateFieldLevelEncryptionProfile ::
  -- | 'fieldLevelEncryptionProfileConfig'
  FieldLevelEncryptionProfileConfig ->
  -- | 'id'
  Lude.Text ->
  UpdateFieldLevelEncryptionProfile
mkUpdateFieldLevelEncryptionProfile
  pFieldLevelEncryptionProfileConfig_
  pId_ =
    UpdateFieldLevelEncryptionProfile'
      { ifMatch = Lude.Nothing,
        fieldLevelEncryptionProfileConfig =
          pFieldLevelEncryptionProfileConfig_,
        id = pId_
      }

-- | The value of the @ETag@ header that you received when retrieving the profile identity to update. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflepIfMatch :: Lens.Lens' UpdateFieldLevelEncryptionProfile (Lude.Maybe Lude.Text)
uflepIfMatch = Lens.lens (ifMatch :: UpdateFieldLevelEncryptionProfile -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: UpdateFieldLevelEncryptionProfile)
{-# DEPRECATED uflepIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | Request to update a field-level encryption profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflepFieldLevelEncryptionProfileConfig :: Lens.Lens' UpdateFieldLevelEncryptionProfile FieldLevelEncryptionProfileConfig
uflepFieldLevelEncryptionProfileConfig = Lens.lens (fieldLevelEncryptionProfileConfig :: UpdateFieldLevelEncryptionProfile -> FieldLevelEncryptionProfileConfig) (\s a -> s {fieldLevelEncryptionProfileConfig = a} :: UpdateFieldLevelEncryptionProfile)
{-# DEPRECATED uflepFieldLevelEncryptionProfileConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileConfig' instead." #-}

-- | The ID of the field-level encryption profile request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflepId :: Lens.Lens' UpdateFieldLevelEncryptionProfile Lude.Text
uflepId = Lens.lens (id :: UpdateFieldLevelEncryptionProfile -> Lude.Text) (\s a -> s {id = a} :: UpdateFieldLevelEncryptionProfile)
{-# DEPRECATED uflepId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateFieldLevelEncryptionProfile where
  type
    Rs UpdateFieldLevelEncryptionProfile =
      UpdateFieldLevelEncryptionProfileResponse
  request = Req.putXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateFieldLevelEncryptionProfileResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateFieldLevelEncryptionProfile where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}FieldLevelEncryptionProfileConfig"
      Lude.. fieldLevelEncryptionProfileConfig

instance Lude.ToHeaders UpdateFieldLevelEncryptionProfile where
  toHeaders UpdateFieldLevelEncryptionProfile' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath UpdateFieldLevelEncryptionProfile where
  toPath UpdateFieldLevelEncryptionProfile' {..} =
    Lude.mconcat
      [ "/2020-05-31/field-level-encryption-profile/",
        Lude.toBS id,
        "/config"
      ]

instance Lude.ToQuery UpdateFieldLevelEncryptionProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateFieldLevelEncryptionProfileResponse' smart constructor.
data UpdateFieldLevelEncryptionProfileResponse = UpdateFieldLevelEncryptionProfileResponse'
  { eTag ::
      Lude.Maybe
        Lude.Text,
    fieldLevelEncryptionProfile ::
      Lude.Maybe
        FieldLevelEncryptionProfile,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFieldLevelEncryptionProfileResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The result of the field-level encryption profile request.
-- * 'fieldLevelEncryptionProfile' - Return the results of updating the profile.
-- * 'responseStatus' - The response status code.
mkUpdateFieldLevelEncryptionProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateFieldLevelEncryptionProfileResponse
mkUpdateFieldLevelEncryptionProfileResponse pResponseStatus_ =
  UpdateFieldLevelEncryptionProfileResponse'
    { eTag = Lude.Nothing,
      fieldLevelEncryptionProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The result of the field-level encryption profile request.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufleprsETag :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse (Lude.Maybe Lude.Text)
ufleprsETag = Lens.lens (eTag :: UpdateFieldLevelEncryptionProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: UpdateFieldLevelEncryptionProfileResponse)
{-# DEPRECATED ufleprsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Return the results of updating the profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufleprsFieldLevelEncryptionProfile :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse (Lude.Maybe FieldLevelEncryptionProfile)
ufleprsFieldLevelEncryptionProfile = Lens.lens (fieldLevelEncryptionProfile :: UpdateFieldLevelEncryptionProfileResponse -> Lude.Maybe FieldLevelEncryptionProfile) (\s a -> s {fieldLevelEncryptionProfile = a} :: UpdateFieldLevelEncryptionProfileResponse)
{-# DEPRECATED ufleprsFieldLevelEncryptionProfile "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufleprsResponseStatus :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse Lude.Int
ufleprsResponseStatus = Lens.lens (responseStatus :: UpdateFieldLevelEncryptionProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFieldLevelEncryptionProfileResponse)
{-# DEPRECATED ufleprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
