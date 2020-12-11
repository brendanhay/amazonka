{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption profile information.
module Network.AWS.CloudFront.GetFieldLevelEncryptionProfile
  ( -- * Creating a request
    GetFieldLevelEncryptionProfile (..),
    mkGetFieldLevelEncryptionProfile,

    -- ** Request lenses
    gflepId,

    -- * Destructuring the response
    GetFieldLevelEncryptionProfileResponse (..),
    mkGetFieldLevelEncryptionProfileResponse,

    -- ** Response lenses
    gfleprsETag,
    gfleprsFieldLevelEncryptionProfile,
    gfleprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFieldLevelEncryptionProfile' smart constructor.
newtype GetFieldLevelEncryptionProfile = GetFieldLevelEncryptionProfile'
  { id ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- * 'id' - Get the ID for the field-level encryption profile information.
mkGetFieldLevelEncryptionProfile ::
  -- | 'id'
  Lude.Text ->
  GetFieldLevelEncryptionProfile
mkGetFieldLevelEncryptionProfile pId_ =
  GetFieldLevelEncryptionProfile' {id = pId_}

-- | Get the ID for the field-level encryption profile information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflepId :: Lens.Lens' GetFieldLevelEncryptionProfile Lude.Text
gflepId = Lens.lens (id :: GetFieldLevelEncryptionProfile -> Lude.Text) (\s a -> s {id = a} :: GetFieldLevelEncryptionProfile)
{-# DEPRECATED gflepId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetFieldLevelEncryptionProfile where
  type
    Rs GetFieldLevelEncryptionProfile =
      GetFieldLevelEncryptionProfileResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionProfileResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFieldLevelEncryptionProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetFieldLevelEncryptionProfile where
  toPath GetFieldLevelEncryptionProfile' {..} =
    Lude.mconcat
      ["/2020-05-31/field-level-encryption-profile/", Lude.toBS id]

instance Lude.ToQuery GetFieldLevelEncryptionProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFieldLevelEncryptionProfileResponse' smart constructor.
data GetFieldLevelEncryptionProfileResponse = GetFieldLevelEncryptionProfileResponse'
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

-- | Creates a value of 'GetFieldLevelEncryptionProfileResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
-- * 'fieldLevelEncryptionProfile' - Return the field-level encryption profile information.
-- * 'responseStatus' - The response status code.
mkGetFieldLevelEncryptionProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFieldLevelEncryptionProfileResponse
mkGetFieldLevelEncryptionProfileResponse pResponseStatus_ =
  GetFieldLevelEncryptionProfileResponse'
    { eTag = Lude.Nothing,
      fieldLevelEncryptionProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfleprsETag :: Lens.Lens' GetFieldLevelEncryptionProfileResponse (Lude.Maybe Lude.Text)
gfleprsETag = Lens.lens (eTag :: GetFieldLevelEncryptionProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetFieldLevelEncryptionProfileResponse)
{-# DEPRECATED gfleprsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Return the field-level encryption profile information.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfleprsFieldLevelEncryptionProfile :: Lens.Lens' GetFieldLevelEncryptionProfileResponse (Lude.Maybe FieldLevelEncryptionProfile)
gfleprsFieldLevelEncryptionProfile = Lens.lens (fieldLevelEncryptionProfile :: GetFieldLevelEncryptionProfileResponse -> Lude.Maybe FieldLevelEncryptionProfile) (\s a -> s {fieldLevelEncryptionProfile = a} :: GetFieldLevelEncryptionProfileResponse)
{-# DEPRECATED gfleprsFieldLevelEncryptionProfile "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfleprsResponseStatus :: Lens.Lens' GetFieldLevelEncryptionProfileResponse Lude.Int
gfleprsResponseStatus = Lens.lens (responseStatus :: GetFieldLevelEncryptionProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFieldLevelEncryptionProfileResponse)
{-# DEPRECATED gfleprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
