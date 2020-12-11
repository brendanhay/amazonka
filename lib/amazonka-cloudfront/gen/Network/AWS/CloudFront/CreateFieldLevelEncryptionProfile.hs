{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a field-level encryption profile.
module Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
  ( -- * Creating a request
    CreateFieldLevelEncryptionProfile (..),
    mkCreateFieldLevelEncryptionProfile,

    -- ** Request lenses
    cflepFieldLevelEncryptionProfileConfig,

    -- * Destructuring the response
    CreateFieldLevelEncryptionProfileResponse (..),
    mkCreateFieldLevelEncryptionProfileResponse,

    -- ** Response lenses
    cfleprsETag,
    cfleprsLocation,
    cfleprsFieldLevelEncryptionProfile,
    cfleprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFieldLevelEncryptionProfile' smart constructor.
newtype CreateFieldLevelEncryptionProfile = CreateFieldLevelEncryptionProfile'
  { fieldLevelEncryptionProfileConfig ::
      FieldLevelEncryptionProfileConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- * 'fieldLevelEncryptionProfileConfig' - The request to create a field-level encryption profile.
mkCreateFieldLevelEncryptionProfile ::
  -- | 'fieldLevelEncryptionProfileConfig'
  FieldLevelEncryptionProfileConfig ->
  CreateFieldLevelEncryptionProfile
mkCreateFieldLevelEncryptionProfile
  pFieldLevelEncryptionProfileConfig_ =
    CreateFieldLevelEncryptionProfile'
      { fieldLevelEncryptionProfileConfig =
          pFieldLevelEncryptionProfileConfig_
      }

-- | The request to create a field-level encryption profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflepFieldLevelEncryptionProfileConfig :: Lens.Lens' CreateFieldLevelEncryptionProfile FieldLevelEncryptionProfileConfig
cflepFieldLevelEncryptionProfileConfig = Lens.lens (fieldLevelEncryptionProfileConfig :: CreateFieldLevelEncryptionProfile -> FieldLevelEncryptionProfileConfig) (\s a -> s {fieldLevelEncryptionProfileConfig = a} :: CreateFieldLevelEncryptionProfile)
{-# DEPRECATED cflepFieldLevelEncryptionProfileConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileConfig' instead." #-}

instance Lude.AWSRequest CreateFieldLevelEncryptionProfile where
  type
    Rs CreateFieldLevelEncryptionProfile =
      CreateFieldLevelEncryptionProfileResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateFieldLevelEncryptionProfileResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateFieldLevelEncryptionProfile where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}FieldLevelEncryptionProfileConfig"
      Lude.. fieldLevelEncryptionProfileConfig

instance Lude.ToHeaders CreateFieldLevelEncryptionProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateFieldLevelEncryptionProfile where
  toPath = Lude.const "/2020-05-31/field-level-encryption-profile"

instance Lude.ToQuery CreateFieldLevelEncryptionProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateFieldLevelEncryptionProfileResponse' smart constructor.
data CreateFieldLevelEncryptionProfileResponse = CreateFieldLevelEncryptionProfileResponse'
  { eTag ::
      Lude.Maybe
        Lude.Text,
    location ::
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

-- | Creates a value of 'CreateFieldLevelEncryptionProfileResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
-- * 'fieldLevelEncryptionProfile' - Returned when you create a new field-level encryption profile.
-- * 'location' - The fully qualified URI of the new profile resource just created.
-- * 'responseStatus' - The response status code.
mkCreateFieldLevelEncryptionProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFieldLevelEncryptionProfileResponse
mkCreateFieldLevelEncryptionProfileResponse pResponseStatus_ =
  CreateFieldLevelEncryptionProfileResponse'
    { eTag = Lude.Nothing,
      location = Lude.Nothing,
      fieldLevelEncryptionProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfleprsETag :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Lude.Maybe Lude.Text)
cfleprsETag = Lens.lens (eTag :: CreateFieldLevelEncryptionProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreateFieldLevelEncryptionProfileResponse)
{-# DEPRECATED cfleprsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the new profile resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfleprsLocation :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Lude.Maybe Lude.Text)
cfleprsLocation = Lens.lens (location :: CreateFieldLevelEncryptionProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateFieldLevelEncryptionProfileResponse)
{-# DEPRECATED cfleprsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | Returned when you create a new field-level encryption profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfleprsFieldLevelEncryptionProfile :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Lude.Maybe FieldLevelEncryptionProfile)
cfleprsFieldLevelEncryptionProfile = Lens.lens (fieldLevelEncryptionProfile :: CreateFieldLevelEncryptionProfileResponse -> Lude.Maybe FieldLevelEncryptionProfile) (\s a -> s {fieldLevelEncryptionProfile = a} :: CreateFieldLevelEncryptionProfileResponse)
{-# DEPRECATED cfleprsFieldLevelEncryptionProfile "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfleprsResponseStatus :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse Lude.Int
cfleprsResponseStatus = Lens.lens (responseStatus :: CreateFieldLevelEncryptionProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFieldLevelEncryptionProfileResponse)
{-# DEPRECATED cfleprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
