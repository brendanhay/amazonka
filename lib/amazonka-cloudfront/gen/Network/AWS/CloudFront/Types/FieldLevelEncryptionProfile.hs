{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfile
  ( FieldLevelEncryptionProfile (..),

    -- * Smart constructor
    mkFieldLevelEncryptionProfile,

    -- * Lenses
    flepFieldLevelEncryptionProfileConfig,
    flepLastModifiedTime,
    flepId,
  )
where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex data type for field-level encryption profiles.
--
-- /See:/ 'mkFieldLevelEncryptionProfile' smart constructor.
data FieldLevelEncryptionProfile = FieldLevelEncryptionProfile'
  { -- | A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
    fieldLevelEncryptionProfileConfig :: FieldLevelEncryptionProfileConfig,
    -- | The last time the field-level encryption profile was updated.
    lastModifiedTime :: Lude.DateTime,
    -- | The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- * 'fieldLevelEncryptionProfileConfig' - A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
-- * 'lastModifiedTime' - The last time the field-level encryption profile was updated.
-- * 'id' - The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
mkFieldLevelEncryptionProfile ::
  -- | 'fieldLevelEncryptionProfileConfig'
  FieldLevelEncryptionProfileConfig ->
  -- | 'lastModifiedTime'
  Lude.DateTime ->
  -- | 'id'
  Lude.Text ->
  FieldLevelEncryptionProfile
mkFieldLevelEncryptionProfile
  pFieldLevelEncryptionProfileConfig_
  pLastModifiedTime_
  pId_ =
    FieldLevelEncryptionProfile'
      { fieldLevelEncryptionProfileConfig =
          pFieldLevelEncryptionProfileConfig_,
        lastModifiedTime = pLastModifiedTime_,
        id = pId_
      }

-- | A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepFieldLevelEncryptionProfileConfig :: Lens.Lens' FieldLevelEncryptionProfile FieldLevelEncryptionProfileConfig
flepFieldLevelEncryptionProfileConfig = Lens.lens (fieldLevelEncryptionProfileConfig :: FieldLevelEncryptionProfile -> FieldLevelEncryptionProfileConfig) (\s a -> s {fieldLevelEncryptionProfileConfig = a} :: FieldLevelEncryptionProfile)
{-# DEPRECATED flepFieldLevelEncryptionProfileConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileConfig' instead." #-}

-- | The last time the field-level encryption profile was updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepLastModifiedTime :: Lens.Lens' FieldLevelEncryptionProfile Lude.DateTime
flepLastModifiedTime = Lens.lens (lastModifiedTime :: FieldLevelEncryptionProfile -> Lude.DateTime) (\s a -> s {lastModifiedTime = a} :: FieldLevelEncryptionProfile)
{-# DEPRECATED flepLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepId :: Lens.Lens' FieldLevelEncryptionProfile Lude.Text
flepId = Lens.lens (id :: FieldLevelEncryptionProfile -> Lude.Text) (\s a -> s {id = a} :: FieldLevelEncryptionProfile)
{-# DEPRECATED flepId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML FieldLevelEncryptionProfile where
  parseXML x =
    FieldLevelEncryptionProfile'
      Lude.<$> (x Lude..@ "FieldLevelEncryptionProfileConfig")
      Lude.<*> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "Id")
