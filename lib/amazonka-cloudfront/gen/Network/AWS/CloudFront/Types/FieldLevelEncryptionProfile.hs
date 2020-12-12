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
    flepId,
    flepLastModifiedTime,
    flepFieldLevelEncryptionProfileConfig,
  )
where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex data type for field-level encryption profiles.
--
-- /See:/ 'mkFieldLevelEncryptionProfile' smart constructor.
data FieldLevelEncryptionProfile = FieldLevelEncryptionProfile'
  { id ::
      Lude.Text,
    lastModifiedTime :: Lude.DateTime,
    fieldLevelEncryptionProfileConfig ::
      FieldLevelEncryptionProfileConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- * 'fieldLevelEncryptionProfileConfig' - A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
-- * 'id' - The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
-- * 'lastModifiedTime' - The last time the field-level encryption profile was updated.
mkFieldLevelEncryptionProfile ::
  -- | 'id'
  Lude.Text ->
  -- | 'lastModifiedTime'
  Lude.DateTime ->
  -- | 'fieldLevelEncryptionProfileConfig'
  FieldLevelEncryptionProfileConfig ->
  FieldLevelEncryptionProfile
mkFieldLevelEncryptionProfile
  pId_
  pLastModifiedTime_
  pFieldLevelEncryptionProfileConfig_ =
    FieldLevelEncryptionProfile'
      { id = pId_,
        lastModifiedTime = pLastModifiedTime_,
        fieldLevelEncryptionProfileConfig =
          pFieldLevelEncryptionProfileConfig_
      }

-- | The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepId :: Lens.Lens' FieldLevelEncryptionProfile Lude.Text
flepId = Lens.lens (id :: FieldLevelEncryptionProfile -> Lude.Text) (\s a -> s {id = a} :: FieldLevelEncryptionProfile)
{-# DEPRECATED flepId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The last time the field-level encryption profile was updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepLastModifiedTime :: Lens.Lens' FieldLevelEncryptionProfile Lude.DateTime
flepLastModifiedTime = Lens.lens (lastModifiedTime :: FieldLevelEncryptionProfile -> Lude.DateTime) (\s a -> s {lastModifiedTime = a} :: FieldLevelEncryptionProfile)
{-# DEPRECATED flepLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepFieldLevelEncryptionProfileConfig :: Lens.Lens' FieldLevelEncryptionProfile FieldLevelEncryptionProfileConfig
flepFieldLevelEncryptionProfileConfig = Lens.lens (fieldLevelEncryptionProfileConfig :: FieldLevelEncryptionProfile -> FieldLevelEncryptionProfileConfig) (\s a -> s {fieldLevelEncryptionProfileConfig = a} :: FieldLevelEncryptionProfile)
{-# DEPRECATED flepFieldLevelEncryptionProfileConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileConfig' instead." #-}

instance Lude.FromXML FieldLevelEncryptionProfile where
  parseXML x =
    FieldLevelEncryptionProfile'
      Lude.<$> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "FieldLevelEncryptionProfileConfig")
