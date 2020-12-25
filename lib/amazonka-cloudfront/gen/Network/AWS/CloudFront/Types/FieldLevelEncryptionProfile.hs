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

import qualified Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex data type for field-level encryption profiles.
--
-- /See:/ 'mkFieldLevelEncryptionProfile' smart constructor.
data FieldLevelEncryptionProfile = FieldLevelEncryptionProfile'
  { -- | The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
    id :: Types.String,
    -- | The last time the field-level encryption profile was updated.
    lastModifiedTime :: Core.UTCTime,
    -- | A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
    fieldLevelEncryptionProfileConfig :: Types.FieldLevelEncryptionProfileConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FieldLevelEncryptionProfile' value with any optional fields omitted.
mkFieldLevelEncryptionProfile ::
  -- | 'id'
  Types.String ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'fieldLevelEncryptionProfileConfig'
  Types.FieldLevelEncryptionProfileConfig ->
  FieldLevelEncryptionProfile
mkFieldLevelEncryptionProfile
  id
  lastModifiedTime
  fieldLevelEncryptionProfileConfig =
    FieldLevelEncryptionProfile'
      { id,
        lastModifiedTime,
        fieldLevelEncryptionProfileConfig
      }

-- | The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepId :: Lens.Lens' FieldLevelEncryptionProfile Types.String
flepId = Lens.field @"id"
{-# DEPRECATED flepId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The last time the field-level encryption profile was updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepLastModifiedTime :: Lens.Lens' FieldLevelEncryptionProfile Core.UTCTime
flepLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED flepLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepFieldLevelEncryptionProfileConfig :: Lens.Lens' FieldLevelEncryptionProfile Types.FieldLevelEncryptionProfileConfig
flepFieldLevelEncryptionProfileConfig = Lens.field @"fieldLevelEncryptionProfileConfig"
{-# DEPRECATED flepFieldLevelEncryptionProfileConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileConfig' instead." #-}

instance Core.FromXML FieldLevelEncryptionProfile where
  parseXML x =
    FieldLevelEncryptionProfile'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "LastModifiedTime")
      Core.<*> (x Core..@ "FieldLevelEncryptionProfileConfig")
