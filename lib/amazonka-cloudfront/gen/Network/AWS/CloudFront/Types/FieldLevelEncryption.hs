{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryption
  ( FieldLevelEncryption (..),

    -- * Smart constructor
    mkFieldLevelEncryption,

    -- * Lenses
    fleId,
    fleLastModifiedTime,
    fleFieldLevelEncryptionConfig,
  )
where

import qualified Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex data type that includes the profile configurations and other options specified for field-level encryption.
--
-- /See:/ 'mkFieldLevelEncryption' smart constructor.
data FieldLevelEncryption = FieldLevelEncryption'
  { -- | The configuration ID for a field-level encryption configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
    id :: Types.String,
    -- | The last time the field-level encryption configuration was changed.
    lastModifiedTime :: Core.UTCTime,
    -- | A complex data type that includes the profile configurations specified for field-level encryption.
    fieldLevelEncryptionConfig :: Types.FieldLevelEncryptionConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FieldLevelEncryption' value with any optional fields omitted.
mkFieldLevelEncryption ::
  -- | 'id'
  Types.String ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'fieldLevelEncryptionConfig'
  Types.FieldLevelEncryptionConfig ->
  FieldLevelEncryption
mkFieldLevelEncryption
  id
  lastModifiedTime
  fieldLevelEncryptionConfig =
    FieldLevelEncryption'
      { id,
        lastModifiedTime,
        fieldLevelEncryptionConfig
      }

-- | The configuration ID for a field-level encryption configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleId :: Lens.Lens' FieldLevelEncryption Types.String
fleId = Lens.field @"id"
{-# DEPRECATED fleId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The last time the field-level encryption configuration was changed.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleLastModifiedTime :: Lens.Lens' FieldLevelEncryption Core.UTCTime
fleLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED fleLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | A complex data type that includes the profile configurations specified for field-level encryption.
--
-- /Note:/ Consider using 'fieldLevelEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleFieldLevelEncryptionConfig :: Lens.Lens' FieldLevelEncryption Types.FieldLevelEncryptionConfig
fleFieldLevelEncryptionConfig = Lens.field @"fieldLevelEncryptionConfig"
{-# DEPRECATED fleFieldLevelEncryptionConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionConfig' instead." #-}

instance Core.FromXML FieldLevelEncryption where
  parseXML x =
    FieldLevelEncryption'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "LastModifiedTime")
      Core.<*> (x Core..@ "FieldLevelEncryptionConfig")
