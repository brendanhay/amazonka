{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
  ( FieldLevelEncryptionProfileConfig (..),

    -- * Smart constructor
    mkFieldLevelEncryptionProfileConfig,

    -- * Lenses
    flepcName,
    flepcCallerReference,
    flepcEncryptionEntities,
    flepcComment,
  )
where

import qualified Network.AWS.CloudFront.Types.CallerReference as Types
import qualified Network.AWS.CloudFront.Types.Comment as Types
import qualified Network.AWS.CloudFront.Types.EncryptionEntities as Types
import qualified Network.AWS.CloudFront.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex data type of profiles for the field-level encryption.
--
-- /See:/ 'mkFieldLevelEncryptionProfileConfig' smart constructor.
data FieldLevelEncryptionProfileConfig = FieldLevelEncryptionProfileConfig'
  { -- | Profile name for the field-level encryption profile.
    name :: Types.Name,
    -- | A unique number that ensures that the request can't be replayed.
    callerReference :: Types.CallerReference,
    -- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
    encryptionEntities :: Types.EncryptionEntities,
    -- | An optional comment for the field-level encryption profile.
    comment :: Core.Maybe Types.Comment
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FieldLevelEncryptionProfileConfig' value with any optional fields omitted.
mkFieldLevelEncryptionProfileConfig ::
  -- | 'name'
  Types.Name ->
  -- | 'callerReference'
  Types.CallerReference ->
  -- | 'encryptionEntities'
  Types.EncryptionEntities ->
  FieldLevelEncryptionProfileConfig
mkFieldLevelEncryptionProfileConfig
  name
  callerReference
  encryptionEntities =
    FieldLevelEncryptionProfileConfig'
      { name,
        callerReference,
        encryptionEntities,
        comment = Core.Nothing
      }

-- | Profile name for the field-level encryption profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcName :: Lens.Lens' FieldLevelEncryptionProfileConfig Types.Name
flepcName = Lens.field @"name"
{-# DEPRECATED flepcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique number that ensures that the request can't be replayed.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcCallerReference :: Lens.Lens' FieldLevelEncryptionProfileConfig Types.CallerReference
flepcCallerReference = Lens.field @"callerReference"
{-# DEPRECATED flepcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
--
-- /Note:/ Consider using 'encryptionEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcEncryptionEntities :: Lens.Lens' FieldLevelEncryptionProfileConfig Types.EncryptionEntities
flepcEncryptionEntities = Lens.field @"encryptionEntities"
{-# DEPRECATED flepcEncryptionEntities "Use generic-lens or generic-optics with 'encryptionEntities' instead." #-}

-- | An optional comment for the field-level encryption profile.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcComment :: Lens.Lens' FieldLevelEncryptionProfileConfig (Core.Maybe Types.Comment)
flepcComment = Lens.field @"comment"
{-# DEPRECATED flepcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.ToXML FieldLevelEncryptionProfileConfig where
  toXML FieldLevelEncryptionProfileConfig {..} =
    Core.toXMLNode "Name" name
      Core.<> Core.toXMLNode "CallerReference" callerReference
      Core.<> Core.toXMLNode "EncryptionEntities" encryptionEntities
      Core.<> Core.toXMLNode "Comment" Core.<$> comment

instance Core.FromXML FieldLevelEncryptionProfileConfig where
  parseXML x =
    FieldLevelEncryptionProfileConfig'
      Core.<$> (x Core..@ "Name")
      Core.<*> (x Core..@ "CallerReference")
      Core.<*> (x Core..@ "EncryptionEntities")
      Core.<*> (x Core..@? "Comment")
