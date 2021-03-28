{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
  ( FieldLevelEncryptionProfileConfig (..)
  -- * Smart constructor
  , mkFieldLevelEncryptionProfileConfig
  -- * Lenses
  , flepcName
  , flepcCallerReference
  , flepcEncryptionEntities
  , flepcComment
  ) where

import qualified Network.AWS.CloudFront.Types.EncryptionEntities as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex data type of profiles for the field-level encryption.
--
-- /See:/ 'mkFieldLevelEncryptionProfileConfig' smart constructor.
data FieldLevelEncryptionProfileConfig = FieldLevelEncryptionProfileConfig'
  { name :: Core.Text
    -- ^ Profile name for the field-level encryption profile.
  , callerReference :: Core.Text
    -- ^ A unique number that ensures that the request can't be replayed.
  , encryptionEntities :: Types.EncryptionEntities
    -- ^ A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
  , comment :: Core.Maybe Core.Text
    -- ^ An optional comment for the field-level encryption profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FieldLevelEncryptionProfileConfig' value with any optional fields omitted.
mkFieldLevelEncryptionProfileConfig
    :: Core.Text -- ^ 'name'
    -> Core.Text -- ^ 'callerReference'
    -> Types.EncryptionEntities -- ^ 'encryptionEntities'
    -> FieldLevelEncryptionProfileConfig
mkFieldLevelEncryptionProfileConfig name callerReference
  encryptionEntities
  = FieldLevelEncryptionProfileConfig'{name, callerReference,
                                       encryptionEntities, comment = Core.Nothing}

-- | Profile name for the field-level encryption profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcName :: Lens.Lens' FieldLevelEncryptionProfileConfig Core.Text
flepcName = Lens.field @"name"
{-# INLINEABLE flepcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique number that ensures that the request can't be replayed.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcCallerReference :: Lens.Lens' FieldLevelEncryptionProfileConfig Core.Text
flepcCallerReference = Lens.field @"callerReference"
{-# INLINEABLE flepcCallerReference #-}
{-# DEPRECATED callerReference "Use generic-lens or generic-optics with 'callerReference' instead"  #-}

-- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
--
-- /Note:/ Consider using 'encryptionEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcEncryptionEntities :: Lens.Lens' FieldLevelEncryptionProfileConfig Types.EncryptionEntities
flepcEncryptionEntities = Lens.field @"encryptionEntities"
{-# INLINEABLE flepcEncryptionEntities #-}
{-# DEPRECATED encryptionEntities "Use generic-lens or generic-optics with 'encryptionEntities' instead"  #-}

-- | An optional comment for the field-level encryption profile.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flepcComment :: Lens.Lens' FieldLevelEncryptionProfileConfig (Core.Maybe Core.Text)
flepcComment = Lens.field @"comment"
{-# INLINEABLE flepcComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.ToXML FieldLevelEncryptionProfileConfig where
        toXML FieldLevelEncryptionProfileConfig{..}
          = Core.toXMLElement "Name" name Core.<>
              Core.toXMLElement "CallerReference" callerReference
              Core.<> Core.toXMLElement "EncryptionEntities" encryptionEntities
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Comment") comment

instance Core.FromXML FieldLevelEncryptionProfileConfig where
        parseXML x
          = FieldLevelEncryptionProfileConfig' Core.<$>
              (x Core..@ "Name") Core.<*> x Core..@ "CallerReference" Core.<*>
                x Core..@ "EncryptionEntities"
                Core.<*> x Core..@? "Comment"
