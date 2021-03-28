{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputDecryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.InputDecryptionSettings
  ( InputDecryptionSettings (..)
  -- * Smart constructor
  , mkInputDecryptionSettings
  -- * Lenses
  , idsDecryptionMode
  , idsEncryptedDecryptionKey
  , idsInitializationVector
  , idsKmsKeyRegion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.DecryptionMode as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for decrypting any input files that you encrypt before you upload them to Amazon S3. MediaConvert can decrypt files only when you use AWS Key Management Service (KMS) to encrypt the data key that you use to encrypt your content.
--
-- /See:/ 'mkInputDecryptionSettings' smart constructor.
data InputDecryptionSettings = InputDecryptionSettings'
  { decryptionMode :: Core.Maybe Types.DecryptionMode
    -- ^ Specify the encryption mode that you used to encrypt your input files.
  , encryptedDecryptionKey :: Core.Maybe Core.Text
    -- ^ Warning! Don't provide your encryption key in plaintext. Your job settings could be intercepted, making your encrypted content vulnerable. Specify the encrypted version of the data key that you used to encrypt your content. The data key must be encrypted by AWS Key Management Service (KMS). The key can be 128, 192, or 256 bits.
  , initializationVector :: Core.Maybe Core.Text
    -- ^ Specify the initialization vector that you used when you encrypted your content before uploading it to Amazon S3. You can use a 16-byte initialization vector with any encryption mode. Or, you can use a 12-byte initialization vector with GCM or CTR. MediaConvert accepts only initialization vectors that are base64-encoded.
  , kmsKeyRegion :: Core.Maybe Core.Text
    -- ^ Specify the AWS Region for AWS Key Management Service (KMS) that you used to encrypt your data key, if that Region is different from the one you are using for AWS Elemental MediaConvert.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputDecryptionSettings' value with any optional fields omitted.
mkInputDecryptionSettings
    :: InputDecryptionSettings
mkInputDecryptionSettings
  = InputDecryptionSettings'{decryptionMode = Core.Nothing,
                             encryptedDecryptionKey = Core.Nothing,
                             initializationVector = Core.Nothing, kmsKeyRegion = Core.Nothing}

-- | Specify the encryption mode that you used to encrypt your input files.
--
-- /Note:/ Consider using 'decryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsDecryptionMode :: Lens.Lens' InputDecryptionSettings (Core.Maybe Types.DecryptionMode)
idsDecryptionMode = Lens.field @"decryptionMode"
{-# INLINEABLE idsDecryptionMode #-}
{-# DEPRECATED decryptionMode "Use generic-lens or generic-optics with 'decryptionMode' instead"  #-}

-- | Warning! Don't provide your encryption key in plaintext. Your job settings could be intercepted, making your encrypted content vulnerable. Specify the encrypted version of the data key that you used to encrypt your content. The data key must be encrypted by AWS Key Management Service (KMS). The key can be 128, 192, or 256 bits.
--
-- /Note:/ Consider using 'encryptedDecryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsEncryptedDecryptionKey :: Lens.Lens' InputDecryptionSettings (Core.Maybe Core.Text)
idsEncryptedDecryptionKey = Lens.field @"encryptedDecryptionKey"
{-# INLINEABLE idsEncryptedDecryptionKey #-}
{-# DEPRECATED encryptedDecryptionKey "Use generic-lens or generic-optics with 'encryptedDecryptionKey' instead"  #-}

-- | Specify the initialization vector that you used when you encrypted your content before uploading it to Amazon S3. You can use a 16-byte initialization vector with any encryption mode. Or, you can use a 12-byte initialization vector with GCM or CTR. MediaConvert accepts only initialization vectors that are base64-encoded.
--
-- /Note:/ Consider using 'initializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsInitializationVector :: Lens.Lens' InputDecryptionSettings (Core.Maybe Core.Text)
idsInitializationVector = Lens.field @"initializationVector"
{-# INLINEABLE idsInitializationVector #-}
{-# DEPRECATED initializationVector "Use generic-lens or generic-optics with 'initializationVector' instead"  #-}

-- | Specify the AWS Region for AWS Key Management Service (KMS) that you used to encrypt your data key, if that Region is different from the one you are using for AWS Elemental MediaConvert.
--
-- /Note:/ Consider using 'kmsKeyRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsKmsKeyRegion :: Lens.Lens' InputDecryptionSettings (Core.Maybe Core.Text)
idsKmsKeyRegion = Lens.field @"kmsKeyRegion"
{-# INLINEABLE idsKmsKeyRegion #-}
{-# DEPRECATED kmsKeyRegion "Use generic-lens or generic-optics with 'kmsKeyRegion' instead"  #-}

instance Core.FromJSON InputDecryptionSettings where
        toJSON InputDecryptionSettings{..}
          = Core.object
              (Core.catMaybes
                 [("decryptionMode" Core..=) Core.<$> decryptionMode,
                  ("encryptedDecryptionKey" Core..=) Core.<$> encryptedDecryptionKey,
                  ("initializationVector" Core..=) Core.<$> initializationVector,
                  ("kmsKeyRegion" Core..=) Core.<$> kmsKeyRegion])

instance Core.FromJSON InputDecryptionSettings where
        parseJSON
          = Core.withObject "InputDecryptionSettings" Core.$
              \ x ->
                InputDecryptionSettings' Core.<$>
                  (x Core..:? "decryptionMode") Core.<*>
                    x Core..:? "encryptedDecryptionKey"
                    Core.<*> x Core..:? "initializationVector"
                    Core.<*> x Core..:? "kmsKeyRegion"
