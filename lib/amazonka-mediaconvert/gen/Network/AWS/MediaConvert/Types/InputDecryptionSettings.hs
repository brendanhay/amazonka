{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputDecryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputDecryptionSettings
  ( InputDecryptionSettings (..),

    -- * Smart constructor
    mkInputDecryptionSettings,

    -- * Lenses
    idsEncryptedDecryptionKey,
    idsKMSKeyRegion,
    idsDecryptionMode,
    idsInitializationVector,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DecryptionMode
import qualified Network.AWS.Prelude as Lude

-- | Settings for decrypting any input files that you encrypt before you upload them to Amazon S3. MediaConvert can decrypt files only when you use AWS Key Management Service (KMS) to encrypt the data key that you use to encrypt your content.
--
-- /See:/ 'mkInputDecryptionSettings' smart constructor.
data InputDecryptionSettings = InputDecryptionSettings'
  { -- | Warning! Don't provide your encryption key in plaintext. Your job settings could be intercepted, making your encrypted content vulnerable. Specify the encrypted version of the data key that you used to encrypt your content. The data key must be encrypted by AWS Key Management Service (KMS). The key can be 128, 192, or 256 bits.
    encryptedDecryptionKey :: Lude.Maybe Lude.Text,
    -- | Specify the AWS Region for AWS Key Management Service (KMS) that you used to encrypt your data key, if that Region is different from the one you are using for AWS Elemental MediaConvert.
    kmsKeyRegion :: Lude.Maybe Lude.Text,
    -- | Specify the encryption mode that you used to encrypt your input files.
    decryptionMode :: Lude.Maybe DecryptionMode,
    -- | Specify the initialization vector that you used when you encrypted your content before uploading it to Amazon S3. You can use a 16-byte initialization vector with any encryption mode. Or, you can use a 12-byte initialization vector with GCM or CTR. MediaConvert accepts only initialization vectors that are base64-encoded.
    initializationVector :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDecryptionSettings' with the minimum fields required to make a request.
--
-- * 'encryptedDecryptionKey' - Warning! Don't provide your encryption key in plaintext. Your job settings could be intercepted, making your encrypted content vulnerable. Specify the encrypted version of the data key that you used to encrypt your content. The data key must be encrypted by AWS Key Management Service (KMS). The key can be 128, 192, or 256 bits.
-- * 'kmsKeyRegion' - Specify the AWS Region for AWS Key Management Service (KMS) that you used to encrypt your data key, if that Region is different from the one you are using for AWS Elemental MediaConvert.
-- * 'decryptionMode' - Specify the encryption mode that you used to encrypt your input files.
-- * 'initializationVector' - Specify the initialization vector that you used when you encrypted your content before uploading it to Amazon S3. You can use a 16-byte initialization vector with any encryption mode. Or, you can use a 12-byte initialization vector with GCM or CTR. MediaConvert accepts only initialization vectors that are base64-encoded.
mkInputDecryptionSettings ::
  InputDecryptionSettings
mkInputDecryptionSettings =
  InputDecryptionSettings'
    { encryptedDecryptionKey = Lude.Nothing,
      kmsKeyRegion = Lude.Nothing,
      decryptionMode = Lude.Nothing,
      initializationVector = Lude.Nothing
    }

-- | Warning! Don't provide your encryption key in plaintext. Your job settings could be intercepted, making your encrypted content vulnerable. Specify the encrypted version of the data key that you used to encrypt your content. The data key must be encrypted by AWS Key Management Service (KMS). The key can be 128, 192, or 256 bits.
--
-- /Note:/ Consider using 'encryptedDecryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsEncryptedDecryptionKey :: Lens.Lens' InputDecryptionSettings (Lude.Maybe Lude.Text)
idsEncryptedDecryptionKey = Lens.lens (encryptedDecryptionKey :: InputDecryptionSettings -> Lude.Maybe Lude.Text) (\s a -> s {encryptedDecryptionKey = a} :: InputDecryptionSettings)
{-# DEPRECATED idsEncryptedDecryptionKey "Use generic-lens or generic-optics with 'encryptedDecryptionKey' instead." #-}

-- | Specify the AWS Region for AWS Key Management Service (KMS) that you used to encrypt your data key, if that Region is different from the one you are using for AWS Elemental MediaConvert.
--
-- /Note:/ Consider using 'kmsKeyRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsKMSKeyRegion :: Lens.Lens' InputDecryptionSettings (Lude.Maybe Lude.Text)
idsKMSKeyRegion = Lens.lens (kmsKeyRegion :: InputDecryptionSettings -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyRegion = a} :: InputDecryptionSettings)
{-# DEPRECATED idsKMSKeyRegion "Use generic-lens or generic-optics with 'kmsKeyRegion' instead." #-}

-- | Specify the encryption mode that you used to encrypt your input files.
--
-- /Note:/ Consider using 'decryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsDecryptionMode :: Lens.Lens' InputDecryptionSettings (Lude.Maybe DecryptionMode)
idsDecryptionMode = Lens.lens (decryptionMode :: InputDecryptionSettings -> Lude.Maybe DecryptionMode) (\s a -> s {decryptionMode = a} :: InputDecryptionSettings)
{-# DEPRECATED idsDecryptionMode "Use generic-lens or generic-optics with 'decryptionMode' instead." #-}

-- | Specify the initialization vector that you used when you encrypted your content before uploading it to Amazon S3. You can use a 16-byte initialization vector with any encryption mode. Or, you can use a 12-byte initialization vector with GCM or CTR. MediaConvert accepts only initialization vectors that are base64-encoded.
--
-- /Note:/ Consider using 'initializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsInitializationVector :: Lens.Lens' InputDecryptionSettings (Lude.Maybe Lude.Text)
idsInitializationVector = Lens.lens (initializationVector :: InputDecryptionSettings -> Lude.Maybe Lude.Text) (\s a -> s {initializationVector = a} :: InputDecryptionSettings)
{-# DEPRECATED idsInitializationVector "Use generic-lens or generic-optics with 'initializationVector' instead." #-}

instance Lude.FromJSON InputDecryptionSettings where
  parseJSON =
    Lude.withObject
      "InputDecryptionSettings"
      ( \x ->
          InputDecryptionSettings'
            Lude.<$> (x Lude..:? "encryptedDecryptionKey")
            Lude.<*> (x Lude..:? "kmsKeyRegion")
            Lude.<*> (x Lude..:? "decryptionMode")
            Lude.<*> (x Lude..:? "initializationVector")
      )

instance Lude.ToJSON InputDecryptionSettings where
  toJSON InputDecryptionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("encryptedDecryptionKey" Lude..=)
              Lude.<$> encryptedDecryptionKey,
            ("kmsKeyRegion" Lude..=) Lude.<$> kmsKeyRegion,
            ("decryptionMode" Lude..=) Lude.<$> decryptionMode,
            ("initializationVector" Lude..=) Lude.<$> initializationVector
          ]
      )
