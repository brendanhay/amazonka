-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.S3Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3Encryption
  ( S3Encryption (..),

    -- * Smart constructor
    mkS3Encryption,

    -- * Lenses
    seS3EncryptionMode,
    seKMSKeyARN,
  )
where

import Network.AWS.Glue.Types.S3EncryptionMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies how Amazon Simple Storage Service (Amazon S3) data should be encrypted.
--
-- /See:/ 'mkS3Encryption' smart constructor.
data S3Encryption = S3Encryption'
  { s3EncryptionMode ::
      Lude.Maybe S3EncryptionMode,
    kmsKeyARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Encryption' with the minimum fields required to make a request.
--
-- * 'kmsKeyARN' - The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
-- * 's3EncryptionMode' - The encryption mode to use for Amazon S3 data.
mkS3Encryption ::
  S3Encryption
mkS3Encryption =
  S3Encryption'
    { s3EncryptionMode = Lude.Nothing,
      kmsKeyARN = Lude.Nothing
    }

-- | The encryption mode to use for Amazon S3 data.
--
-- /Note:/ Consider using 's3EncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seS3EncryptionMode :: Lens.Lens' S3Encryption (Lude.Maybe S3EncryptionMode)
seS3EncryptionMode = Lens.lens (s3EncryptionMode :: S3Encryption -> Lude.Maybe S3EncryptionMode) (\s a -> s {s3EncryptionMode = a} :: S3Encryption)
{-# DEPRECATED seS3EncryptionMode "Use generic-lens or generic-optics with 's3EncryptionMode' instead." #-}

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seKMSKeyARN :: Lens.Lens' S3Encryption (Lude.Maybe Lude.Text)
seKMSKeyARN = Lens.lens (kmsKeyARN :: S3Encryption -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: S3Encryption)
{-# DEPRECATED seKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

instance Lude.FromJSON S3Encryption where
  parseJSON =
    Lude.withObject
      "S3Encryption"
      ( \x ->
          S3Encryption'
            Lude.<$> (x Lude..:? "S3EncryptionMode") Lude.<*> (x Lude..:? "KmsKeyArn")
      )

instance Lude.ToJSON S3Encryption where
  toJSON S3Encryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3EncryptionMode" Lude..=) Lude.<$> s3EncryptionMode,
            ("KmsKeyArn" Lude..=) Lude.<$> kmsKeyARN
          ]
      )
