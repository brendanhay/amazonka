{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Encryption
  ( Encryption (..),

    -- * Smart constructor
    mkEncryption,

    -- * Lenses
    eMode,
    eKeyMD5,
    eKey,
    eInitializationVector,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The encryption settings, if any, that are used for decrypting your input files or encrypting your output files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file, otherwise you must specify the mode you want Elastic Transcoder to use to encrypt your output files.
--
-- /See:/ 'mkEncryption' smart constructor.
data Encryption = Encryption'
  { mode :: Lude.Maybe Lude.Text,
    keyMD5 :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text,
    initializationVector :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Encryption' with the minimum fields required to make a request.
--
-- * 'initializationVector' - The series of random bits created by a random bit generator, unique for every encryption operation, that you used to encrypt your input files or that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
-- * 'key' - The data encryption key that you want Elastic Transcoder to use to encrypt your output file, or that was used to encrypt your input file. The key must be base64-encoded and it must be one of the following bit lengths before being base64-encoded:
--
-- @128@ , @192@ , or @256@ .
-- The key must also be encrypted by using the Amazon Key Management Service.
-- * 'keyMD5' - The MD5 digest of the key that you used to encrypt your input file, or that you want Elastic Transcoder to use to encrypt your output file. Elastic Transcoder uses the key digest as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
-- * 'mode' - The specific server-side encryption mode that you want Elastic Transcoder to use when decrypting your input files or encrypting your output files. Elastic Transcoder supports the following options:
--
--
--     * __s3:__ Amazon S3 creates and manages the keys used for encrypting your files.
--
--
--     * __s3-aws-kms:__ Amazon S3 calls the Amazon Key Management Service, which creates and manages the keys that are used for encrypting your files. If you specify @s3-aws-kms@ and you don't want to use the default key, you must add the AWS-KMS key that you want to use to your pipeline.
--
--
--     * __aes-cbc-pkcs7:__ A padded cipher-block mode of operation originally used for HLS files.
--
--
--     * __aes-ctr:__ AES Counter Mode.
--
--
--     * __aes-gcm:__ AES Galois Counter Mode, a mode of operation that is an authenticated encryption format, meaning that a file, key, or initialization vector that has been tampered with fails the decryption process.
--
--
-- For all three AES options, you must provide the following settings, which must be base64-encoded:
--
--     * __Key__
--
--
--     * __Key MD5__
--
--
--     * __Initialization Vector__
--
--
-- /Important:/ For the AES modes, your private encryption keys and your unencrypted data are never stored by AWS; therefore, it is important that you safely manage your encryption keys. If you lose them, you won't be able to unencrypt your data.
mkEncryption ::
  Encryption
mkEncryption =
  Encryption'
    { mode = Lude.Nothing,
      keyMD5 = Lude.Nothing,
      key = Lude.Nothing,
      initializationVector = Lude.Nothing
    }

-- | The specific server-side encryption mode that you want Elastic Transcoder to use when decrypting your input files or encrypting your output files. Elastic Transcoder supports the following options:
--
--
--     * __s3:__ Amazon S3 creates and manages the keys used for encrypting your files.
--
--
--     * __s3-aws-kms:__ Amazon S3 calls the Amazon Key Management Service, which creates and manages the keys that are used for encrypting your files. If you specify @s3-aws-kms@ and you don't want to use the default key, you must add the AWS-KMS key that you want to use to your pipeline.
--
--
--     * __aes-cbc-pkcs7:__ A padded cipher-block mode of operation originally used for HLS files.
--
--
--     * __aes-ctr:__ AES Counter Mode.
--
--
--     * __aes-gcm:__ AES Galois Counter Mode, a mode of operation that is an authenticated encryption format, meaning that a file, key, or initialization vector that has been tampered with fails the decryption process.
--
--
-- For all three AES options, you must provide the following settings, which must be base64-encoded:
--
--     * __Key__
--
--
--     * __Key MD5__
--
--
--     * __Initialization Vector__
--
--
-- /Important:/ For the AES modes, your private encryption keys and your unencrypted data are never stored by AWS; therefore, it is important that you safely manage your encryption keys. If you lose them, you won't be able to unencrypt your data.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMode :: Lens.Lens' Encryption (Lude.Maybe Lude.Text)
eMode = Lens.lens (mode :: Encryption -> Lude.Maybe Lude.Text) (\s a -> s {mode = a} :: Encryption)
{-# DEPRECATED eMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The MD5 digest of the key that you used to encrypt your input file, or that you want Elastic Transcoder to use to encrypt your output file. Elastic Transcoder uses the key digest as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
--
-- /Note:/ Consider using 'keyMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKeyMD5 :: Lens.Lens' Encryption (Lude.Maybe Lude.Text)
eKeyMD5 = Lens.lens (keyMD5 :: Encryption -> Lude.Maybe Lude.Text) (\s a -> s {keyMD5 = a} :: Encryption)
{-# DEPRECATED eKeyMD5 "Use generic-lens or generic-optics with 'keyMD5' instead." #-}

-- | The data encryption key that you want Elastic Transcoder to use to encrypt your output file, or that was used to encrypt your input file. The key must be base64-encoded and it must be one of the following bit lengths before being base64-encoded:
--
-- @128@ , @192@ , or @256@ .
-- The key must also be encrypted by using the Amazon Key Management Service.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKey :: Lens.Lens' Encryption (Lude.Maybe Lude.Text)
eKey = Lens.lens (key :: Encryption -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: Encryption)
{-# DEPRECATED eKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The series of random bits created by a random bit generator, unique for every encryption operation, that you used to encrypt your input files or that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
--
-- /Note:/ Consider using 'initializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eInitializationVector :: Lens.Lens' Encryption (Lude.Maybe Lude.Text)
eInitializationVector = Lens.lens (initializationVector :: Encryption -> Lude.Maybe Lude.Text) (\s a -> s {initializationVector = a} :: Encryption)
{-# DEPRECATED eInitializationVector "Use generic-lens or generic-optics with 'initializationVector' instead." #-}

instance Lude.FromJSON Encryption where
  parseJSON =
    Lude.withObject
      "Encryption"
      ( \x ->
          Encryption'
            Lude.<$> (x Lude..:? "Mode")
            Lude.<*> (x Lude..:? "KeyMd5")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "InitializationVector")
      )

instance Lude.ToJSON Encryption where
  toJSON Encryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Mode" Lude..=) Lude.<$> mode,
            ("KeyMd5" Lude..=) Lude.<$> keyMD5,
            ("Key" Lude..=) Lude.<$> key,
            ("InitializationVector" Lude..=) Lude.<$> initializationVector
          ]
      )
