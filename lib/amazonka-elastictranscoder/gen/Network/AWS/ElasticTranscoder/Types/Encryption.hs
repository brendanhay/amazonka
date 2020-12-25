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
    eInitializationVector,
    eKey,
    eKeyMd5,
    eMode,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.Base64EncodedString as Types
import qualified Network.AWS.ElasticTranscoder.Types.EncryptionMode as Types
import qualified Network.AWS.ElasticTranscoder.Types.InitializationVector as Types
import qualified Network.AWS.ElasticTranscoder.Types.KeyMd5 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The encryption settings, if any, that are used for decrypting your input files or encrypting your output files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file, otherwise you must specify the mode you want Elastic Transcoder to use to encrypt your output files.
--
-- /See:/ 'mkEncryption' smart constructor.
data Encryption = Encryption'
  { -- | The series of random bits created by a random bit generator, unique for every encryption operation, that you used to encrypt your input files or that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
    initializationVector :: Core.Maybe Types.InitializationVector,
    -- | The data encryption key that you want Elastic Transcoder to use to encrypt your output file, or that was used to encrypt your input file. The key must be base64-encoded and it must be one of the following bit lengths before being base64-encoded:
    --
    -- @128@ , @192@ , or @256@ .
    -- The key must also be encrypted by using the Amazon Key Management Service.
    key :: Core.Maybe Types.Base64EncodedString,
    -- | The MD5 digest of the key that you used to encrypt your input file, or that you want Elastic Transcoder to use to encrypt your output file. Elastic Transcoder uses the key digest as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
    keyMd5 :: Core.Maybe Types.KeyMd5,
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
    mode :: Core.Maybe Types.EncryptionMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Encryption' value with any optional fields omitted.
mkEncryption ::
  Encryption
mkEncryption =
  Encryption'
    { initializationVector = Core.Nothing,
      key = Core.Nothing,
      keyMd5 = Core.Nothing,
      mode = Core.Nothing
    }

-- | The series of random bits created by a random bit generator, unique for every encryption operation, that you used to encrypt your input files or that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
--
-- /Note:/ Consider using 'initializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eInitializationVector :: Lens.Lens' Encryption (Core.Maybe Types.InitializationVector)
eInitializationVector = Lens.field @"initializationVector"
{-# DEPRECATED eInitializationVector "Use generic-lens or generic-optics with 'initializationVector' instead." #-}

-- | The data encryption key that you want Elastic Transcoder to use to encrypt your output file, or that was used to encrypt your input file. The key must be base64-encoded and it must be one of the following bit lengths before being base64-encoded:
--
-- @128@ , @192@ , or @256@ .
-- The key must also be encrypted by using the Amazon Key Management Service.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKey :: Lens.Lens' Encryption (Core.Maybe Types.Base64EncodedString)
eKey = Lens.field @"key"
{-# DEPRECATED eKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The MD5 digest of the key that you used to encrypt your input file, or that you want Elastic Transcoder to use to encrypt your output file. Elastic Transcoder uses the key digest as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
--
-- /Note:/ Consider using 'keyMd5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKeyMd5 :: Lens.Lens' Encryption (Core.Maybe Types.KeyMd5)
eKeyMd5 = Lens.field @"keyMd5"
{-# DEPRECATED eKeyMd5 "Use generic-lens or generic-optics with 'keyMd5' instead." #-}

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
eMode :: Lens.Lens' Encryption (Core.Maybe Types.EncryptionMode)
eMode = Lens.field @"mode"
{-# DEPRECATED eMode "Use generic-lens or generic-optics with 'mode' instead." #-}

instance Core.FromJSON Encryption where
  toJSON Encryption {..} =
    Core.object
      ( Core.catMaybes
          [ ("InitializationVector" Core..=) Core.<$> initializationVector,
            ("Key" Core..=) Core.<$> key,
            ("KeyMd5" Core..=) Core.<$> keyMd5,
            ("Mode" Core..=) Core.<$> mode
          ]
      )

instance Core.FromJSON Encryption where
  parseJSON =
    Core.withObject "Encryption" Core.$
      \x ->
        Encryption'
          Core.<$> (x Core..:? "InitializationVector")
          Core.<*> (x Core..:? "Key")
          Core.<*> (x Core..:? "KeyMd5")
          Core.<*> (x Core..:? "Mode")
