{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Encryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Encryption where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The encryption settings, if any, that are used for decrypting your input
-- files or encrypting your output files. If your input file is encrypted,
-- you must specify the mode that Elastic Transcoder uses to decrypt your
-- file, otherwise you must specify the mode you want Elastic Transcoder to
-- use to encrypt your output files.
--
-- /See:/ 'newEncryption' smart constructor.
data Encryption = Encryption'
  { -- | The data encryption key that you want Elastic Transcoder to use to
    -- encrypt your output file, or that was used to encrypt your input file.
    -- The key must be base64-encoded and it must be one of the following bit
    -- lengths before being base64-encoded:
    --
    -- @128@, @192@, or @256@.
    --
    -- The key must also be encrypted by using the Amazon Key Management
    -- Service.
    key :: Prelude.Maybe Prelude.Text,
    -- | The specific server-side encryption mode that you want Elastic
    -- Transcoder to use when decrypting your input files or encrypting your
    -- output files. Elastic Transcoder supports the following options:
    --
    -- -   __s3:__ Amazon S3 creates and manages the keys used for encrypting
    --     your files.
    --
    -- -   __s3-aws-kms:__ Amazon S3 calls the Amazon Key Management Service,
    --     which creates and manages the keys that are used for encrypting your
    --     files. If you specify @s3-aws-kms@ and you don\'t want to use the
    --     default key, you must add the AWS-KMS key that you want to use to
    --     your pipeline.
    --
    -- -   __aes-cbc-pkcs7:__ A padded cipher-block mode of operation
    --     originally used for HLS files.
    --
    -- -   __aes-ctr:__ AES Counter Mode.
    --
    -- -   __aes-gcm:__ AES Galois Counter Mode, a mode of operation that is an
    --     authenticated encryption format, meaning that a file, key, or
    --     initialization vector that has been tampered with fails the
    --     decryption process.
    --
    -- For all three AES options, you must provide the following settings,
    -- which must be base64-encoded:
    --
    -- -   __Key__
    --
    -- -   __Key MD5__
    --
    -- -   __Initialization Vector__
    --
    -- For the AES modes, your private encryption keys and your unencrypted
    -- data are never stored by AWS; therefore, it is important that you safely
    -- manage your encryption keys. If you lose them, you won\'t be able to
    -- unencrypt your data.
    mode :: Prelude.Maybe Prelude.Text,
    -- | The MD5 digest of the key that you used to encrypt your input file, or
    -- that you want Elastic Transcoder to use to encrypt your output file.
    -- Elastic Transcoder uses the key digest as a checksum to make sure your
    -- key was not corrupted in transit. The key MD5 must be base64-encoded,
    -- and it must be exactly 16 bytes long before being base64-encoded.
    keyMd5 :: Prelude.Maybe Prelude.Text,
    -- | The series of random bits created by a random bit generator, unique for
    -- every encryption operation, that you used to encrypt your input files or
    -- that you want Elastic Transcoder to use to encrypt your output files.
    -- The initialization vector must be base64-encoded, and it must be exactly
    -- 16 bytes long before being base64-encoded.
    initializationVector :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Encryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'encryption_key' - The data encryption key that you want Elastic Transcoder to use to
-- encrypt your output file, or that was used to encrypt your input file.
-- The key must be base64-encoded and it must be one of the following bit
-- lengths before being base64-encoded:
--
-- @128@, @192@, or @256@.
--
-- The key must also be encrypted by using the Amazon Key Management
-- Service.
--
-- 'mode', 'encryption_mode' - The specific server-side encryption mode that you want Elastic
-- Transcoder to use when decrypting your input files or encrypting your
-- output files. Elastic Transcoder supports the following options:
--
-- -   __s3:__ Amazon S3 creates and manages the keys used for encrypting
--     your files.
--
-- -   __s3-aws-kms:__ Amazon S3 calls the Amazon Key Management Service,
--     which creates and manages the keys that are used for encrypting your
--     files. If you specify @s3-aws-kms@ and you don\'t want to use the
--     default key, you must add the AWS-KMS key that you want to use to
--     your pipeline.
--
-- -   __aes-cbc-pkcs7:__ A padded cipher-block mode of operation
--     originally used for HLS files.
--
-- -   __aes-ctr:__ AES Counter Mode.
--
-- -   __aes-gcm:__ AES Galois Counter Mode, a mode of operation that is an
--     authenticated encryption format, meaning that a file, key, or
--     initialization vector that has been tampered with fails the
--     decryption process.
--
-- For all three AES options, you must provide the following settings,
-- which must be base64-encoded:
--
-- -   __Key__
--
-- -   __Key MD5__
--
-- -   __Initialization Vector__
--
-- For the AES modes, your private encryption keys and your unencrypted
-- data are never stored by AWS; therefore, it is important that you safely
-- manage your encryption keys. If you lose them, you won\'t be able to
-- unencrypt your data.
--
-- 'keyMd5', 'encryption_keyMd5' - The MD5 digest of the key that you used to encrypt your input file, or
-- that you want Elastic Transcoder to use to encrypt your output file.
-- Elastic Transcoder uses the key digest as a checksum to make sure your
-- key was not corrupted in transit. The key MD5 must be base64-encoded,
-- and it must be exactly 16 bytes long before being base64-encoded.
--
-- 'initializationVector', 'encryption_initializationVector' - The series of random bits created by a random bit generator, unique for
-- every encryption operation, that you used to encrypt your input files or
-- that you want Elastic Transcoder to use to encrypt your output files.
-- The initialization vector must be base64-encoded, and it must be exactly
-- 16 bytes long before being base64-encoded.
newEncryption ::
  Encryption
newEncryption =
  Encryption'
    { key = Prelude.Nothing,
      mode = Prelude.Nothing,
      keyMd5 = Prelude.Nothing,
      initializationVector = Prelude.Nothing
    }

-- | The data encryption key that you want Elastic Transcoder to use to
-- encrypt your output file, or that was used to encrypt your input file.
-- The key must be base64-encoded and it must be one of the following bit
-- lengths before being base64-encoded:
--
-- @128@, @192@, or @256@.
--
-- The key must also be encrypted by using the Amazon Key Management
-- Service.
encryption_key :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_key = Lens.lens (\Encryption' {key} -> key) (\s@Encryption' {} a -> s {key = a} :: Encryption)

-- | The specific server-side encryption mode that you want Elastic
-- Transcoder to use when decrypting your input files or encrypting your
-- output files. Elastic Transcoder supports the following options:
--
-- -   __s3:__ Amazon S3 creates and manages the keys used for encrypting
--     your files.
--
-- -   __s3-aws-kms:__ Amazon S3 calls the Amazon Key Management Service,
--     which creates and manages the keys that are used for encrypting your
--     files. If you specify @s3-aws-kms@ and you don\'t want to use the
--     default key, you must add the AWS-KMS key that you want to use to
--     your pipeline.
--
-- -   __aes-cbc-pkcs7:__ A padded cipher-block mode of operation
--     originally used for HLS files.
--
-- -   __aes-ctr:__ AES Counter Mode.
--
-- -   __aes-gcm:__ AES Galois Counter Mode, a mode of operation that is an
--     authenticated encryption format, meaning that a file, key, or
--     initialization vector that has been tampered with fails the
--     decryption process.
--
-- For all three AES options, you must provide the following settings,
-- which must be base64-encoded:
--
-- -   __Key__
--
-- -   __Key MD5__
--
-- -   __Initialization Vector__
--
-- For the AES modes, your private encryption keys and your unencrypted
-- data are never stored by AWS; therefore, it is important that you safely
-- manage your encryption keys. If you lose them, you won\'t be able to
-- unencrypt your data.
encryption_mode :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_mode = Lens.lens (\Encryption' {mode} -> mode) (\s@Encryption' {} a -> s {mode = a} :: Encryption)

-- | The MD5 digest of the key that you used to encrypt your input file, or
-- that you want Elastic Transcoder to use to encrypt your output file.
-- Elastic Transcoder uses the key digest as a checksum to make sure your
-- key was not corrupted in transit. The key MD5 must be base64-encoded,
-- and it must be exactly 16 bytes long before being base64-encoded.
encryption_keyMd5 :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_keyMd5 = Lens.lens (\Encryption' {keyMd5} -> keyMd5) (\s@Encryption' {} a -> s {keyMd5 = a} :: Encryption)

-- | The series of random bits created by a random bit generator, unique for
-- every encryption operation, that you used to encrypt your input files or
-- that you want Elastic Transcoder to use to encrypt your output files.
-- The initialization vector must be base64-encoded, and it must be exactly
-- 16 bytes long before being base64-encoded.
encryption_initializationVector :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_initializationVector = Lens.lens (\Encryption' {initializationVector} -> initializationVector) (\s@Encryption' {} a -> s {initializationVector = a} :: Encryption)

instance Prelude.FromJSON Encryption where
  parseJSON =
    Prelude.withObject
      "Encryption"
      ( \x ->
          Encryption'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Mode")
            Prelude.<*> (x Prelude..:? "KeyMd5")
            Prelude.<*> (x Prelude..:? "InitializationVector")
      )

instance Prelude.Hashable Encryption

instance Prelude.NFData Encryption

instance Prelude.ToJSON Encryption where
  toJSON Encryption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Mode" Prelude..=) Prelude.<$> mode,
            ("KeyMd5" Prelude..=) Prelude.<$> keyMd5,
            ("InitializationVector" Prelude..=)
              Prelude.<$> initializationVector
          ]
      )
