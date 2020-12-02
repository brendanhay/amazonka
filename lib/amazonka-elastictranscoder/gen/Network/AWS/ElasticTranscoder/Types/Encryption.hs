{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Encryption where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The encryption settings, if any, that are used for decrypting your input files or encrypting your output files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file, otherwise you must specify the mode you want Elastic Transcoder to use to encrypt your output files.
--
--
--
-- /See:/ 'encryption' smart constructor.
data Encryption = Encryption'
  { _eMode :: !(Maybe Text),
    _eKeyMD5 :: !(Maybe Text),
    _eKey :: !(Maybe Text),
    _eInitializationVector :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Encryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eMode' - The specific server-side encryption mode that you want Elastic Transcoder to use when decrypting your input files or encrypting your output files. Elastic Transcoder supports the following options:     * __s3:__ Amazon S3 creates and manages the keys used for encrypting your files.     * __s3-aws-kms:__ Amazon S3 calls the Amazon Key Management Service, which creates and manages the keys that are used for encrypting your files. If you specify @s3-aws-kms@ and you don't want to use the default key, you must add the AWS-KMS key that you want to use to your pipeline.     * __aes-cbc-pkcs7:__ A padded cipher-block mode of operation originally used for HLS files.     * __aes-ctr:__ AES Counter Mode.     * __aes-gcm:__ AES Galois Counter Mode, a mode of operation that is an authenticated encryption format, meaning that a file, key, or initialization vector that has been tampered with fails the decryption process. For all three AES options, you must provide the following settings, which must be base64-encoded:     * __Key__      * __Key MD5__      * __Initialization Vector__  /Important:/ For the AES modes, your private encryption keys and your unencrypted data are never stored by AWS; therefore, it is important that you safely manage your encryption keys. If you lose them, you won't be able to unencrypt your data.
--
-- * 'eKeyMD5' - The MD5 digest of the key that you used to encrypt your input file, or that you want Elastic Transcoder to use to encrypt your output file. Elastic Transcoder uses the key digest as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
--
-- * 'eKey' - The data encryption key that you want Elastic Transcoder to use to encrypt your output file, or that was used to encrypt your input file. The key must be base64-encoded and it must be one of the following bit lengths before being base64-encoded: @128@ , @192@ , or @256@ .  The key must also be encrypted by using the Amazon Key Management Service.
--
-- * 'eInitializationVector' - The series of random bits created by a random bit generator, unique for every encryption operation, that you used to encrypt your input files or that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
encryption ::
  Encryption
encryption =
  Encryption'
    { _eMode = Nothing,
      _eKeyMD5 = Nothing,
      _eKey = Nothing,
      _eInitializationVector = Nothing
    }

-- | The specific server-side encryption mode that you want Elastic Transcoder to use when decrypting your input files or encrypting your output files. Elastic Transcoder supports the following options:     * __s3:__ Amazon S3 creates and manages the keys used for encrypting your files.     * __s3-aws-kms:__ Amazon S3 calls the Amazon Key Management Service, which creates and manages the keys that are used for encrypting your files. If you specify @s3-aws-kms@ and you don't want to use the default key, you must add the AWS-KMS key that you want to use to your pipeline.     * __aes-cbc-pkcs7:__ A padded cipher-block mode of operation originally used for HLS files.     * __aes-ctr:__ AES Counter Mode.     * __aes-gcm:__ AES Galois Counter Mode, a mode of operation that is an authenticated encryption format, meaning that a file, key, or initialization vector that has been tampered with fails the decryption process. For all three AES options, you must provide the following settings, which must be base64-encoded:     * __Key__      * __Key MD5__      * __Initialization Vector__  /Important:/ For the AES modes, your private encryption keys and your unencrypted data are never stored by AWS; therefore, it is important that you safely manage your encryption keys. If you lose them, you won't be able to unencrypt your data.
eMode :: Lens' Encryption (Maybe Text)
eMode = lens _eMode (\s a -> s {_eMode = a})

-- | The MD5 digest of the key that you used to encrypt your input file, or that you want Elastic Transcoder to use to encrypt your output file. Elastic Transcoder uses the key digest as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
eKeyMD5 :: Lens' Encryption (Maybe Text)
eKeyMD5 = lens _eKeyMD5 (\s a -> s {_eKeyMD5 = a})

-- | The data encryption key that you want Elastic Transcoder to use to encrypt your output file, or that was used to encrypt your input file. The key must be base64-encoded and it must be one of the following bit lengths before being base64-encoded: @128@ , @192@ , or @256@ .  The key must also be encrypted by using the Amazon Key Management Service.
eKey :: Lens' Encryption (Maybe Text)
eKey = lens _eKey (\s a -> s {_eKey = a})

-- | The series of random bits created by a random bit generator, unique for every encryption operation, that you used to encrypt your input files or that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
eInitializationVector :: Lens' Encryption (Maybe Text)
eInitializationVector = lens _eInitializationVector (\s a -> s {_eInitializationVector = a})

instance FromJSON Encryption where
  parseJSON =
    withObject
      "Encryption"
      ( \x ->
          Encryption'
            <$> (x .:? "Mode")
            <*> (x .:? "KeyMd5")
            <*> (x .:? "Key")
            <*> (x .:? "InitializationVector")
      )

instance Hashable Encryption

instance NFData Encryption

instance ToJSON Encryption where
  toJSON Encryption' {..} =
    object
      ( catMaybes
          [ ("Mode" .=) <$> _eMode,
            ("KeyMd5" .=) <$> _eKeyMD5,
            ("Key" .=) <$> _eKey,
            ("InitializationVector" .=) <$> _eInitializationVector
          ]
      )
