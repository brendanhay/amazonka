{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputDecryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputDecryptionSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.DecryptionMode
import Network.AWS.Prelude

-- | Settings for decrypting any input files that you encrypt before you upload them to Amazon S3. MediaConvert can decrypt files only when you use AWS Key Management Service (KMS) to encrypt the data key that you use to encrypt your content.
--
-- /See:/ 'inputDecryptionSettings' smart constructor.
data InputDecryptionSettings = InputDecryptionSettings'
  { _idsEncryptedDecryptionKey ::
      !(Maybe Text),
    _idsKMSKeyRegion :: !(Maybe Text),
    _idsDecryptionMode ::
      !(Maybe DecryptionMode),
    _idsInitializationVector :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDecryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idsEncryptedDecryptionKey' - Warning! Don't provide your encryption key in plaintext. Your job settings could be intercepted, making your encrypted content vulnerable. Specify the encrypted version of the data key that you used to encrypt your content. The data key must be encrypted by AWS Key Management Service (KMS). The key can be 128, 192, or 256 bits.
--
-- * 'idsKMSKeyRegion' - Specify the AWS Region for AWS Key Management Service (KMS) that you used to encrypt your data key, if that Region is different from the one you are using for AWS Elemental MediaConvert.
--
-- * 'idsDecryptionMode' - Specify the encryption mode that you used to encrypt your input files.
--
-- * 'idsInitializationVector' - Specify the initialization vector that you used when you encrypted your content before uploading it to Amazon S3. You can use a 16-byte initialization vector with any encryption mode. Or, you can use a 12-byte initialization vector with GCM or CTR. MediaConvert accepts only initialization vectors that are base64-encoded.
inputDecryptionSettings ::
  InputDecryptionSettings
inputDecryptionSettings =
  InputDecryptionSettings'
    { _idsEncryptedDecryptionKey = Nothing,
      _idsKMSKeyRegion = Nothing,
      _idsDecryptionMode = Nothing,
      _idsInitializationVector = Nothing
    }

-- | Warning! Don't provide your encryption key in plaintext. Your job settings could be intercepted, making your encrypted content vulnerable. Specify the encrypted version of the data key that you used to encrypt your content. The data key must be encrypted by AWS Key Management Service (KMS). The key can be 128, 192, or 256 bits.
idsEncryptedDecryptionKey :: Lens' InputDecryptionSettings (Maybe Text)
idsEncryptedDecryptionKey = lens _idsEncryptedDecryptionKey (\s a -> s {_idsEncryptedDecryptionKey = a})

-- | Specify the AWS Region for AWS Key Management Service (KMS) that you used to encrypt your data key, if that Region is different from the one you are using for AWS Elemental MediaConvert.
idsKMSKeyRegion :: Lens' InputDecryptionSettings (Maybe Text)
idsKMSKeyRegion = lens _idsKMSKeyRegion (\s a -> s {_idsKMSKeyRegion = a})

-- | Specify the encryption mode that you used to encrypt your input files.
idsDecryptionMode :: Lens' InputDecryptionSettings (Maybe DecryptionMode)
idsDecryptionMode = lens _idsDecryptionMode (\s a -> s {_idsDecryptionMode = a})

-- | Specify the initialization vector that you used when you encrypted your content before uploading it to Amazon S3. You can use a 16-byte initialization vector with any encryption mode. Or, you can use a 12-byte initialization vector with GCM or CTR. MediaConvert accepts only initialization vectors that are base64-encoded.
idsInitializationVector :: Lens' InputDecryptionSettings (Maybe Text)
idsInitializationVector = lens _idsInitializationVector (\s a -> s {_idsInitializationVector = a})

instance FromJSON InputDecryptionSettings where
  parseJSON =
    withObject
      "InputDecryptionSettings"
      ( \x ->
          InputDecryptionSettings'
            <$> (x .:? "encryptedDecryptionKey")
            <*> (x .:? "kmsKeyRegion")
            <*> (x .:? "decryptionMode")
            <*> (x .:? "initializationVector")
      )

instance Hashable InputDecryptionSettings

instance NFData InputDecryptionSettings

instance ToJSON InputDecryptionSettings where
  toJSON InputDecryptionSettings' {..} =
    object
      ( catMaybes
          [ ("encryptedDecryptionKey" .=) <$> _idsEncryptedDecryptionKey,
            ("kmsKeyRegion" .=) <$> _idsKMSKeyRegion,
            ("decryptionMode" .=) <$> _idsDecryptionMode,
            ("initializationVector" .=) <$> _idsInitializationVector
          ]
      )
