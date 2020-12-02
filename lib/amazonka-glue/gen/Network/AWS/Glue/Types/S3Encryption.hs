{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.S3Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3Encryption where

import Network.AWS.Glue.Types.S3EncryptionMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies how Amazon Simple Storage Service (Amazon S3) data should be encrypted.
--
--
--
-- /See:/ 's3Encryption' smart constructor.
data S3Encryption = S3Encryption'
  { _seS3EncryptionMode ::
      !(Maybe S3EncryptionMode),
    _seKMSKeyARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Encryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seS3EncryptionMode' - The encryption mode to use for Amazon S3 data.
--
-- * 'seKMSKeyARN' - The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
s3Encryption ::
  S3Encryption
s3Encryption =
  S3Encryption'
    { _seS3EncryptionMode = Nothing,
      _seKMSKeyARN = Nothing
    }

-- | The encryption mode to use for Amazon S3 data.
seS3EncryptionMode :: Lens' S3Encryption (Maybe S3EncryptionMode)
seS3EncryptionMode = lens _seS3EncryptionMode (\s a -> s {_seS3EncryptionMode = a})

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
seKMSKeyARN :: Lens' S3Encryption (Maybe Text)
seKMSKeyARN = lens _seKMSKeyARN (\s a -> s {_seKMSKeyARN = a})

instance FromJSON S3Encryption where
  parseJSON =
    withObject
      "S3Encryption"
      ( \x ->
          S3Encryption'
            <$> (x .:? "S3EncryptionMode") <*> (x .:? "KmsKeyArn")
      )

instance Hashable S3Encryption

instance NFData S3Encryption

instance ToJSON S3Encryption where
  toJSON S3Encryption' {..} =
    object
      ( catMaybes
          [ ("S3EncryptionMode" .=) <$> _seS3EncryptionMode,
            ("KmsKeyArn" .=) <$> _seKMSKeyARN
          ]
      )
