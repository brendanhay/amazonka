{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CloudWatchEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CloudWatchEncryption where

import Network.AWS.Glue.Types.CloudWatchEncryptionMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies how Amazon CloudWatch data should be encrypted.
--
--
--
-- /See:/ 'cloudWatchEncryption' smart constructor.
data CloudWatchEncryption = CloudWatchEncryption'
  { _cweCloudWatchEncryptionMode ::
      !(Maybe CloudWatchEncryptionMode),
    _cweKMSKeyARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cweCloudWatchEncryptionMode' - The encryption mode to use for CloudWatch data.
--
-- * 'cweKMSKeyARN' - The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
cloudWatchEncryption ::
  CloudWatchEncryption
cloudWatchEncryption =
  CloudWatchEncryption'
    { _cweCloudWatchEncryptionMode = Nothing,
      _cweKMSKeyARN = Nothing
    }

-- | The encryption mode to use for CloudWatch data.
cweCloudWatchEncryptionMode :: Lens' CloudWatchEncryption (Maybe CloudWatchEncryptionMode)
cweCloudWatchEncryptionMode = lens _cweCloudWatchEncryptionMode (\s a -> s {_cweCloudWatchEncryptionMode = a})

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
cweKMSKeyARN :: Lens' CloudWatchEncryption (Maybe Text)
cweKMSKeyARN = lens _cweKMSKeyARN (\s a -> s {_cweKMSKeyARN = a})

instance FromJSON CloudWatchEncryption where
  parseJSON =
    withObject
      "CloudWatchEncryption"
      ( \x ->
          CloudWatchEncryption'
            <$> (x .:? "CloudWatchEncryptionMode") <*> (x .:? "KmsKeyArn")
      )

instance Hashable CloudWatchEncryption

instance NFData CloudWatchEncryption

instance ToJSON CloudWatchEncryption where
  toJSON CloudWatchEncryption' {..} =
    object
      ( catMaybes
          [ ("CloudWatchEncryptionMode" .=) <$> _cweCloudWatchEncryptionMode,
            ("KmsKeyArn" .=) <$> _cweKMSKeyARN
          ]
      )
