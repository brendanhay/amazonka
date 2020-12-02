{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EncryptionConfiguration where

import Network.AWS.Glue.Types.CloudWatchEncryption
import Network.AWS.Glue.Types.JobBookmarksEncryption
import Network.AWS.Glue.Types.S3Encryption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies an encryption configuration.
--
--
--
-- /See:/ 'encryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { _ecS3Encryption ::
      !(Maybe [S3Encryption]),
    _ecJobBookmarksEncryption ::
      !(Maybe JobBookmarksEncryption),
    _ecCloudWatchEncryption ::
      !(Maybe CloudWatchEncryption)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecS3Encryption' - The encryption configuration for Amazon Simple Storage Service (Amazon S3) data.
--
-- * 'ecJobBookmarksEncryption' - The encryption configuration for job bookmarks.
--
-- * 'ecCloudWatchEncryption' - The encryption configuration for Amazon CloudWatch.
encryptionConfiguration ::
  EncryptionConfiguration
encryptionConfiguration =
  EncryptionConfiguration'
    { _ecS3Encryption = Nothing,
      _ecJobBookmarksEncryption = Nothing,
      _ecCloudWatchEncryption = Nothing
    }

-- | The encryption configuration for Amazon Simple Storage Service (Amazon S3) data.
ecS3Encryption :: Lens' EncryptionConfiguration [S3Encryption]
ecS3Encryption = lens _ecS3Encryption (\s a -> s {_ecS3Encryption = a}) . _Default . _Coerce

-- | The encryption configuration for job bookmarks.
ecJobBookmarksEncryption :: Lens' EncryptionConfiguration (Maybe JobBookmarksEncryption)
ecJobBookmarksEncryption = lens _ecJobBookmarksEncryption (\s a -> s {_ecJobBookmarksEncryption = a})

-- | The encryption configuration for Amazon CloudWatch.
ecCloudWatchEncryption :: Lens' EncryptionConfiguration (Maybe CloudWatchEncryption)
ecCloudWatchEncryption = lens _ecCloudWatchEncryption (\s a -> s {_ecCloudWatchEncryption = a})

instance FromJSON EncryptionConfiguration where
  parseJSON =
    withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            <$> (x .:? "S3Encryption" .!= mempty)
            <*> (x .:? "JobBookmarksEncryption")
            <*> (x .:? "CloudWatchEncryption")
      )

instance Hashable EncryptionConfiguration

instance NFData EncryptionConfiguration

instance ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    object
      ( catMaybes
          [ ("S3Encryption" .=) <$> _ecS3Encryption,
            ("JobBookmarksEncryption" .=) <$> _ecJobBookmarksEncryption,
            ("CloudWatchEncryption" .=) <$> _ecCloudWatchEncryption
          ]
      )
