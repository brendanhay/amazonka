{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.EncryptionConfiguration where

import Network.AWS.Firehose.Types.KMSEncryptionConfig
import Network.AWS.Firehose.Types.NoEncryptionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the encryption for a destination in Amazon S3.
--
--
--
-- /See:/ 'encryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { _ecNoEncryptionConfig ::
      !(Maybe NoEncryptionConfig),
    _ecKMSEncryptionConfig ::
      !(Maybe KMSEncryptionConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecNoEncryptionConfig' - Specifically override existing encryption information to ensure that no encryption is used.
--
-- * 'ecKMSEncryptionConfig' - The encryption key.
encryptionConfiguration ::
  EncryptionConfiguration
encryptionConfiguration =
  EncryptionConfiguration'
    { _ecNoEncryptionConfig = Nothing,
      _ecKMSEncryptionConfig = Nothing
    }

-- | Specifically override existing encryption information to ensure that no encryption is used.
ecNoEncryptionConfig :: Lens' EncryptionConfiguration (Maybe NoEncryptionConfig)
ecNoEncryptionConfig = lens _ecNoEncryptionConfig (\s a -> s {_ecNoEncryptionConfig = a})

-- | The encryption key.
ecKMSEncryptionConfig :: Lens' EncryptionConfiguration (Maybe KMSEncryptionConfig)
ecKMSEncryptionConfig = lens _ecKMSEncryptionConfig (\s a -> s {_ecKMSEncryptionConfig = a})

instance FromJSON EncryptionConfiguration where
  parseJSON =
    withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            <$> (x .:? "NoEncryptionConfig") <*> (x .:? "KMSEncryptionConfig")
      )

instance Hashable EncryptionConfiguration

instance NFData EncryptionConfiguration

instance ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    object
      ( catMaybes
          [ ("NoEncryptionConfig" .=) <$> _ecNoEncryptionConfig,
            ("KMSEncryptionConfig" .=) <$> _ecKMSEncryptionConfig
          ]
      )
