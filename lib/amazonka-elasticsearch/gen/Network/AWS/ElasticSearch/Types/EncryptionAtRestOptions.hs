{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the Encryption At Rest Options.
--
--
--
-- /See:/ 'encryptionAtRestOptions' smart constructor.
data EncryptionAtRestOptions = EncryptionAtRestOptions'
  { _earoEnabled ::
      !(Maybe Bool),
    _earoKMSKeyId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionAtRestOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'earoEnabled' - Specifies the option to enable Encryption At Rest.
--
-- * 'earoKMSKeyId' - Specifies the KMS Key ID for Encryption At Rest options.
encryptionAtRestOptions ::
  EncryptionAtRestOptions
encryptionAtRestOptions =
  EncryptionAtRestOptions'
    { _earoEnabled = Nothing,
      _earoKMSKeyId = Nothing
    }

-- | Specifies the option to enable Encryption At Rest.
earoEnabled :: Lens' EncryptionAtRestOptions (Maybe Bool)
earoEnabled = lens _earoEnabled (\s a -> s {_earoEnabled = a})

-- | Specifies the KMS Key ID for Encryption At Rest options.
earoKMSKeyId :: Lens' EncryptionAtRestOptions (Maybe Text)
earoKMSKeyId = lens _earoKMSKeyId (\s a -> s {_earoKMSKeyId = a})

instance FromJSON EncryptionAtRestOptions where
  parseJSON =
    withObject
      "EncryptionAtRestOptions"
      ( \x ->
          EncryptionAtRestOptions'
            <$> (x .:? "Enabled") <*> (x .:? "KmsKeyId")
      )

instance Hashable EncryptionAtRestOptions

instance NFData EncryptionAtRestOptions

instance ToJSON EncryptionAtRestOptions where
  toJSON EncryptionAtRestOptions' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _earoEnabled,
            ("KmsKeyId" .=) <$> _earoKMSKeyId
          ]
      )
