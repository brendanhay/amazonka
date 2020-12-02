{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.EncryptionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.EncryptionOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Encryption options for the broker.
--
-- /See:/ 'encryptionOptions' smart constructor.
data EncryptionOptions = EncryptionOptions'
  { _eoKMSKeyId ::
      !(Maybe Text),
    _eoUseAWSOwnedKey :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoKMSKeyId' - The symmetric customer master key (CMK) to use for the AWS Key Management Service (KMS). This key is used to encrypt your data at rest. If not provided, Amazon MQ will use a default CMK to encrypt your data.
--
-- * 'eoUseAWSOwnedKey' - Enables the use of an AWS owned CMK using AWS Key Management Service (KMS).
encryptionOptions ::
  -- | 'eoUseAWSOwnedKey'
  Bool ->
  EncryptionOptions
encryptionOptions pUseAWSOwnedKey_ =
  EncryptionOptions'
    { _eoKMSKeyId = Nothing,
      _eoUseAWSOwnedKey = pUseAWSOwnedKey_
    }

-- | The symmetric customer master key (CMK) to use for the AWS Key Management Service (KMS). This key is used to encrypt your data at rest. If not provided, Amazon MQ will use a default CMK to encrypt your data.
eoKMSKeyId :: Lens' EncryptionOptions (Maybe Text)
eoKMSKeyId = lens _eoKMSKeyId (\s a -> s {_eoKMSKeyId = a})

-- | Enables the use of an AWS owned CMK using AWS Key Management Service (KMS).
eoUseAWSOwnedKey :: Lens' EncryptionOptions Bool
eoUseAWSOwnedKey = lens _eoUseAWSOwnedKey (\s a -> s {_eoUseAWSOwnedKey = a})

instance FromJSON EncryptionOptions where
  parseJSON =
    withObject
      "EncryptionOptions"
      ( \x ->
          EncryptionOptions'
            <$> (x .:? "kmsKeyId") <*> (x .: "useAwsOwnedKey")
      )

instance Hashable EncryptionOptions

instance NFData EncryptionOptions

instance ToJSON EncryptionOptions where
  toJSON EncryptionOptions' {..} =
    object
      ( catMaybes
          [ ("kmsKeyId" .=) <$> _eoKMSKeyId,
            Just ("useAwsOwnedKey" .= _eoUseAWSOwnedKey)
          ]
      )
