{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.EncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.EncryptionConfig where

import Network.AWS.Connect.Types.EncryptionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The encryption configuration.
--
--
--
-- /See:/ 'encryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { _ecEncryptionType ::
      !EncryptionType,
    _ecKeyId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecEncryptionType' - The type of encryption.
--
-- * 'ecKeyId' - The identifier of the encryption key.
encryptionConfig ::
  -- | 'ecEncryptionType'
  EncryptionType ->
  -- | 'ecKeyId'
  Text ->
  EncryptionConfig
encryptionConfig pEncryptionType_ pKeyId_ =
  EncryptionConfig'
    { _ecEncryptionType = pEncryptionType_,
      _ecKeyId = pKeyId_
    }

-- | The type of encryption.
ecEncryptionType :: Lens' EncryptionConfig EncryptionType
ecEncryptionType = lens _ecEncryptionType (\s a -> s {_ecEncryptionType = a})

-- | The identifier of the encryption key.
ecKeyId :: Lens' EncryptionConfig Text
ecKeyId = lens _ecKeyId (\s a -> s {_ecKeyId = a})

instance FromJSON EncryptionConfig where
  parseJSON =
    withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig' <$> (x .: "EncryptionType") <*> (x .: "KeyId")
      )

instance Hashable EncryptionConfig

instance NFData EncryptionConfig

instance ToJSON EncryptionConfig where
  toJSON EncryptionConfig' {..} =
    object
      ( catMaybes
          [ Just ("EncryptionType" .= _ecEncryptionType),
            Just ("KeyId" .= _ecKeyId)
          ]
      )
