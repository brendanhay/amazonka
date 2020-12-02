{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.EncryptionKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.EncryptionKey where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Translate.Types.EncryptionKeyType

-- | The encryption key used to encrypt this object.
--
--
--
-- /See:/ 'encryptionKey' smart constructor.
data EncryptionKey = EncryptionKey'
  { _ekType :: !EncryptionKeyType,
    _ekId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ekType' - The type of encryption key used by Amazon Translate to encrypt custom terminologies.
--
-- * 'ekId' - The Amazon Resource Name (ARN) of the encryption key being used to encrypt the custom terminology.
encryptionKey ::
  -- | 'ekType'
  EncryptionKeyType ->
  -- | 'ekId'
  Text ->
  EncryptionKey
encryptionKey pType_ pId_ =
  EncryptionKey' {_ekType = pType_, _ekId = pId_}

-- | The type of encryption key used by Amazon Translate to encrypt custom terminologies.
ekType :: Lens' EncryptionKey EncryptionKeyType
ekType = lens _ekType (\s a -> s {_ekType = a})

-- | The Amazon Resource Name (ARN) of the encryption key being used to encrypt the custom terminology.
ekId :: Lens' EncryptionKey Text
ekId = lens _ekId (\s a -> s {_ekId = a})

instance FromJSON EncryptionKey where
  parseJSON =
    withObject
      "EncryptionKey"
      (\x -> EncryptionKey' <$> (x .: "Type") <*> (x .: "Id"))

instance Hashable EncryptionKey

instance NFData EncryptionKey

instance ToJSON EncryptionKey where
  toJSON EncryptionKey' {..} =
    object
      (catMaybes [Just ("Type" .= _ekType), Just ("Id" .= _ekId)])
