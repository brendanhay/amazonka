{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.EncryptionKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.EncryptionKey where

import Network.AWS.CodePipeline.Types.EncryptionKeyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.
--
--
--
-- /See:/ 'encryptionKey' smart constructor.
data EncryptionKey = EncryptionKey'
  { _ekId :: !Text,
    _ekType :: !EncryptionKeyType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ekId' - The ID used to identify the key. For an AWS KMS key, you can use the key ID, the key ARN, or the alias ARN.
--
-- * 'ekType' - The type of encryption key, such as an AWS Key Management Service (AWS KMS) key. When creating or updating a pipeline, the value must be set to 'KMS'.
encryptionKey ::
  -- | 'ekId'
  Text ->
  -- | 'ekType'
  EncryptionKeyType ->
  EncryptionKey
encryptionKey pId_ pType_ =
  EncryptionKey' {_ekId = pId_, _ekType = pType_}

-- | The ID used to identify the key. For an AWS KMS key, you can use the key ID, the key ARN, or the alias ARN.
ekId :: Lens' EncryptionKey Text
ekId = lens _ekId (\s a -> s {_ekId = a})

-- | The type of encryption key, such as an AWS Key Management Service (AWS KMS) key. When creating or updating a pipeline, the value must be set to 'KMS'.
ekType :: Lens' EncryptionKey EncryptionKeyType
ekType = lens _ekType (\s a -> s {_ekType = a})

instance FromJSON EncryptionKey where
  parseJSON =
    withObject
      "EncryptionKey"
      (\x -> EncryptionKey' <$> (x .: "id") <*> (x .: "type"))

instance Hashable EncryptionKey

instance NFData EncryptionKey

instance ToJSON EncryptionKey where
  toJSON EncryptionKey' {..} =
    object
      (catMaybes [Just ("id" .= _ekId), Just ("type" .= _ekType)])
