{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionPasswordEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionPasswordEncryption where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The data structure used by the Data Catalog to encrypt the password as part of @CreateConnection@ or @UpdateConnection@ and store it in the @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable catalog encryption or only password encryption.
--
--
-- When a @CreationConnection@ request arrives containing a password, the Data Catalog first encrypts the password using your AWS KMS key. It then encrypts the whole connection object again if catalog encryption is also enabled.
--
-- This encryption requires that you set AWS KMS key permissions to enable or restrict access on the password key according to your security requirements. For example, you might want only administrators to have decrypt permission on the password key.
--
--
-- /See:/ 'connectionPasswordEncryption' smart constructor.
data ConnectionPasswordEncryption = ConnectionPasswordEncryption'
  { _cpeAWSKMSKeyId ::
      !(Maybe Text),
    _cpeReturnConnectionPasswordEncrypted ::
      !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectionPasswordEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpeAWSKMSKeyId' - An AWS KMS key that is used to encrypt the connection password.  If connection password protection is enabled, the caller of @CreateConnection@ and @UpdateConnection@ needs at least @kms:Encrypt@ permission on the specified AWS KMS key, to encrypt passwords before storing them in the Data Catalog.  You can set the decrypt permission to enable or restrict access on the password key according to your security requirements.
--
-- * 'cpeReturnConnectionPasswordEncrypted' - When the @ReturnConnectionPasswordEncrypted@ flag is set to "true", passwords remain encrypted in the responses of @GetConnection@ and @GetConnections@ . This encryption takes effect independently from catalog encryption.
connectionPasswordEncryption ::
  -- | 'cpeReturnConnectionPasswordEncrypted'
  Bool ->
  ConnectionPasswordEncryption
connectionPasswordEncryption pReturnConnectionPasswordEncrypted_ =
  ConnectionPasswordEncryption'
    { _cpeAWSKMSKeyId = Nothing,
      _cpeReturnConnectionPasswordEncrypted =
        pReturnConnectionPasswordEncrypted_
    }

-- | An AWS KMS key that is used to encrypt the connection password.  If connection password protection is enabled, the caller of @CreateConnection@ and @UpdateConnection@ needs at least @kms:Encrypt@ permission on the specified AWS KMS key, to encrypt passwords before storing them in the Data Catalog.  You can set the decrypt permission to enable or restrict access on the password key according to your security requirements.
cpeAWSKMSKeyId :: Lens' ConnectionPasswordEncryption (Maybe Text)
cpeAWSKMSKeyId = lens _cpeAWSKMSKeyId (\s a -> s {_cpeAWSKMSKeyId = a})

-- | When the @ReturnConnectionPasswordEncrypted@ flag is set to "true", passwords remain encrypted in the responses of @GetConnection@ and @GetConnections@ . This encryption takes effect independently from catalog encryption.
cpeReturnConnectionPasswordEncrypted :: Lens' ConnectionPasswordEncryption Bool
cpeReturnConnectionPasswordEncrypted = lens _cpeReturnConnectionPasswordEncrypted (\s a -> s {_cpeReturnConnectionPasswordEncrypted = a})

instance FromJSON ConnectionPasswordEncryption where
  parseJSON =
    withObject
      "ConnectionPasswordEncryption"
      ( \x ->
          ConnectionPasswordEncryption'
            <$> (x .:? "AwsKmsKeyId")
            <*> (x .: "ReturnConnectionPasswordEncrypted")
      )

instance Hashable ConnectionPasswordEncryption

instance NFData ConnectionPasswordEncryption

instance ToJSON ConnectionPasswordEncryption where
  toJSON ConnectionPasswordEncryption' {..} =
    object
      ( catMaybes
          [ ("AwsKmsKeyId" .=) <$> _cpeAWSKMSKeyId,
            Just
              ( "ReturnConnectionPasswordEncrypted"
                  .= _cpeReturnConnectionPasswordEncrypted
              )
          ]
      )
