{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Encryption where

import Network.AWS.Glacier.Types.EncryptionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the encryption used to store the job results in Amazon S3.
--
--
--
-- /See:/ 'encryption' smart constructor.
data Encryption = Encryption'
  { _eEncryptionType ::
      !(Maybe EncryptionType),
    _eKMSKeyId :: !(Maybe Text),
    _eKMSContext :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Encryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eEncryptionType' - The server-side encryption algorithm used when storing job results in Amazon S3, for example @AES256@ or @aws:kms@ .
--
-- * 'eKMSKeyId' - The AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS fail if not made by using Secure Sockets Layer (SSL) or Signature Version 4.
--
-- * 'eKMSContext' - Optional. If the encryption type is @aws:kms@ , you can use this value to specify the encryption context for the job results.
encryption ::
  Encryption
encryption =
  Encryption'
    { _eEncryptionType = Nothing,
      _eKMSKeyId = Nothing,
      _eKMSContext = Nothing
    }

-- | The server-side encryption algorithm used when storing job results in Amazon S3, for example @AES256@ or @aws:kms@ .
eEncryptionType :: Lens' Encryption (Maybe EncryptionType)
eEncryptionType = lens _eEncryptionType (\s a -> s {_eEncryptionType = a})

-- | The AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS fail if not made by using Secure Sockets Layer (SSL) or Signature Version 4.
eKMSKeyId :: Lens' Encryption (Maybe Text)
eKMSKeyId = lens _eKMSKeyId (\s a -> s {_eKMSKeyId = a})

-- | Optional. If the encryption type is @aws:kms@ , you can use this value to specify the encryption context for the job results.
eKMSContext :: Lens' Encryption (Maybe Text)
eKMSContext = lens _eKMSContext (\s a -> s {_eKMSContext = a})

instance FromJSON Encryption where
  parseJSON =
    withObject
      "Encryption"
      ( \x ->
          Encryption'
            <$> (x .:? "EncryptionType")
            <*> (x .:? "KMSKeyId")
            <*> (x .:? "KMSContext")
      )

instance Hashable Encryption

instance NFData Encryption

instance ToJSON Encryption where
  toJSON Encryption' {..} =
    object
      ( catMaybes
          [ ("EncryptionType" .=) <$> _eEncryptionType,
            ("KMSKeyId" .=) <$> _eKMSKeyId,
            ("KMSContext" .=) <$> _eKMSContext
          ]
      )
