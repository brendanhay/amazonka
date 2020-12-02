{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DefaultServerSideEncryption where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on the server side encryption method used in the S3 bucket. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html S3 Server-Side Encryption> for more information.
--
--
--
-- /See:/ 'defaultServerSideEncryption' smart constructor.
data DefaultServerSideEncryption = DefaultServerSideEncryption'
  { _dsseEncryptionType ::
      !(Maybe Text),
    _dsseKMSMasterKeyARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DefaultServerSideEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsseEncryptionType' - The type of encryption used for objects within the S3 bucket.
--
-- * 'dsseKMSMasterKeyARN' - The Amazon Resource Name (ARN) of the KMS encryption key. Only available if the bucket @EncryptionType@ is @aws:kms@ .
defaultServerSideEncryption ::
  DefaultServerSideEncryption
defaultServerSideEncryption =
  DefaultServerSideEncryption'
    { _dsseEncryptionType = Nothing,
      _dsseKMSMasterKeyARN = Nothing
    }

-- | The type of encryption used for objects within the S3 bucket.
dsseEncryptionType :: Lens' DefaultServerSideEncryption (Maybe Text)
dsseEncryptionType = lens _dsseEncryptionType (\s a -> s {_dsseEncryptionType = a})

-- | The Amazon Resource Name (ARN) of the KMS encryption key. Only available if the bucket @EncryptionType@ is @aws:kms@ .
dsseKMSMasterKeyARN :: Lens' DefaultServerSideEncryption (Maybe Text)
dsseKMSMasterKeyARN = lens _dsseKMSMasterKeyARN (\s a -> s {_dsseKMSMasterKeyARN = a})

instance FromJSON DefaultServerSideEncryption where
  parseJSON =
    withObject
      "DefaultServerSideEncryption"
      ( \x ->
          DefaultServerSideEncryption'
            <$> (x .:? "encryptionType") <*> (x .:? "kmsMasterKeyArn")
      )

instance Hashable DefaultServerSideEncryption

instance NFData DefaultServerSideEncryption
