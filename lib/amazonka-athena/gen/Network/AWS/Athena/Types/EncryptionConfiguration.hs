{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.EncryptionConfiguration where

import Network.AWS.Athena.Types.EncryptionOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information.
--
--
--
-- /See:/ 'encryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { _ecKMSKey ::
      !(Maybe Text),
    _ecEncryptionOption :: !EncryptionOption
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecKMSKey' - For @SSE-KMS@ and @CSE-KMS@ , this is the KMS key ARN or ID.
--
-- * 'ecEncryptionOption' - Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used. If a query runs in a workgroup and the workgroup overrides client-side settings, then the workgroup's setting for encryption is used. It specifies whether query results must be encrypted, for all queries that run in this workgroup.
encryptionConfiguration ::
  -- | 'ecEncryptionOption'
  EncryptionOption ->
  EncryptionConfiguration
encryptionConfiguration pEncryptionOption_ =
  EncryptionConfiguration'
    { _ecKMSKey = Nothing,
      _ecEncryptionOption = pEncryptionOption_
    }

-- | For @SSE-KMS@ and @CSE-KMS@ , this is the KMS key ARN or ID.
ecKMSKey :: Lens' EncryptionConfiguration (Maybe Text)
ecKMSKey = lens _ecKMSKey (\s a -> s {_ecKMSKey = a})

-- | Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used. If a query runs in a workgroup and the workgroup overrides client-side settings, then the workgroup's setting for encryption is used. It specifies whether query results must be encrypted, for all queries that run in this workgroup.
ecEncryptionOption :: Lens' EncryptionConfiguration EncryptionOption
ecEncryptionOption = lens _ecEncryptionOption (\s a -> s {_ecEncryptionOption = a})

instance FromJSON EncryptionConfiguration where
  parseJSON =
    withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            <$> (x .:? "KmsKey") <*> (x .: "EncryptionOption")
      )

instance Hashable EncryptionConfiguration

instance NFData EncryptionConfiguration

instance ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    object
      ( catMaybes
          [ ("KmsKey" .=) <$> _ecKMSKey,
            Just ("EncryptionOption" .= _ecEncryptionOption)
          ]
      )
