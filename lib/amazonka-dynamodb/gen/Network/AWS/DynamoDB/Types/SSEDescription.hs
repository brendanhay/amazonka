{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SSEDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SSEDescription where

import Network.AWS.DynamoDB.Types.SSEStatus
import Network.AWS.DynamoDB.Types.SSEType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The description of the server-side encryption status on the specified table.
--
--
--
-- /See:/ 'sSEDescription' smart constructor.
data SSEDescription = SSEDescription'
  { _ssedStatus ::
      !(Maybe SSEStatus),
    _ssedInaccessibleEncryptionDateTime :: !(Maybe POSIX),
    _ssedSSEType :: !(Maybe SSEType),
    _ssedKMSMasterKeyARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSEDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssedStatus' - Represents the current state of server-side encryption. The only supported values are:     * @ENABLED@ - Server-side encryption is enabled.     * @UPDATING@ - Server-side encryption is being updated.
--
-- * 'ssedInaccessibleEncryptionDateTime' - Indicates the time, in UNIX epoch date format, when DynamoDB detected that the table's AWS KMS key was inaccessible. This attribute will automatically be cleared when DynamoDB detects that the table's AWS KMS key is accessible again. DynamoDB will initiate the table archival process when table's AWS KMS key remains inaccessible for more than seven days from this date.
--
-- * 'ssedSSEType' - Server-side encryption type. The only supported value is:     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
--
-- * 'ssedKMSMasterKeyARN' - The AWS KMS customer master key (CMK) ARN used for the AWS KMS encryption.
sSEDescription ::
  SSEDescription
sSEDescription =
  SSEDescription'
    { _ssedStatus = Nothing,
      _ssedInaccessibleEncryptionDateTime = Nothing,
      _ssedSSEType = Nothing,
      _ssedKMSMasterKeyARN = Nothing
    }

-- | Represents the current state of server-side encryption. The only supported values are:     * @ENABLED@ - Server-side encryption is enabled.     * @UPDATING@ - Server-side encryption is being updated.
ssedStatus :: Lens' SSEDescription (Maybe SSEStatus)
ssedStatus = lens _ssedStatus (\s a -> s {_ssedStatus = a})

-- | Indicates the time, in UNIX epoch date format, when DynamoDB detected that the table's AWS KMS key was inaccessible. This attribute will automatically be cleared when DynamoDB detects that the table's AWS KMS key is accessible again. DynamoDB will initiate the table archival process when table's AWS KMS key remains inaccessible for more than seven days from this date.
ssedInaccessibleEncryptionDateTime :: Lens' SSEDescription (Maybe UTCTime)
ssedInaccessibleEncryptionDateTime = lens _ssedInaccessibleEncryptionDateTime (\s a -> s {_ssedInaccessibleEncryptionDateTime = a}) . mapping _Time

-- | Server-side encryption type. The only supported value is:     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
ssedSSEType :: Lens' SSEDescription (Maybe SSEType)
ssedSSEType = lens _ssedSSEType (\s a -> s {_ssedSSEType = a})

-- | The AWS KMS customer master key (CMK) ARN used for the AWS KMS encryption.
ssedKMSMasterKeyARN :: Lens' SSEDescription (Maybe Text)
ssedKMSMasterKeyARN = lens _ssedKMSMasterKeyARN (\s a -> s {_ssedKMSMasterKeyARN = a})

instance FromJSON SSEDescription where
  parseJSON =
    withObject
      "SSEDescription"
      ( \x ->
          SSEDescription'
            <$> (x .:? "Status")
            <*> (x .:? "InaccessibleEncryptionDateTime")
            <*> (x .:? "SSEType")
            <*> (x .:? "KMSMasterKeyArn")
      )

instance Hashable SSEDescription

instance NFData SSEDescription
