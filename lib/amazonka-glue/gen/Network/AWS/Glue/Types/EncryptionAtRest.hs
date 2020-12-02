{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.EncryptionAtRest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EncryptionAtRest where

import Network.AWS.Glue.Types.CatalogEncryptionMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the encryption-at-rest configuration for the Data Catalog.
--
--
--
-- /See:/ 'encryptionAtRest' smart constructor.
data EncryptionAtRest = EncryptionAtRest'
  { _earSseAWSKMSKeyId ::
      !(Maybe Text),
    _earCatalogEncryptionMode :: !CatalogEncryptionMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionAtRest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'earSseAWSKMSKeyId' - The ID of the AWS KMS key to use for encryption at rest.
--
-- * 'earCatalogEncryptionMode' - The encryption-at-rest mode for encrypting Data Catalog data.
encryptionAtRest ::
  -- | 'earCatalogEncryptionMode'
  CatalogEncryptionMode ->
  EncryptionAtRest
encryptionAtRest pCatalogEncryptionMode_ =
  EncryptionAtRest'
    { _earSseAWSKMSKeyId = Nothing,
      _earCatalogEncryptionMode = pCatalogEncryptionMode_
    }

-- | The ID of the AWS KMS key to use for encryption at rest.
earSseAWSKMSKeyId :: Lens' EncryptionAtRest (Maybe Text)
earSseAWSKMSKeyId = lens _earSseAWSKMSKeyId (\s a -> s {_earSseAWSKMSKeyId = a})

-- | The encryption-at-rest mode for encrypting Data Catalog data.
earCatalogEncryptionMode :: Lens' EncryptionAtRest CatalogEncryptionMode
earCatalogEncryptionMode = lens _earCatalogEncryptionMode (\s a -> s {_earCatalogEncryptionMode = a})

instance FromJSON EncryptionAtRest where
  parseJSON =
    withObject
      "EncryptionAtRest"
      ( \x ->
          EncryptionAtRest'
            <$> (x .:? "SseAwsKmsKeyId") <*> (x .: "CatalogEncryptionMode")
      )

instance Hashable EncryptionAtRest

instance NFData EncryptionAtRest

instance ToJSON EncryptionAtRest where
  toJSON EncryptionAtRest' {..} =
    object
      ( catMaybes
          [ ("SseAwsKmsKeyId" .=) <$> _earSseAWSKMSKeyId,
            Just ("CatalogEncryptionMode" .= _earCatalogEncryptionMode)
          ]
      )
