{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DataCatalogEncryptionSettings where

import Network.AWS.Glue.Types.ConnectionPasswordEncryption
import Network.AWS.Glue.Types.EncryptionAtRest
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains configuration information for maintaining Data Catalog security.
--
--
--
-- /See:/ 'dataCatalogEncryptionSettings' smart constructor.
data DataCatalogEncryptionSettings = DataCatalogEncryptionSettings'
  { _dcesEncryptionAtRest ::
      !(Maybe EncryptionAtRest),
    _dcesConnectionPasswordEncryption ::
      !( Maybe
           ConnectionPasswordEncryption
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataCatalogEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcesEncryptionAtRest' - Specifies the encryption-at-rest configuration for the Data Catalog.
--
-- * 'dcesConnectionPasswordEncryption' - When connection password protection is enabled, the Data Catalog uses a customer-provided key to encrypt the password as part of @CreateConnection@ or @UpdateConnection@ and store it in the @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable catalog encryption or only password encryption.
dataCatalogEncryptionSettings ::
  DataCatalogEncryptionSettings
dataCatalogEncryptionSettings =
  DataCatalogEncryptionSettings'
    { _dcesEncryptionAtRest = Nothing,
      _dcesConnectionPasswordEncryption = Nothing
    }

-- | Specifies the encryption-at-rest configuration for the Data Catalog.
dcesEncryptionAtRest :: Lens' DataCatalogEncryptionSettings (Maybe EncryptionAtRest)
dcesEncryptionAtRest = lens _dcesEncryptionAtRest (\s a -> s {_dcesEncryptionAtRest = a})

-- | When connection password protection is enabled, the Data Catalog uses a customer-provided key to encrypt the password as part of @CreateConnection@ or @UpdateConnection@ and store it in the @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable catalog encryption or only password encryption.
dcesConnectionPasswordEncryption :: Lens' DataCatalogEncryptionSettings (Maybe ConnectionPasswordEncryption)
dcesConnectionPasswordEncryption = lens _dcesConnectionPasswordEncryption (\s a -> s {_dcesConnectionPasswordEncryption = a})

instance FromJSON DataCatalogEncryptionSettings where
  parseJSON =
    withObject
      "DataCatalogEncryptionSettings"
      ( \x ->
          DataCatalogEncryptionSettings'
            <$> (x .:? "EncryptionAtRest")
            <*> (x .:? "ConnectionPasswordEncryption")
      )

instance Hashable DataCatalogEncryptionSettings

instance NFData DataCatalogEncryptionSettings

instance ToJSON DataCatalogEncryptionSettings where
  toJSON DataCatalogEncryptionSettings' {..} =
    object
      ( catMaybes
          [ ("EncryptionAtRest" .=) <$> _dcesEncryptionAtRest,
            ("ConnectionPasswordEncryption" .=)
              <$> _dcesConnectionPasswordEncryption
          ]
      )
