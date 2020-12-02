{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StorageConnector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StorageConnector where

import Network.AWS.AppStream.Types.StorageConnectorType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a connector that enables persistent storage for users.
--
--
--
-- /See:/ 'storageConnector' smart constructor.
data StorageConnector = StorageConnector'
  { _scDomains ::
      !(Maybe [Text]),
    _scResourceIdentifier :: !(Maybe Text),
    _scConnectorType :: !StorageConnectorType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StorageConnector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scDomains' - The names of the domains for the account.
--
-- * 'scResourceIdentifier' - The ARN of the storage connector.
--
-- * 'scConnectorType' - The type of storage connector.
storageConnector ::
  -- | 'scConnectorType'
  StorageConnectorType ->
  StorageConnector
storageConnector pConnectorType_ =
  StorageConnector'
    { _scDomains = Nothing,
      _scResourceIdentifier = Nothing,
      _scConnectorType = pConnectorType_
    }

-- | The names of the domains for the account.
scDomains :: Lens' StorageConnector [Text]
scDomains = lens _scDomains (\s a -> s {_scDomains = a}) . _Default . _Coerce

-- | The ARN of the storage connector.
scResourceIdentifier :: Lens' StorageConnector (Maybe Text)
scResourceIdentifier = lens _scResourceIdentifier (\s a -> s {_scResourceIdentifier = a})

-- | The type of storage connector.
scConnectorType :: Lens' StorageConnector StorageConnectorType
scConnectorType = lens _scConnectorType (\s a -> s {_scConnectorType = a})

instance FromJSON StorageConnector where
  parseJSON =
    withObject
      "StorageConnector"
      ( \x ->
          StorageConnector'
            <$> (x .:? "Domains" .!= mempty)
            <*> (x .:? "ResourceIdentifier")
            <*> (x .: "ConnectorType")
      )

instance Hashable StorageConnector

instance NFData StorageConnector

instance ToJSON StorageConnector where
  toJSON StorageConnector' {..} =
    object
      ( catMaybes
          [ ("Domains" .=) <$> _scDomains,
            ("ResourceIdentifier" .=) <$> _scResourceIdentifier,
            Just ("ConnectorType" .= _scConnectorType)
          ]
      )
