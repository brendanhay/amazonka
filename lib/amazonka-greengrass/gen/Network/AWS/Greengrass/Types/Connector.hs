{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Connector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Connector where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a connector. Connectors run on the Greengrass core and contain built-in integration with local infrastructure, device protocols, AWS, and other cloud services.
--
-- /See:/ 'connector' smart constructor.
data Connector = Connector'
  { _conParameters ::
      !(Maybe (Map Text (Text))),
    _conConnectorARN :: !Text,
    _conId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Connector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'conParameters' - The parameters or configuration that the connector uses.
--
-- * 'conConnectorARN' - The ARN of the connector.
--
-- * 'conId' - A descriptive or arbitrary ID for the connector. This value must be unique within the connector definition version. Max length is 128 characters with pattern [a-zA-Z0-9:_-]+.
connector ::
  -- | 'conConnectorARN'
  Text ->
  -- | 'conId'
  Text ->
  Connector
connector pConnectorARN_ pId_ =
  Connector'
    { _conParameters = Nothing,
      _conConnectorARN = pConnectorARN_,
      _conId = pId_
    }

-- | The parameters or configuration that the connector uses.
conParameters :: Lens' Connector (HashMap Text (Text))
conParameters = lens _conParameters (\s a -> s {_conParameters = a}) . _Default . _Map

-- | The ARN of the connector.
conConnectorARN :: Lens' Connector Text
conConnectorARN = lens _conConnectorARN (\s a -> s {_conConnectorARN = a})

-- | A descriptive or arbitrary ID for the connector. This value must be unique within the connector definition version. Max length is 128 characters with pattern [a-zA-Z0-9:_-]+.
conId :: Lens' Connector Text
conId = lens _conId (\s a -> s {_conId = a})

instance FromJSON Connector where
  parseJSON =
    withObject
      "Connector"
      ( \x ->
          Connector'
            <$> (x .:? "Parameters" .!= mempty)
            <*> (x .: "ConnectorArn")
            <*> (x .: "Id")
      )

instance Hashable Connector

instance NFData Connector

instance ToJSON Connector where
  toJSON Connector' {..} =
    object
      ( catMaybes
          [ ("Parameters" .=) <$> _conParameters,
            Just ("ConnectorArn" .= _conConnectorARN),
            Just ("Id" .= _conId)
          ]
      )
