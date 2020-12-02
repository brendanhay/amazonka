{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ConnectorDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ConnectorDefinitionVersion where

import Network.AWS.Greengrass.Types.Connector
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the connector definition version, which is a container for connectors.
--
-- /See:/ 'connectorDefinitionVersion' smart constructor.
newtype ConnectorDefinitionVersion = ConnectorDefinitionVersion'
  { _cdvConnectors ::
      Maybe [Connector]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectorDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdvConnectors' - A list of references to connectors in this version, with their corresponding configuration settings.
connectorDefinitionVersion ::
  ConnectorDefinitionVersion
connectorDefinitionVersion =
  ConnectorDefinitionVersion' {_cdvConnectors = Nothing}

-- | A list of references to connectors in this version, with their corresponding configuration settings.
cdvConnectors :: Lens' ConnectorDefinitionVersion [Connector]
cdvConnectors = lens _cdvConnectors (\s a -> s {_cdvConnectors = a}) . _Default . _Coerce

instance FromJSON ConnectorDefinitionVersion where
  parseJSON =
    withObject
      "ConnectorDefinitionVersion"
      ( \x ->
          ConnectorDefinitionVersion' <$> (x .:? "Connectors" .!= mempty)
      )

instance Hashable ConnectorDefinitionVersion

instance NFData ConnectorDefinitionVersion

instance ToJSON ConnectorDefinitionVersion where
  toJSON ConnectorDefinitionVersion' {..} =
    object (catMaybes [("Connectors" .=) <$> _cdvConnectors])
