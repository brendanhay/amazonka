{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Connector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.Connector where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.ConnectorCapability
import Network.AWS.SMS.Types.ConnectorStatus
import Network.AWS.SMS.Types.VMManagerType

-- | Represents a connector.
--
--
--
-- /See:/ 'connector' smart constructor.
data Connector = Connector'
  { _cStatus :: !(Maybe ConnectorStatus),
    _cVmManagerName :: !(Maybe Text),
    _cIpAddress :: !(Maybe Text),
    _cVmManagerId :: !(Maybe Text),
    _cVmManagerType :: !(Maybe VMManagerType),
    _cConnectorId :: !(Maybe Text),
    _cAssociatedOn :: !(Maybe POSIX),
    _cMacAddress :: !(Maybe Text),
    _cVersion :: !(Maybe Text),
    _cCapabilityList :: !(Maybe [ConnectorCapability])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Connector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The status of the connector.
--
-- * 'cVmManagerName' - The name of the VM manager.
--
-- * 'cIpAddress' - The IP address of the connector.
--
-- * 'cVmManagerId' - The ID of the VM manager.
--
-- * 'cVmManagerType' - The VM management product.
--
-- * 'cConnectorId' - The ID of the connector.
--
-- * 'cAssociatedOn' - The time the connector was associated.
--
-- * 'cMacAddress' - The MAC address of the connector.
--
-- * 'cVersion' - The connector version.
--
-- * 'cCapabilityList' - The capabilities of the connector.
connector ::
  Connector
connector =
  Connector'
    { _cStatus = Nothing,
      _cVmManagerName = Nothing,
      _cIpAddress = Nothing,
      _cVmManagerId = Nothing,
      _cVmManagerType = Nothing,
      _cConnectorId = Nothing,
      _cAssociatedOn = Nothing,
      _cMacAddress = Nothing,
      _cVersion = Nothing,
      _cCapabilityList = Nothing
    }

-- | The status of the connector.
cStatus :: Lens' Connector (Maybe ConnectorStatus)
cStatus = lens _cStatus (\s a -> s {_cStatus = a})

-- | The name of the VM manager.
cVmManagerName :: Lens' Connector (Maybe Text)
cVmManagerName = lens _cVmManagerName (\s a -> s {_cVmManagerName = a})

-- | The IP address of the connector.
cIpAddress :: Lens' Connector (Maybe Text)
cIpAddress = lens _cIpAddress (\s a -> s {_cIpAddress = a})

-- | The ID of the VM manager.
cVmManagerId :: Lens' Connector (Maybe Text)
cVmManagerId = lens _cVmManagerId (\s a -> s {_cVmManagerId = a})

-- | The VM management product.
cVmManagerType :: Lens' Connector (Maybe VMManagerType)
cVmManagerType = lens _cVmManagerType (\s a -> s {_cVmManagerType = a})

-- | The ID of the connector.
cConnectorId :: Lens' Connector (Maybe Text)
cConnectorId = lens _cConnectorId (\s a -> s {_cConnectorId = a})

-- | The time the connector was associated.
cAssociatedOn :: Lens' Connector (Maybe UTCTime)
cAssociatedOn = lens _cAssociatedOn (\s a -> s {_cAssociatedOn = a}) . mapping _Time

-- | The MAC address of the connector.
cMacAddress :: Lens' Connector (Maybe Text)
cMacAddress = lens _cMacAddress (\s a -> s {_cMacAddress = a})

-- | The connector version.
cVersion :: Lens' Connector (Maybe Text)
cVersion = lens _cVersion (\s a -> s {_cVersion = a})

-- | The capabilities of the connector.
cCapabilityList :: Lens' Connector [ConnectorCapability]
cCapabilityList = lens _cCapabilityList (\s a -> s {_cCapabilityList = a}) . _Default . _Coerce

instance FromJSON Connector where
  parseJSON =
    withObject
      "Connector"
      ( \x ->
          Connector'
            <$> (x .:? "status")
            <*> (x .:? "vmManagerName")
            <*> (x .:? "ipAddress")
            <*> (x .:? "vmManagerId")
            <*> (x .:? "vmManagerType")
            <*> (x .:? "connectorId")
            <*> (x .:? "associatedOn")
            <*> (x .:? "macAddress")
            <*> (x .:? "version")
            <*> (x .:? "capabilityList" .!= mempty)
      )

instance Hashable Connector

instance NFData Connector
