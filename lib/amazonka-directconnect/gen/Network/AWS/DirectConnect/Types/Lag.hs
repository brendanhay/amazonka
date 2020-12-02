{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Lag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Lag where

import Network.AWS.DirectConnect.Types.Connection
import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.LagState
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a link aggregation group (LAG).
--
--
--
-- /See:/ 'lag' smart constructor.
data Lag = Lag'
  { _lagLagId :: !(Maybe Text),
    _lagConnectionsBandwidth :: !(Maybe Text),
    _lagMinimumLinks :: !(Maybe Int),
    _lagLagName :: !(Maybe Text),
    _lagLocation :: !(Maybe Text),
    _lagConnections :: !(Maybe [Connection]),
    _lagAwsDevice :: !(Maybe Text),
    _lagHasLogicalRedundancy :: !(Maybe HasLogicalRedundancy),
    _lagAllowsHostedConnections :: !(Maybe Bool),
    _lagNumberOfConnections :: !(Maybe Int),
    _lagJumboFrameCapable :: !(Maybe Bool),
    _lagLagState :: !(Maybe LagState),
    _lagOwnerAccount :: !(Maybe Text),
    _lagRegion :: !(Maybe Text),
    _lagProviderName :: !(Maybe Text),
    _lagAwsDeviceV2 :: !(Maybe Text),
    _lagTags :: !(Maybe (List1 Tag))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Lag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lagLagId' - The ID of the LAG.
--
-- * 'lagConnectionsBandwidth' - The individual bandwidth of the physical connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
--
-- * 'lagMinimumLinks' - The minimum number of physical dedicated connections that must be operational for the LAG itself to be operational.
--
-- * 'lagLagName' - The name of the LAG.
--
-- * 'lagLocation' - The location of the LAG.
--
-- * 'lagConnections' - The connections bundled by the LAG.
--
-- * 'lagAwsDevice' - The AWS Direct Connect endpoint that hosts the LAG.
--
-- * 'lagHasLogicalRedundancy' - Indicates whether the LAG supports a secondary BGP peer in the same address family (IPv4/IPv6).
--
-- * 'lagAllowsHostedConnections' - Indicates whether the LAG can host other connections.
--
-- * 'lagNumberOfConnections' - The number of physical dedicated connections bundled by the LAG, up to a maximum of 10.
--
-- * 'lagJumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- * 'lagLagState' - The state of the LAG. The following are the possible values:     * @requested@ : The initial state of a LAG. The LAG stays in the requested state until the Letter of Authorization (LOA) is available.     * @pending@ : The LAG has been approved and is being initialized.     * @available@ : The network link is established and the LAG is ready for use.     * @down@ : The network link is down.     * @deleting@ : The LAG is being deleted.     * @deleted@ : The LAG is deleted.     * @unknown@ : The state of the LAG is not available.
--
-- * 'lagOwnerAccount' - The ID of the AWS account that owns the LAG.
--
-- * 'lagRegion' - The AWS Region where the connection is located.
--
-- * 'lagProviderName' - The name of the service provider associated with the LAG.
--
-- * 'lagAwsDeviceV2' - The AWS Direct Connect endpoint that hosts the LAG.
--
-- * 'lagTags' - The tags associated with the LAG.
lag ::
  Lag
lag =
  Lag'
    { _lagLagId = Nothing,
      _lagConnectionsBandwidth = Nothing,
      _lagMinimumLinks = Nothing,
      _lagLagName = Nothing,
      _lagLocation = Nothing,
      _lagConnections = Nothing,
      _lagAwsDevice = Nothing,
      _lagHasLogicalRedundancy = Nothing,
      _lagAllowsHostedConnections = Nothing,
      _lagNumberOfConnections = Nothing,
      _lagJumboFrameCapable = Nothing,
      _lagLagState = Nothing,
      _lagOwnerAccount = Nothing,
      _lagRegion = Nothing,
      _lagProviderName = Nothing,
      _lagAwsDeviceV2 = Nothing,
      _lagTags = Nothing
    }

-- | The ID of the LAG.
lagLagId :: Lens' Lag (Maybe Text)
lagLagId = lens _lagLagId (\s a -> s {_lagLagId = a})

-- | The individual bandwidth of the physical connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
lagConnectionsBandwidth :: Lens' Lag (Maybe Text)
lagConnectionsBandwidth = lens _lagConnectionsBandwidth (\s a -> s {_lagConnectionsBandwidth = a})

-- | The minimum number of physical dedicated connections that must be operational for the LAG itself to be operational.
lagMinimumLinks :: Lens' Lag (Maybe Int)
lagMinimumLinks = lens _lagMinimumLinks (\s a -> s {_lagMinimumLinks = a})

-- | The name of the LAG.
lagLagName :: Lens' Lag (Maybe Text)
lagLagName = lens _lagLagName (\s a -> s {_lagLagName = a})

-- | The location of the LAG.
lagLocation :: Lens' Lag (Maybe Text)
lagLocation = lens _lagLocation (\s a -> s {_lagLocation = a})

-- | The connections bundled by the LAG.
lagConnections :: Lens' Lag [Connection]
lagConnections = lens _lagConnections (\s a -> s {_lagConnections = a}) . _Default . _Coerce

-- | The AWS Direct Connect endpoint that hosts the LAG.
lagAwsDevice :: Lens' Lag (Maybe Text)
lagAwsDevice = lens _lagAwsDevice (\s a -> s {_lagAwsDevice = a})

-- | Indicates whether the LAG supports a secondary BGP peer in the same address family (IPv4/IPv6).
lagHasLogicalRedundancy :: Lens' Lag (Maybe HasLogicalRedundancy)
lagHasLogicalRedundancy = lens _lagHasLogicalRedundancy (\s a -> s {_lagHasLogicalRedundancy = a})

-- | Indicates whether the LAG can host other connections.
lagAllowsHostedConnections :: Lens' Lag (Maybe Bool)
lagAllowsHostedConnections = lens _lagAllowsHostedConnections (\s a -> s {_lagAllowsHostedConnections = a})

-- | The number of physical dedicated connections bundled by the LAG, up to a maximum of 10.
lagNumberOfConnections :: Lens' Lag (Maybe Int)
lagNumberOfConnections = lens _lagNumberOfConnections (\s a -> s {_lagNumberOfConnections = a})

-- | Indicates whether jumbo frames (9001 MTU) are supported.
lagJumboFrameCapable :: Lens' Lag (Maybe Bool)
lagJumboFrameCapable = lens _lagJumboFrameCapable (\s a -> s {_lagJumboFrameCapable = a})

-- | The state of the LAG. The following are the possible values:     * @requested@ : The initial state of a LAG. The LAG stays in the requested state until the Letter of Authorization (LOA) is available.     * @pending@ : The LAG has been approved and is being initialized.     * @available@ : The network link is established and the LAG is ready for use.     * @down@ : The network link is down.     * @deleting@ : The LAG is being deleted.     * @deleted@ : The LAG is deleted.     * @unknown@ : The state of the LAG is not available.
lagLagState :: Lens' Lag (Maybe LagState)
lagLagState = lens _lagLagState (\s a -> s {_lagLagState = a})

-- | The ID of the AWS account that owns the LAG.
lagOwnerAccount :: Lens' Lag (Maybe Text)
lagOwnerAccount = lens _lagOwnerAccount (\s a -> s {_lagOwnerAccount = a})

-- | The AWS Region where the connection is located.
lagRegion :: Lens' Lag (Maybe Text)
lagRegion = lens _lagRegion (\s a -> s {_lagRegion = a})

-- | The name of the service provider associated with the LAG.
lagProviderName :: Lens' Lag (Maybe Text)
lagProviderName = lens _lagProviderName (\s a -> s {_lagProviderName = a})

-- | The AWS Direct Connect endpoint that hosts the LAG.
lagAwsDeviceV2 :: Lens' Lag (Maybe Text)
lagAwsDeviceV2 = lens _lagAwsDeviceV2 (\s a -> s {_lagAwsDeviceV2 = a})

-- | The tags associated with the LAG.
lagTags :: Lens' Lag (Maybe (NonEmpty Tag))
lagTags = lens _lagTags (\s a -> s {_lagTags = a}) . mapping _List1

instance FromJSON Lag where
  parseJSON =
    withObject
      "Lag"
      ( \x ->
          Lag'
            <$> (x .:? "lagId")
            <*> (x .:? "connectionsBandwidth")
            <*> (x .:? "minimumLinks")
            <*> (x .:? "lagName")
            <*> (x .:? "location")
            <*> (x .:? "connections" .!= mempty)
            <*> (x .:? "awsDevice")
            <*> (x .:? "hasLogicalRedundancy")
            <*> (x .:? "allowsHostedConnections")
            <*> (x .:? "numberOfConnections")
            <*> (x .:? "jumboFrameCapable")
            <*> (x .:? "lagState")
            <*> (x .:? "ownerAccount")
            <*> (x .:? "region")
            <*> (x .:? "providerName")
            <*> (x .:? "awsDeviceV2")
            <*> (x .:? "tags")
      )

instance Hashable Lag

instance NFData Lag
