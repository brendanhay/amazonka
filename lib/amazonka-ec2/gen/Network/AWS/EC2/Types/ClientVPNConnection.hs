{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNConnection where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVPNConnectionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a client connection.
--
--
--
-- /See:/ 'clientVPNConnection' smart constructor.
data ClientVPNConnection = ClientVPNConnection'
  { _cvcIngressPackets ::
      !(Maybe Text),
    _cvcStatus :: !(Maybe ClientVPNConnectionStatus),
    _cvcConnectionEndTime :: !(Maybe Text),
    _cvcCommonName :: !(Maybe Text),
    _cvcPostureComplianceStatuses :: !(Maybe [Text]),
    _cvcConnectionEstablishedTime :: !(Maybe Text),
    _cvcConnectionId :: !(Maybe Text),
    _cvcIngressBytes :: !(Maybe Text),
    _cvcUsername :: !(Maybe Text),
    _cvcEgressBytes :: !(Maybe Text),
    _cvcClientVPNEndpointId :: !(Maybe Text),
    _cvcClientIP :: !(Maybe Text),
    _cvcEgressPackets :: !(Maybe Text),
    _cvcTimestamp :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientVPNConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvcIngressPackets' - The number of packets sent by the client.
--
-- * 'cvcStatus' - The current state of the client connection.
--
-- * 'cvcConnectionEndTime' - The date and time the client connection was terminated.
--
-- * 'cvcCommonName' - The common name associated with the client. This is either the name of the client certificate, or the Active Directory user name.
--
-- * 'cvcPostureComplianceStatuses' - The statuses returned by the client connect handler for posture compliance, if applicable.
--
-- * 'cvcConnectionEstablishedTime' - The date and time the client connection was established.
--
-- * 'cvcConnectionId' - The ID of the client connection.
--
-- * 'cvcIngressBytes' - The number of bytes sent by the client.
--
-- * 'cvcUsername' - The username of the client who established the client connection. This information is only provided if Active Directory client authentication is used.
--
-- * 'cvcEgressBytes' - The number of bytes received by the client.
--
-- * 'cvcClientVPNEndpointId' - The ID of the Client VPN endpoint to which the client is connected.
--
-- * 'cvcClientIP' - The IP address of the client.
--
-- * 'cvcEgressPackets' - The number of packets received by the client.
--
-- * 'cvcTimestamp' - The current date and time.
clientVPNConnection ::
  ClientVPNConnection
clientVPNConnection =
  ClientVPNConnection'
    { _cvcIngressPackets = Nothing,
      _cvcStatus = Nothing,
      _cvcConnectionEndTime = Nothing,
      _cvcCommonName = Nothing,
      _cvcPostureComplianceStatuses = Nothing,
      _cvcConnectionEstablishedTime = Nothing,
      _cvcConnectionId = Nothing,
      _cvcIngressBytes = Nothing,
      _cvcUsername = Nothing,
      _cvcEgressBytes = Nothing,
      _cvcClientVPNEndpointId = Nothing,
      _cvcClientIP = Nothing,
      _cvcEgressPackets = Nothing,
      _cvcTimestamp = Nothing
    }

-- | The number of packets sent by the client.
cvcIngressPackets :: Lens' ClientVPNConnection (Maybe Text)
cvcIngressPackets = lens _cvcIngressPackets (\s a -> s {_cvcIngressPackets = a})

-- | The current state of the client connection.
cvcStatus :: Lens' ClientVPNConnection (Maybe ClientVPNConnectionStatus)
cvcStatus = lens _cvcStatus (\s a -> s {_cvcStatus = a})

-- | The date and time the client connection was terminated.
cvcConnectionEndTime :: Lens' ClientVPNConnection (Maybe Text)
cvcConnectionEndTime = lens _cvcConnectionEndTime (\s a -> s {_cvcConnectionEndTime = a})

-- | The common name associated with the client. This is either the name of the client certificate, or the Active Directory user name.
cvcCommonName :: Lens' ClientVPNConnection (Maybe Text)
cvcCommonName = lens _cvcCommonName (\s a -> s {_cvcCommonName = a})

-- | The statuses returned by the client connect handler for posture compliance, if applicable.
cvcPostureComplianceStatuses :: Lens' ClientVPNConnection [Text]
cvcPostureComplianceStatuses = lens _cvcPostureComplianceStatuses (\s a -> s {_cvcPostureComplianceStatuses = a}) . _Default . _Coerce

-- | The date and time the client connection was established.
cvcConnectionEstablishedTime :: Lens' ClientVPNConnection (Maybe Text)
cvcConnectionEstablishedTime = lens _cvcConnectionEstablishedTime (\s a -> s {_cvcConnectionEstablishedTime = a})

-- | The ID of the client connection.
cvcConnectionId :: Lens' ClientVPNConnection (Maybe Text)
cvcConnectionId = lens _cvcConnectionId (\s a -> s {_cvcConnectionId = a})

-- | The number of bytes sent by the client.
cvcIngressBytes :: Lens' ClientVPNConnection (Maybe Text)
cvcIngressBytes = lens _cvcIngressBytes (\s a -> s {_cvcIngressBytes = a})

-- | The username of the client who established the client connection. This information is only provided if Active Directory client authentication is used.
cvcUsername :: Lens' ClientVPNConnection (Maybe Text)
cvcUsername = lens _cvcUsername (\s a -> s {_cvcUsername = a})

-- | The number of bytes received by the client.
cvcEgressBytes :: Lens' ClientVPNConnection (Maybe Text)
cvcEgressBytes = lens _cvcEgressBytes (\s a -> s {_cvcEgressBytes = a})

-- | The ID of the Client VPN endpoint to which the client is connected.
cvcClientVPNEndpointId :: Lens' ClientVPNConnection (Maybe Text)
cvcClientVPNEndpointId = lens _cvcClientVPNEndpointId (\s a -> s {_cvcClientVPNEndpointId = a})

-- | The IP address of the client.
cvcClientIP :: Lens' ClientVPNConnection (Maybe Text)
cvcClientIP = lens _cvcClientIP (\s a -> s {_cvcClientIP = a})

-- | The number of packets received by the client.
cvcEgressPackets :: Lens' ClientVPNConnection (Maybe Text)
cvcEgressPackets = lens _cvcEgressPackets (\s a -> s {_cvcEgressPackets = a})

-- | The current date and time.
cvcTimestamp :: Lens' ClientVPNConnection (Maybe Text)
cvcTimestamp = lens _cvcTimestamp (\s a -> s {_cvcTimestamp = a})

instance FromXML ClientVPNConnection where
  parseXML x =
    ClientVPNConnection'
      <$> (x .@? "ingressPackets")
      <*> (x .@? "status")
      <*> (x .@? "connectionEndTime")
      <*> (x .@? "commonName")
      <*> ( x .@? "postureComplianceStatusSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "connectionEstablishedTime")
      <*> (x .@? "connectionId")
      <*> (x .@? "ingressBytes")
      <*> (x .@? "username")
      <*> (x .@? "egressBytes")
      <*> (x .@? "clientVpnEndpointId")
      <*> (x .@? "clientIp")
      <*> (x .@? "egressPackets")
      <*> (x .@? "timestamp")

instance Hashable ClientVPNConnection

instance NFData ClientVPNConnection
