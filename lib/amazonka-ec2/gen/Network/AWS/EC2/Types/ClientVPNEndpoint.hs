{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNEndpoint where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AssociatedTargetNetwork
import Network.AWS.EC2.Types.ClientConnectResponseOptions
import Network.AWS.EC2.Types.ClientVPNAuthentication
import Network.AWS.EC2.Types.ClientVPNEndpointStatus
import Network.AWS.EC2.Types.ConnectionLogResponseOptions
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransportProtocol
import Network.AWS.EC2.Types.VPNProtocol
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Client VPN endpoint.
--
--
--
-- /See:/ 'clientVPNEndpoint' smart constructor.
data ClientVPNEndpoint = ClientVPNEndpoint'
  { _cveCreationTime ::
      !(Maybe Text),
    _cveStatus :: !(Maybe ClientVPNEndpointStatus),
    _cveAssociatedTargetNetworks ::
      !(Maybe [AssociatedTargetNetwork]),
    _cveSecurityGroupIds :: !(Maybe [Text]),
    _cveConnectionLogOptions ::
      !(Maybe ConnectionLogResponseOptions),
    _cveSplitTunnel :: !(Maybe Bool),
    _cveTransportProtocol :: !(Maybe TransportProtocol),
    _cveVPCId :: !(Maybe Text),
    _cveVPNPort :: !(Maybe Int),
    _cveDeletionTime :: !(Maybe Text),
    _cveClientCidrBlock :: !(Maybe Text),
    _cveDNSServers :: !(Maybe [Text]),
    _cveClientVPNEndpointId :: !(Maybe Text),
    _cveClientConnectOptions ::
      !(Maybe ClientConnectResponseOptions),
    _cveServerCertificateARN :: !(Maybe Text),
    _cveAuthenticationOptions ::
      !(Maybe [ClientVPNAuthentication]),
    _cveSelfServicePortalURL :: !(Maybe Text),
    _cveDescription :: !(Maybe Text),
    _cveDNSName :: !(Maybe Text),
    _cveVPNProtocol :: !(Maybe VPNProtocol),
    _cveTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientVPNEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cveCreationTime' - The date and time the Client VPN endpoint was created.
--
-- * 'cveStatus' - The current state of the Client VPN endpoint.
--
-- * 'cveAssociatedTargetNetworks' - Information about the associated target networks. A target network is a subnet in a VPC.
--
-- * 'cveSecurityGroupIds' - The IDs of the security groups for the target network.
--
-- * 'cveConnectionLogOptions' - Information about the client connection logging options for the Client VPN endpoint.
--
-- * 'cveSplitTunnel' - Indicates whether split-tunnel is enabled in the AWS Client VPN endpoint. For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
--
-- * 'cveTransportProtocol' - The transport protocol used by the Client VPN endpoint.
--
-- * 'cveVPCId' - The ID of the VPC.
--
-- * 'cveVPNPort' - The port number for the Client VPN endpoint.
--
-- * 'cveDeletionTime' - The date and time the Client VPN endpoint was deleted, if applicable.
--
-- * 'cveClientCidrBlock' - The IPv4 address range, in CIDR notation, from which client IP addresses are assigned.
--
-- * 'cveDNSServers' - Information about the DNS servers to be used for DNS resolution.
--
-- * 'cveClientVPNEndpointId' - The ID of the Client VPN endpoint.
--
-- * 'cveClientConnectOptions' - The options for managing connection authorization for new client connections.
--
-- * 'cveServerCertificateARN' - The ARN of the server certificate.
--
-- * 'cveAuthenticationOptions' - Information about the authentication method used by the Client VPN endpoint.
--
-- * 'cveSelfServicePortalURL' - The URL of the self-service portal.
--
-- * 'cveDescription' - A brief description of the endpoint.
--
-- * 'cveDNSName' - The DNS name to be used by clients when connecting to the Client VPN endpoint.
--
-- * 'cveVPNProtocol' - The protocol used by the VPN session.
--
-- * 'cveTags' - Any tags assigned to the Client VPN endpoint.
clientVPNEndpoint ::
  ClientVPNEndpoint
clientVPNEndpoint =
  ClientVPNEndpoint'
    { _cveCreationTime = Nothing,
      _cveStatus = Nothing,
      _cveAssociatedTargetNetworks = Nothing,
      _cveSecurityGroupIds = Nothing,
      _cveConnectionLogOptions = Nothing,
      _cveSplitTunnel = Nothing,
      _cveTransportProtocol = Nothing,
      _cveVPCId = Nothing,
      _cveVPNPort = Nothing,
      _cveDeletionTime = Nothing,
      _cveClientCidrBlock = Nothing,
      _cveDNSServers = Nothing,
      _cveClientVPNEndpointId = Nothing,
      _cveClientConnectOptions = Nothing,
      _cveServerCertificateARN = Nothing,
      _cveAuthenticationOptions = Nothing,
      _cveSelfServicePortalURL = Nothing,
      _cveDescription = Nothing,
      _cveDNSName = Nothing,
      _cveVPNProtocol = Nothing,
      _cveTags = Nothing
    }

-- | The date and time the Client VPN endpoint was created.
cveCreationTime :: Lens' ClientVPNEndpoint (Maybe Text)
cveCreationTime = lens _cveCreationTime (\s a -> s {_cveCreationTime = a})

-- | The current state of the Client VPN endpoint.
cveStatus :: Lens' ClientVPNEndpoint (Maybe ClientVPNEndpointStatus)
cveStatus = lens _cveStatus (\s a -> s {_cveStatus = a})

-- | Information about the associated target networks. A target network is a subnet in a VPC.
cveAssociatedTargetNetworks :: Lens' ClientVPNEndpoint [AssociatedTargetNetwork]
cveAssociatedTargetNetworks = lens _cveAssociatedTargetNetworks (\s a -> s {_cveAssociatedTargetNetworks = a}) . _Default . _Coerce

-- | The IDs of the security groups for the target network.
cveSecurityGroupIds :: Lens' ClientVPNEndpoint [Text]
cveSecurityGroupIds = lens _cveSecurityGroupIds (\s a -> s {_cveSecurityGroupIds = a}) . _Default . _Coerce

-- | Information about the client connection logging options for the Client VPN endpoint.
cveConnectionLogOptions :: Lens' ClientVPNEndpoint (Maybe ConnectionLogResponseOptions)
cveConnectionLogOptions = lens _cveConnectionLogOptions (\s a -> s {_cveConnectionLogOptions = a})

-- | Indicates whether split-tunnel is enabled in the AWS Client VPN endpoint. For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
cveSplitTunnel :: Lens' ClientVPNEndpoint (Maybe Bool)
cveSplitTunnel = lens _cveSplitTunnel (\s a -> s {_cveSplitTunnel = a})

-- | The transport protocol used by the Client VPN endpoint.
cveTransportProtocol :: Lens' ClientVPNEndpoint (Maybe TransportProtocol)
cveTransportProtocol = lens _cveTransportProtocol (\s a -> s {_cveTransportProtocol = a})

-- | The ID of the VPC.
cveVPCId :: Lens' ClientVPNEndpoint (Maybe Text)
cveVPCId = lens _cveVPCId (\s a -> s {_cveVPCId = a})

-- | The port number for the Client VPN endpoint.
cveVPNPort :: Lens' ClientVPNEndpoint (Maybe Int)
cveVPNPort = lens _cveVPNPort (\s a -> s {_cveVPNPort = a})

-- | The date and time the Client VPN endpoint was deleted, if applicable.
cveDeletionTime :: Lens' ClientVPNEndpoint (Maybe Text)
cveDeletionTime = lens _cveDeletionTime (\s a -> s {_cveDeletionTime = a})

-- | The IPv4 address range, in CIDR notation, from which client IP addresses are assigned.
cveClientCidrBlock :: Lens' ClientVPNEndpoint (Maybe Text)
cveClientCidrBlock = lens _cveClientCidrBlock (\s a -> s {_cveClientCidrBlock = a})

-- | Information about the DNS servers to be used for DNS resolution.
cveDNSServers :: Lens' ClientVPNEndpoint [Text]
cveDNSServers = lens _cveDNSServers (\s a -> s {_cveDNSServers = a}) . _Default . _Coerce

-- | The ID of the Client VPN endpoint.
cveClientVPNEndpointId :: Lens' ClientVPNEndpoint (Maybe Text)
cveClientVPNEndpointId = lens _cveClientVPNEndpointId (\s a -> s {_cveClientVPNEndpointId = a})

-- | The options for managing connection authorization for new client connections.
cveClientConnectOptions :: Lens' ClientVPNEndpoint (Maybe ClientConnectResponseOptions)
cveClientConnectOptions = lens _cveClientConnectOptions (\s a -> s {_cveClientConnectOptions = a})

-- | The ARN of the server certificate.
cveServerCertificateARN :: Lens' ClientVPNEndpoint (Maybe Text)
cveServerCertificateARN = lens _cveServerCertificateARN (\s a -> s {_cveServerCertificateARN = a})

-- | Information about the authentication method used by the Client VPN endpoint.
cveAuthenticationOptions :: Lens' ClientVPNEndpoint [ClientVPNAuthentication]
cveAuthenticationOptions = lens _cveAuthenticationOptions (\s a -> s {_cveAuthenticationOptions = a}) . _Default . _Coerce

-- | The URL of the self-service portal.
cveSelfServicePortalURL :: Lens' ClientVPNEndpoint (Maybe Text)
cveSelfServicePortalURL = lens _cveSelfServicePortalURL (\s a -> s {_cveSelfServicePortalURL = a})

-- | A brief description of the endpoint.
cveDescription :: Lens' ClientVPNEndpoint (Maybe Text)
cveDescription = lens _cveDescription (\s a -> s {_cveDescription = a})

-- | The DNS name to be used by clients when connecting to the Client VPN endpoint.
cveDNSName :: Lens' ClientVPNEndpoint (Maybe Text)
cveDNSName = lens _cveDNSName (\s a -> s {_cveDNSName = a})

-- | The protocol used by the VPN session.
cveVPNProtocol :: Lens' ClientVPNEndpoint (Maybe VPNProtocol)
cveVPNProtocol = lens _cveVPNProtocol (\s a -> s {_cveVPNProtocol = a})

-- | Any tags assigned to the Client VPN endpoint.
cveTags :: Lens' ClientVPNEndpoint [Tag]
cveTags = lens _cveTags (\s a -> s {_cveTags = a}) . _Default . _Coerce

instance FromXML ClientVPNEndpoint where
  parseXML x =
    ClientVPNEndpoint'
      <$> (x .@? "creationTime")
      <*> (x .@? "status")
      <*> ( x .@? "associatedTargetNetwork" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> ( x .@? "securityGroupIdSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "connectionLogOptions")
      <*> (x .@? "splitTunnel")
      <*> (x .@? "transportProtocol")
      <*> (x .@? "vpcId")
      <*> (x .@? "vpnPort")
      <*> (x .@? "deletionTime")
      <*> (x .@? "clientCidrBlock")
      <*> (x .@? "dnsServer" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "clientVpnEndpointId")
      <*> (x .@? "clientConnectOptions")
      <*> (x .@? "serverCertificateArn")
      <*> ( x .@? "authenticationOptions" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "selfServicePortalUrl")
      <*> (x .@? "description")
      <*> (x .@? "dnsName")
      <*> (x .@? "vpnProtocol")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable ClientVPNEndpoint

instance NFData ClientVPNEndpoint
