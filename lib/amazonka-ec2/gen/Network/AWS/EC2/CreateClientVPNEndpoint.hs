{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateClientVPNEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Client VPN endpoint. A Client VPN endpoint is the resource you create and configure to enable and manage client VPN sessions. It is the destination endpoint at which all client VPN sessions are terminated.
module Network.AWS.EC2.CreateClientVPNEndpoint
  ( -- * Creating a request
    CreateClientVPNEndpoint (..),
    mkCreateClientVPNEndpoint,

    -- ** Request lenses
    ccveSecurityGroupIds,
    ccveSplitTunnel,
    ccveClientToken,
    ccveTransportProtocol,
    ccveVPCId,
    ccveVPNPort,
    ccveTagSpecifications,
    ccveDNSServers,
    ccveClientConnectOptions,
    ccveSelfServicePortal,
    ccveDescription,
    ccveDryRun,
    ccveClientCidrBlock,
    ccveServerCertificateARN,
    ccveAuthenticationOptions,
    ccveConnectionLogOptions,

    -- * Destructuring the response
    CreateClientVPNEndpointResponse (..),
    mkCreateClientVPNEndpointResponse,

    -- ** Response lenses
    ccversStatus,
    ccversClientVPNEndpointId,
    ccversDNSName,
    ccversResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateClientVPNEndpoint' smart constructor.
data CreateClientVPNEndpoint = CreateClientVPNEndpoint'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    splitTunnel :: Lude.Maybe Lude.Bool,
    clientToken :: Lude.Maybe Lude.Text,
    transportProtocol ::
      Lude.Maybe TransportProtocol,
    vpcId :: Lude.Maybe Lude.Text,
    vpnPort :: Lude.Maybe Lude.Int,
    tagSpecifications ::
      Lude.Maybe [TagSpecification],
    dnsServers :: Lude.Maybe [Lude.Text],
    clientConnectOptions ::
      Lude.Maybe ClientConnectOptions,
    selfServicePortal ::
      Lude.Maybe SelfServicePortal,
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    clientCidrBlock :: Lude.Text,
    serverCertificateARN :: Lude.Text,
    authenticationOptions ::
      [ClientVPNAuthenticationRequest],
    connectionLogOptions ::
      ConnectionLogOptions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClientVPNEndpoint' with the minimum fields required to make a request.
--
-- * 'authenticationOptions' - Information about the authentication method to be used to authenticate clients.
-- * 'clientCidrBlock' - The IPv4 address range, in CIDR notation, from which to assign client IP addresses. The address range cannot overlap with the local CIDR of the VPC in which the associated subnet is located, or the routes that you add manually. The address range cannot be changed after the Client VPN endpoint has been created. The CIDR block should be /22 or greater.
-- * 'clientConnectOptions' - The options for managing connection authorization for new client connections.
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'connectionLogOptions' - Information about the client connection logging options.
--
-- If you enable client connection logging, data about client connections is sent to a Cloudwatch Logs log stream. The following information is logged:
--
--     * Client connection requests
--
--
--     * Client connection results (successful and unsuccessful)
--
--
--     * Reasons for unsuccessful client connection requests
--
--
--     * Client connection termination time
--
--
-- * 'description' - A brief description of the Client VPN endpoint.
-- * 'dnsServers' - Information about the DNS servers to be used for DNS resolution. A Client VPN endpoint can have up to two DNS servers. If no DNS server is specified, the DNS address configured on the device is used for the DNS server.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'securityGroupIds' - The IDs of one or more security groups to apply to the target network. You must also specify the ID of the VPC that contains the security groups.
-- * 'selfServicePortal' - Specify whether to enable the self-service portal for the Client VPN endpoint.
--
-- Default Value: @enabled@
-- * 'serverCertificateARN' - The ARN of the server certificate. For more information, see the <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide> .
-- * 'splitTunnel' - Indicates whether split-tunnel is enabled on the AWS Client VPN endpoint.
--
-- By default, split-tunnel on a VPN endpoint is disabled.
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
-- * 'tagSpecifications' - The tags to apply to the Client VPN endpoint during creation.
-- * 'transportProtocol' - The transport protocol to be used by the VPN session.
--
-- Default value: @udp@
-- * 'vpcId' - The ID of the VPC to associate with the Client VPN endpoint. If no security group IDs are specified in the request, the default security group for the VPC is applied.
-- * 'vpnPort' - The port number to assign to the Client VPN endpoint for TCP and UDP traffic.
--
-- Valid Values: @443@ | @1194@
-- Default Value: @443@
mkCreateClientVPNEndpoint ::
  -- | 'clientCidrBlock'
  Lude.Text ->
  -- | 'serverCertificateARN'
  Lude.Text ->
  -- | 'connectionLogOptions'
  ConnectionLogOptions ->
  CreateClientVPNEndpoint
mkCreateClientVPNEndpoint
  pClientCidrBlock_
  pServerCertificateARN_
  pConnectionLogOptions_ =
    CreateClientVPNEndpoint'
      { securityGroupIds = Lude.Nothing,
        splitTunnel = Lude.Nothing,
        clientToken = Lude.Nothing,
        transportProtocol = Lude.Nothing,
        vpcId = Lude.Nothing,
        vpnPort = Lude.Nothing,
        tagSpecifications = Lude.Nothing,
        dnsServers = Lude.Nothing,
        clientConnectOptions = Lude.Nothing,
        selfServicePortal = Lude.Nothing,
        description = Lude.Nothing,
        dryRun = Lude.Nothing,
        clientCidrBlock = pClientCidrBlock_,
        serverCertificateARN = pServerCertificateARN_,
        authenticationOptions = Lude.mempty,
        connectionLogOptions = pConnectionLogOptions_
      }

-- | The IDs of one or more security groups to apply to the target network. You must also specify the ID of the VPC that contains the security groups.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveSecurityGroupIds :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe [Lude.Text])
ccveSecurityGroupIds = Lens.lens (securityGroupIds :: CreateClientVPNEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | Indicates whether split-tunnel is enabled on the AWS Client VPN endpoint.
--
-- By default, split-tunnel on a VPN endpoint is disabled.
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
--
-- /Note:/ Consider using 'splitTunnel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveSplitTunnel :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe Lude.Bool)
ccveSplitTunnel = Lens.lens (splitTunnel :: CreateClientVPNEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {splitTunnel = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveSplitTunnel "Use generic-lens or generic-optics with 'splitTunnel' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveClientToken :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe Lude.Text)
ccveClientToken = Lens.lens (clientToken :: CreateClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The transport protocol to be used by the VPN session.
--
-- Default value: @udp@
--
-- /Note:/ Consider using 'transportProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveTransportProtocol :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe TransportProtocol)
ccveTransportProtocol = Lens.lens (transportProtocol :: CreateClientVPNEndpoint -> Lude.Maybe TransportProtocol) (\s a -> s {transportProtocol = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveTransportProtocol "Use generic-lens or generic-optics with 'transportProtocol' instead." #-}

-- | The ID of the VPC to associate with the Client VPN endpoint. If no security group IDs are specified in the request, the default security group for the VPC is applied.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveVPCId :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe Lude.Text)
ccveVPCId = Lens.lens (vpcId :: CreateClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The port number to assign to the Client VPN endpoint for TCP and UDP traffic.
--
-- Valid Values: @443@ | @1194@
-- Default Value: @443@
--
-- /Note:/ Consider using 'vpnPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveVPNPort :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe Lude.Int)
ccveVPNPort = Lens.lens (vpnPort :: CreateClientVPNEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {vpnPort = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveVPNPort "Use generic-lens or generic-optics with 'vpnPort' instead." #-}

-- | The tags to apply to the Client VPN endpoint during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveTagSpecifications :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe [TagSpecification])
ccveTagSpecifications = Lens.lens (tagSpecifications :: CreateClientVPNEndpoint -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Information about the DNS servers to be used for DNS resolution. A Client VPN endpoint can have up to two DNS servers. If no DNS server is specified, the DNS address configured on the device is used for the DNS server.
--
-- /Note:/ Consider using 'dnsServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveDNSServers :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe [Lude.Text])
ccveDNSServers = Lens.lens (dnsServers :: CreateClientVPNEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {dnsServers = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveDNSServers "Use generic-lens or generic-optics with 'dnsServers' instead." #-}

-- | The options for managing connection authorization for new client connections.
--
-- /Note:/ Consider using 'clientConnectOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveClientConnectOptions :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe ClientConnectOptions)
ccveClientConnectOptions = Lens.lens (clientConnectOptions :: CreateClientVPNEndpoint -> Lude.Maybe ClientConnectOptions) (\s a -> s {clientConnectOptions = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveClientConnectOptions "Use generic-lens or generic-optics with 'clientConnectOptions' instead." #-}

-- | Specify whether to enable the self-service portal for the Client VPN endpoint.
--
-- Default Value: @enabled@
--
-- /Note:/ Consider using 'selfServicePortal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveSelfServicePortal :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe SelfServicePortal)
ccveSelfServicePortal = Lens.lens (selfServicePortal :: CreateClientVPNEndpoint -> Lude.Maybe SelfServicePortal) (\s a -> s {selfServicePortal = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveSelfServicePortal "Use generic-lens or generic-optics with 'selfServicePortal' instead." #-}

-- | A brief description of the Client VPN endpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveDescription :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe Lude.Text)
ccveDescription = Lens.lens (description :: CreateClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveDryRun :: Lens.Lens' CreateClientVPNEndpoint (Lude.Maybe Lude.Bool)
ccveDryRun = Lens.lens (dryRun :: CreateClientVPNEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IPv4 address range, in CIDR notation, from which to assign client IP addresses. The address range cannot overlap with the local CIDR of the VPC in which the associated subnet is located, or the routes that you add manually. The address range cannot be changed after the Client VPN endpoint has been created. The CIDR block should be /22 or greater.
--
-- /Note:/ Consider using 'clientCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveClientCidrBlock :: Lens.Lens' CreateClientVPNEndpoint Lude.Text
ccveClientCidrBlock = Lens.lens (clientCidrBlock :: CreateClientVPNEndpoint -> Lude.Text) (\s a -> s {clientCidrBlock = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveClientCidrBlock "Use generic-lens or generic-optics with 'clientCidrBlock' instead." #-}

-- | The ARN of the server certificate. For more information, see the <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide> .
--
-- /Note:/ Consider using 'serverCertificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveServerCertificateARN :: Lens.Lens' CreateClientVPNEndpoint Lude.Text
ccveServerCertificateARN = Lens.lens (serverCertificateARN :: CreateClientVPNEndpoint -> Lude.Text) (\s a -> s {serverCertificateARN = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveServerCertificateARN "Use generic-lens or generic-optics with 'serverCertificateARN' instead." #-}

-- | Information about the authentication method to be used to authenticate clients.
--
-- /Note:/ Consider using 'authenticationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveAuthenticationOptions :: Lens.Lens' CreateClientVPNEndpoint [ClientVPNAuthenticationRequest]
ccveAuthenticationOptions = Lens.lens (authenticationOptions :: CreateClientVPNEndpoint -> [ClientVPNAuthenticationRequest]) (\s a -> s {authenticationOptions = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveAuthenticationOptions "Use generic-lens or generic-optics with 'authenticationOptions' instead." #-}

-- | Information about the client connection logging options.
--
-- If you enable client connection logging, data about client connections is sent to a Cloudwatch Logs log stream. The following information is logged:
--
--     * Client connection requests
--
--
--     * Client connection results (successful and unsuccessful)
--
--
--     * Reasons for unsuccessful client connection requests
--
--
--     * Client connection termination time
--
--
--
-- /Note:/ Consider using 'connectionLogOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccveConnectionLogOptions :: Lens.Lens' CreateClientVPNEndpoint ConnectionLogOptions
ccveConnectionLogOptions = Lens.lens (connectionLogOptions :: CreateClientVPNEndpoint -> ConnectionLogOptions) (\s a -> s {connectionLogOptions = a} :: CreateClientVPNEndpoint)
{-# DEPRECATED ccveConnectionLogOptions "Use generic-lens or generic-optics with 'connectionLogOptions' instead." #-}

instance Lude.AWSRequest CreateClientVPNEndpoint where
  type Rs CreateClientVPNEndpoint = CreateClientVPNEndpointResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateClientVPNEndpointResponse'
            Lude.<$> (x Lude..@? "status")
            Lude.<*> (x Lude..@? "clientVpnEndpointId")
            Lude.<*> (x Lude..@? "dnsName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateClientVPNEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateClientVPNEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateClientVPNEndpoint where
  toQuery CreateClientVPNEndpoint' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateClientVpnEndpoint" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "SecurityGroupId" Lude.<$> securityGroupIds),
        "SplitTunnel" Lude.=: splitTunnel,
        "ClientToken" Lude.=: clientToken,
        "TransportProtocol" Lude.=: transportProtocol,
        "VpcId" Lude.=: vpcId,
        "VpnPort" Lude.=: vpnPort,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        Lude.toQuery (Lude.toQueryList "DnsServers" Lude.<$> dnsServers),
        "ClientConnectOptions" Lude.=: clientConnectOptions,
        "SelfServicePortal" Lude.=: selfServicePortal,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "ClientCidrBlock" Lude.=: clientCidrBlock,
        "ServerCertificateArn" Lude.=: serverCertificateARN,
        Lude.toQueryList "Authentication" authenticationOptions,
        "ConnectionLogOptions" Lude.=: connectionLogOptions
      ]

-- | /See:/ 'mkCreateClientVPNEndpointResponse' smart constructor.
data CreateClientVPNEndpointResponse = CreateClientVPNEndpointResponse'
  { status ::
      Lude.Maybe
        ClientVPNEndpointStatus,
    clientVPNEndpointId ::
      Lude.Maybe Lude.Text,
    dnsName ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClientVPNEndpointResponse' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'dnsName' - The DNS name to be used by clients when establishing their VPN session.
-- * 'responseStatus' - The response status code.
-- * 'status' - The current state of the Client VPN endpoint.
mkCreateClientVPNEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClientVPNEndpointResponse
mkCreateClientVPNEndpointResponse pResponseStatus_ =
  CreateClientVPNEndpointResponse'
    { status = Lude.Nothing,
      clientVPNEndpointId = Lude.Nothing,
      dnsName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the Client VPN endpoint.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccversStatus :: Lens.Lens' CreateClientVPNEndpointResponse (Lude.Maybe ClientVPNEndpointStatus)
ccversStatus = Lens.lens (status :: CreateClientVPNEndpointResponse -> Lude.Maybe ClientVPNEndpointStatus) (\s a -> s {status = a} :: CreateClientVPNEndpointResponse)
{-# DEPRECATED ccversStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccversClientVPNEndpointId :: Lens.Lens' CreateClientVPNEndpointResponse (Lude.Maybe Lude.Text)
ccversClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: CreateClientVPNEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: CreateClientVPNEndpointResponse)
{-# DEPRECATED ccversClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | The DNS name to be used by clients when establishing their VPN session.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccversDNSName :: Lens.Lens' CreateClientVPNEndpointResponse (Lude.Maybe Lude.Text)
ccversDNSName = Lens.lens (dnsName :: CreateClientVPNEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: CreateClientVPNEndpointResponse)
{-# DEPRECATED ccversDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccversResponseStatus :: Lens.Lens' CreateClientVPNEndpointResponse Lude.Int
ccversResponseStatus = Lens.lens (responseStatus :: CreateClientVPNEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClientVPNEndpointResponse)
{-# DEPRECATED ccversResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
