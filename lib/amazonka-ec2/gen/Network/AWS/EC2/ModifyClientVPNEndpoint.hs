{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyClientVPNEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Client VPN endpoint. Modifying the DNS server resets existing client connections.
module Network.AWS.EC2.ModifyClientVPNEndpoint
  ( -- * Creating a request
    ModifyClientVPNEndpoint (..),
    mkModifyClientVPNEndpoint,

    -- ** Request lenses
    mcveSecurityGroupIds,
    mcveConnectionLogOptions,
    mcveSplitTunnel,
    mcveVPCId,
    mcveVPNPort,
    mcveDNSServers,
    mcveClientConnectOptions,
    mcveSelfServicePortal,
    mcveServerCertificateARN,
    mcveDescription,
    mcveDryRun,
    mcveClientVPNEndpointId,

    -- * Destructuring the response
    ModifyClientVPNEndpointResponse (..),
    mkModifyClientVPNEndpointResponse,

    -- ** Response lenses
    mcversReturn,
    mcversResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyClientVPNEndpoint' smart constructor.
data ModifyClientVPNEndpoint = ModifyClientVPNEndpoint'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    connectionLogOptions ::
      Lude.Maybe ConnectionLogOptions,
    splitTunnel :: Lude.Maybe Lude.Bool,
    vpcId :: Lude.Maybe Lude.Text,
    vpnPort :: Lude.Maybe Lude.Int,
    dnsServers ::
      Lude.Maybe DNSServersOptionsModifyStructure,
    clientConnectOptions ::
      Lude.Maybe ClientConnectOptions,
    selfServicePortal ::
      Lude.Maybe SelfServicePortal,
    serverCertificateARN ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    clientVPNEndpointId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClientVPNEndpoint' with the minimum fields required to make a request.
--
-- * 'clientConnectOptions' - The options for managing connection authorization for new client connections.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint to modify.
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
-- * 'dnsServers' - Information about the DNS servers to be used by Client VPN connections. A Client VPN endpoint can have up to two DNS servers.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'securityGroupIds' - The IDs of one or more security groups to apply to the target network.
-- * 'selfServicePortal' - Specify whether to enable the self-service portal for the Client VPN endpoint.
-- * 'serverCertificateARN' - The ARN of the server certificate to be used. The server certificate must be provisioned in AWS Certificate Manager (ACM).
-- * 'splitTunnel' - Indicates whether the VPN is split-tunnel.
--
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
-- * 'vpcId' - The ID of the VPC to associate with the Client VPN endpoint.
-- * 'vpnPort' - The port number to assign to the Client VPN endpoint for TCP and UDP traffic.
--
-- Valid Values: @443@ | @1194@
-- Default Value: @443@
mkModifyClientVPNEndpoint ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  ModifyClientVPNEndpoint
mkModifyClientVPNEndpoint pClientVPNEndpointId_ =
  ModifyClientVPNEndpoint'
    { securityGroupIds = Lude.Nothing,
      connectionLogOptions = Lude.Nothing,
      splitTunnel = Lude.Nothing,
      vpcId = Lude.Nothing,
      vpnPort = Lude.Nothing,
      dnsServers = Lude.Nothing,
      clientConnectOptions = Lude.Nothing,
      selfServicePortal = Lude.Nothing,
      serverCertificateARN = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      clientVPNEndpointId = pClientVPNEndpointId_
    }

-- | The IDs of one or more security groups to apply to the target network.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveSecurityGroupIds :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe [Lude.Text])
mcveSecurityGroupIds = Lens.lens (securityGroupIds :: ModifyClientVPNEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

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
mcveConnectionLogOptions :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe ConnectionLogOptions)
mcveConnectionLogOptions = Lens.lens (connectionLogOptions :: ModifyClientVPNEndpoint -> Lude.Maybe ConnectionLogOptions) (\s a -> s {connectionLogOptions = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveConnectionLogOptions "Use generic-lens or generic-optics with 'connectionLogOptions' instead." #-}

-- | Indicates whether the VPN is split-tunnel.
--
-- For information about split-tunnel VPN endpoints, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/split-tunnel-vpn.html Split-Tunnel AWS Client VPN Endpoint> in the /AWS Client VPN Administrator Guide/ .
--
-- /Note:/ Consider using 'splitTunnel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveSplitTunnel :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe Lude.Bool)
mcveSplitTunnel = Lens.lens (splitTunnel :: ModifyClientVPNEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {splitTunnel = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveSplitTunnel "Use generic-lens or generic-optics with 'splitTunnel' instead." #-}

-- | The ID of the VPC to associate with the Client VPN endpoint.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveVPCId :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe Lude.Text)
mcveVPCId = Lens.lens (vpcId :: ModifyClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The port number to assign to the Client VPN endpoint for TCP and UDP traffic.
--
-- Valid Values: @443@ | @1194@
-- Default Value: @443@
--
-- /Note:/ Consider using 'vpnPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveVPNPort :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe Lude.Int)
mcveVPNPort = Lens.lens (vpnPort :: ModifyClientVPNEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {vpnPort = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveVPNPort "Use generic-lens or generic-optics with 'vpnPort' instead." #-}

-- | Information about the DNS servers to be used by Client VPN connections. A Client VPN endpoint can have up to two DNS servers.
--
-- /Note:/ Consider using 'dnsServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveDNSServers :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe DNSServersOptionsModifyStructure)
mcveDNSServers = Lens.lens (dnsServers :: ModifyClientVPNEndpoint -> Lude.Maybe DNSServersOptionsModifyStructure) (\s a -> s {dnsServers = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveDNSServers "Use generic-lens or generic-optics with 'dnsServers' instead." #-}

-- | The options for managing connection authorization for new client connections.
--
-- /Note:/ Consider using 'clientConnectOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveClientConnectOptions :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe ClientConnectOptions)
mcveClientConnectOptions = Lens.lens (clientConnectOptions :: ModifyClientVPNEndpoint -> Lude.Maybe ClientConnectOptions) (\s a -> s {clientConnectOptions = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveClientConnectOptions "Use generic-lens or generic-optics with 'clientConnectOptions' instead." #-}

-- | Specify whether to enable the self-service portal for the Client VPN endpoint.
--
-- /Note:/ Consider using 'selfServicePortal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveSelfServicePortal :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe SelfServicePortal)
mcveSelfServicePortal = Lens.lens (selfServicePortal :: ModifyClientVPNEndpoint -> Lude.Maybe SelfServicePortal) (\s a -> s {selfServicePortal = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveSelfServicePortal "Use generic-lens or generic-optics with 'selfServicePortal' instead." #-}

-- | The ARN of the server certificate to be used. The server certificate must be provisioned in AWS Certificate Manager (ACM).
--
-- /Note:/ Consider using 'serverCertificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveServerCertificateARN :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe Lude.Text)
mcveServerCertificateARN = Lens.lens (serverCertificateARN :: ModifyClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {serverCertificateARN = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveServerCertificateARN "Use generic-lens or generic-optics with 'serverCertificateARN' instead." #-}

-- | A brief description of the Client VPN endpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveDescription :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe Lude.Text)
mcveDescription = Lens.lens (description :: ModifyClientVPNEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveDryRun :: Lens.Lens' ModifyClientVPNEndpoint (Lude.Maybe Lude.Bool)
mcveDryRun = Lens.lens (dryRun :: ModifyClientVPNEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Client VPN endpoint to modify.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcveClientVPNEndpointId :: Lens.Lens' ModifyClientVPNEndpoint Lude.Text
mcveClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: ModifyClientVPNEndpoint -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: ModifyClientVPNEndpoint)
{-# DEPRECATED mcveClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

instance Lude.AWSRequest ModifyClientVPNEndpoint where
  type Rs ModifyClientVPNEndpoint = ModifyClientVPNEndpointResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyClientVPNEndpointResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyClientVPNEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyClientVPNEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyClientVPNEndpoint where
  toQuery ModifyClientVPNEndpoint' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyClientVpnEndpoint" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "SecurityGroupId" Lude.<$> securityGroupIds),
        "ConnectionLogOptions" Lude.=: connectionLogOptions,
        "SplitTunnel" Lude.=: splitTunnel,
        "VpcId" Lude.=: vpcId,
        "VpnPort" Lude.=: vpnPort,
        "DnsServers" Lude.=: dnsServers,
        "ClientConnectOptions" Lude.=: clientConnectOptions,
        "SelfServicePortal" Lude.=: selfServicePortal,
        "ServerCertificateArn" Lude.=: serverCertificateARN,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId
      ]

-- | /See:/ 'mkModifyClientVPNEndpointResponse' smart constructor.
data ModifyClientVPNEndpointResponse = ModifyClientVPNEndpointResponse'
  { return ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'ModifyClientVPNEndpointResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkModifyClientVPNEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyClientVPNEndpointResponse
mkModifyClientVPNEndpointResponse pResponseStatus_ =
  ModifyClientVPNEndpointResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcversReturn :: Lens.Lens' ModifyClientVPNEndpointResponse (Lude.Maybe Lude.Bool)
mcversReturn = Lens.lens (return :: ModifyClientVPNEndpointResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ModifyClientVPNEndpointResponse)
{-# DEPRECATED mcversReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcversResponseStatus :: Lens.Lens' ModifyClientVPNEndpointResponse Lude.Int
mcversResponseStatus = Lens.lens (responseStatus :: ModifyClientVPNEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyClientVPNEndpointResponse)
{-# DEPRECATED mcversResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
