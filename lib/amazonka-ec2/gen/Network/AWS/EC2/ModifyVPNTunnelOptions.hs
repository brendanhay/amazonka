{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPNTunnelOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the options for a VPN tunnel in an AWS Site-to-Site VPN connection. You can modify multiple options for a tunnel in a single request, but you can only modify one tunnel at a time. For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPNTunnels.html Site-to-Site VPN Tunnel Options for Your Site-to-Site VPN Connection> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.ModifyVPNTunnelOptions
  ( -- * Creating a request
    ModifyVPNTunnelOptions (..),
    mkModifyVPNTunnelOptions,

    -- ** Request lenses
    mvtoVPNTunnelOutsideIPAddress,
    mvtoTunnelOptions,
    mvtoVPNConnectionId,
    mvtoDryRun,

    -- * Destructuring the response
    ModifyVPNTunnelOptionsResponse (..),
    mkModifyVPNTunnelOptionsResponse,

    -- ** Response lenses
    mvtorsVPNConnection,
    mvtorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVPNTunnelOptions' smart constructor.
data ModifyVPNTunnelOptions = ModifyVPNTunnelOptions'
  { -- | The external IP address of the VPN tunnel.
    vpnTunnelOutsideIPAddress :: Lude.Text,
    -- | The tunnel options to modify.
    tunnelOptions :: ModifyVPNTunnelOptionsSpecification,
    -- | The ID of the AWS Site-to-Site VPN connection.
    vpnConnectionId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPNTunnelOptions' with the minimum fields required to make a request.
--
-- * 'vpnTunnelOutsideIPAddress' - The external IP address of the VPN tunnel.
-- * 'tunnelOptions' - The tunnel options to modify.
-- * 'vpnConnectionId' - The ID of the AWS Site-to-Site VPN connection.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyVPNTunnelOptions ::
  -- | 'vpnTunnelOutsideIPAddress'
  Lude.Text ->
  -- | 'tunnelOptions'
  ModifyVPNTunnelOptionsSpecification ->
  -- | 'vpnConnectionId'
  Lude.Text ->
  ModifyVPNTunnelOptions
mkModifyVPNTunnelOptions
  pVPNTunnelOutsideIPAddress_
  pTunnelOptions_
  pVPNConnectionId_ =
    ModifyVPNTunnelOptions'
      { vpnTunnelOutsideIPAddress =
          pVPNTunnelOutsideIPAddress_,
        tunnelOptions = pTunnelOptions_,
        vpnConnectionId = pVPNConnectionId_,
        dryRun = Lude.Nothing
      }

-- | The external IP address of the VPN tunnel.
--
-- /Note:/ Consider using 'vpnTunnelOutsideIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtoVPNTunnelOutsideIPAddress :: Lens.Lens' ModifyVPNTunnelOptions Lude.Text
mvtoVPNTunnelOutsideIPAddress = Lens.lens (vpnTunnelOutsideIPAddress :: ModifyVPNTunnelOptions -> Lude.Text) (\s a -> s {vpnTunnelOutsideIPAddress = a} :: ModifyVPNTunnelOptions)
{-# DEPRECATED mvtoVPNTunnelOutsideIPAddress "Use generic-lens or generic-optics with 'vpnTunnelOutsideIPAddress' instead." #-}

-- | The tunnel options to modify.
--
-- /Note:/ Consider using 'tunnelOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtoTunnelOptions :: Lens.Lens' ModifyVPNTunnelOptions ModifyVPNTunnelOptionsSpecification
mvtoTunnelOptions = Lens.lens (tunnelOptions :: ModifyVPNTunnelOptions -> ModifyVPNTunnelOptionsSpecification) (\s a -> s {tunnelOptions = a} :: ModifyVPNTunnelOptions)
{-# DEPRECATED mvtoTunnelOptions "Use generic-lens or generic-optics with 'tunnelOptions' instead." #-}

-- | The ID of the AWS Site-to-Site VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtoVPNConnectionId :: Lens.Lens' ModifyVPNTunnelOptions Lude.Text
mvtoVPNConnectionId = Lens.lens (vpnConnectionId :: ModifyVPNTunnelOptions -> Lude.Text) (\s a -> s {vpnConnectionId = a} :: ModifyVPNTunnelOptions)
{-# DEPRECATED mvtoVPNConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtoDryRun :: Lens.Lens' ModifyVPNTunnelOptions (Lude.Maybe Lude.Bool)
mvtoDryRun = Lens.lens (dryRun :: ModifyVPNTunnelOptions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVPNTunnelOptions)
{-# DEPRECATED mvtoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyVPNTunnelOptions where
  type Rs ModifyVPNTunnelOptions = ModifyVPNTunnelOptionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVPNTunnelOptionsResponse'
            Lude.<$> (x Lude..@? "vpnConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVPNTunnelOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPNTunnelOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPNTunnelOptions where
  toQuery ModifyVPNTunnelOptions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyVpnTunnelOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpnTunnelOutsideIpAddress" Lude.=: vpnTunnelOutsideIPAddress,
        "TunnelOptions" Lude.=: tunnelOptions,
        "VpnConnectionId" Lude.=: vpnConnectionId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyVPNTunnelOptionsResponse' smart constructor.
data ModifyVPNTunnelOptionsResponse = ModifyVPNTunnelOptionsResponse'
  { vpnConnection :: Lude.Maybe VPNConnection,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPNTunnelOptionsResponse' with the minimum fields required to make a request.
--
-- * 'vpnConnection' -
-- * 'responseStatus' - The response status code.
mkModifyVPNTunnelOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVPNTunnelOptionsResponse
mkModifyVPNTunnelOptionsResponse pResponseStatus_ =
  ModifyVPNTunnelOptionsResponse'
    { vpnConnection = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpnConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtorsVPNConnection :: Lens.Lens' ModifyVPNTunnelOptionsResponse (Lude.Maybe VPNConnection)
mvtorsVPNConnection = Lens.lens (vpnConnection :: ModifyVPNTunnelOptionsResponse -> Lude.Maybe VPNConnection) (\s a -> s {vpnConnection = a} :: ModifyVPNTunnelOptionsResponse)
{-# DEPRECATED mvtorsVPNConnection "Use generic-lens or generic-optics with 'vpnConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtorsResponseStatus :: Lens.Lens' ModifyVPNTunnelOptionsResponse Lude.Int
mvtorsResponseStatus = Lens.lens (responseStatus :: ModifyVPNTunnelOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVPNTunnelOptionsResponse)
{-# DEPRECATED mvtorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
