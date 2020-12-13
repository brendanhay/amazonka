{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPNTunnelCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the VPN tunnel endpoint certificate.
module Network.AWS.EC2.ModifyVPNTunnelCertificate
  ( -- * Creating a request
    ModifyVPNTunnelCertificate (..),
    mkModifyVPNTunnelCertificate,

    -- ** Request lenses
    mvtcVPNTunnelOutsideIPAddress,
    mvtcVPNConnectionId,
    mvtcDryRun,

    -- * Destructuring the response
    ModifyVPNTunnelCertificateResponse (..),
    mkModifyVPNTunnelCertificateResponse,

    -- ** Response lenses
    mvtcrsVPNConnection,
    mvtcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVPNTunnelCertificate' smart constructor.
data ModifyVPNTunnelCertificate = ModifyVPNTunnelCertificate'
  { -- | The external IP address of the VPN tunnel.
    vpnTunnelOutsideIPAddress :: Lude.Text,
    -- | The ID of the AWS Site-to-Site VPN connection.
    vpnConnectionId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPNTunnelCertificate' with the minimum fields required to make a request.
--
-- * 'vpnTunnelOutsideIPAddress' - The external IP address of the VPN tunnel.
-- * 'vpnConnectionId' - The ID of the AWS Site-to-Site VPN connection.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyVPNTunnelCertificate ::
  -- | 'vpnTunnelOutsideIPAddress'
  Lude.Text ->
  -- | 'vpnConnectionId'
  Lude.Text ->
  ModifyVPNTunnelCertificate
mkModifyVPNTunnelCertificate
  pVPNTunnelOutsideIPAddress_
  pVPNConnectionId_ =
    ModifyVPNTunnelCertificate'
      { vpnTunnelOutsideIPAddress =
          pVPNTunnelOutsideIPAddress_,
        vpnConnectionId = pVPNConnectionId_,
        dryRun = Lude.Nothing
      }

-- | The external IP address of the VPN tunnel.
--
-- /Note:/ Consider using 'vpnTunnelOutsideIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtcVPNTunnelOutsideIPAddress :: Lens.Lens' ModifyVPNTunnelCertificate Lude.Text
mvtcVPNTunnelOutsideIPAddress = Lens.lens (vpnTunnelOutsideIPAddress :: ModifyVPNTunnelCertificate -> Lude.Text) (\s a -> s {vpnTunnelOutsideIPAddress = a} :: ModifyVPNTunnelCertificate)
{-# DEPRECATED mvtcVPNTunnelOutsideIPAddress "Use generic-lens or generic-optics with 'vpnTunnelOutsideIPAddress' instead." #-}

-- | The ID of the AWS Site-to-Site VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtcVPNConnectionId :: Lens.Lens' ModifyVPNTunnelCertificate Lude.Text
mvtcVPNConnectionId = Lens.lens (vpnConnectionId :: ModifyVPNTunnelCertificate -> Lude.Text) (\s a -> s {vpnConnectionId = a} :: ModifyVPNTunnelCertificate)
{-# DEPRECATED mvtcVPNConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtcDryRun :: Lens.Lens' ModifyVPNTunnelCertificate (Lude.Maybe Lude.Bool)
mvtcDryRun = Lens.lens (dryRun :: ModifyVPNTunnelCertificate -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVPNTunnelCertificate)
{-# DEPRECATED mvtcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyVPNTunnelCertificate where
  type
    Rs ModifyVPNTunnelCertificate =
      ModifyVPNTunnelCertificateResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVPNTunnelCertificateResponse'
            Lude.<$> (x Lude..@? "vpnConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVPNTunnelCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPNTunnelCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPNTunnelCertificate where
  toQuery ModifyVPNTunnelCertificate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyVpnTunnelCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpnTunnelOutsideIpAddress" Lude.=: vpnTunnelOutsideIPAddress,
        "VpnConnectionId" Lude.=: vpnConnectionId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyVPNTunnelCertificateResponse' smart constructor.
data ModifyVPNTunnelCertificateResponse = ModifyVPNTunnelCertificateResponse'
  { vpnConnection :: Lude.Maybe VPNConnection,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPNTunnelCertificateResponse' with the minimum fields required to make a request.
--
-- * 'vpnConnection' -
-- * 'responseStatus' - The response status code.
mkModifyVPNTunnelCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVPNTunnelCertificateResponse
mkModifyVPNTunnelCertificateResponse pResponseStatus_ =
  ModifyVPNTunnelCertificateResponse'
    { vpnConnection = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpnConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtcrsVPNConnection :: Lens.Lens' ModifyVPNTunnelCertificateResponse (Lude.Maybe VPNConnection)
mvtcrsVPNConnection = Lens.lens (vpnConnection :: ModifyVPNTunnelCertificateResponse -> Lude.Maybe VPNConnection) (\s a -> s {vpnConnection = a} :: ModifyVPNTunnelCertificateResponse)
{-# DEPRECATED mvtcrsVPNConnection "Use generic-lens or generic-optics with 'vpnConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtcrsResponseStatus :: Lens.Lens' ModifyVPNTunnelCertificateResponse Lude.Int
mvtcrsResponseStatus = Lens.lens (responseStatus :: ModifyVPNTunnelCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVPNTunnelCertificateResponse)
{-# DEPRECATED mvtcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
