{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPNConnectionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the connection options for your Site-to-Site VPN connection.
--
-- When you modify the VPN connection options, the VPN endpoint IP addresses on the AWS side do not change, and the tunnel options do not change. Your VPN connection will be temporarily unavailable for a brief period while the VPN connection is updated.
module Network.AWS.EC2.ModifyVPNConnectionOptions
  ( -- * Creating a request
    ModifyVPNConnectionOptions (..),
    mkModifyVPNConnectionOptions,

    -- ** Request lenses
    mvcoRemoteIPv4NetworkCidr,
    mvcoLocalIPv4NetworkCidr,
    mvcoRemoteIPv6NetworkCidr,
    mvcoLocalIPv6NetworkCidr,
    mvcoDryRun,
    mvcoVPNConnectionId,

    -- * Destructuring the response
    ModifyVPNConnectionOptionsResponse (..),
    mkModifyVPNConnectionOptionsResponse,

    -- ** Response lenses
    mvcorsVPNConnection,
    mvcorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVPNConnectionOptions' smart constructor.
data ModifyVPNConnectionOptions = ModifyVPNConnectionOptions'
  { remoteIPv4NetworkCidr ::
      Lude.Maybe Lude.Text,
    localIPv4NetworkCidr ::
      Lude.Maybe Lude.Text,
    remoteIPv6NetworkCidr ::
      Lude.Maybe Lude.Text,
    localIPv6NetworkCidr ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    vpnConnectionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPNConnectionOptions' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'localIPv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @0.0.0.0/0@
-- * 'localIPv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @::/0@
-- * 'remoteIPv4NetworkCidr' - The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0/0@
-- * 'remoteIPv6NetworkCidr' - The IPv6 CIDR on the AWS side of the VPN connection.
--
-- Default: @::/0@
-- * 'vpnConnectionId' - The ID of the Site-to-Site VPN connection.
mkModifyVPNConnectionOptions ::
  -- | 'vpnConnectionId'
  Lude.Text ->
  ModifyVPNConnectionOptions
mkModifyVPNConnectionOptions pVPNConnectionId_ =
  ModifyVPNConnectionOptions'
    { remoteIPv4NetworkCidr = Lude.Nothing,
      localIPv4NetworkCidr = Lude.Nothing,
      remoteIPv6NetworkCidr = Lude.Nothing,
      localIPv6NetworkCidr = Lude.Nothing,
      dryRun = Lude.Nothing,
      vpnConnectionId = pVPNConnectionId_
    }

-- | The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0/0@
--
-- /Note:/ Consider using 'remoteIPv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoRemoteIPv4NetworkCidr :: Lens.Lens' ModifyVPNConnectionOptions (Lude.Maybe Lude.Text)
mvcoRemoteIPv4NetworkCidr = Lens.lens (remoteIPv4NetworkCidr :: ModifyVPNConnectionOptions -> Lude.Maybe Lude.Text) (\s a -> s {remoteIPv4NetworkCidr = a} :: ModifyVPNConnectionOptions)
{-# DEPRECATED mvcoRemoteIPv4NetworkCidr "Use generic-lens or generic-optics with 'remoteIPv4NetworkCidr' instead." #-}

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @0.0.0.0/0@
--
-- /Note:/ Consider using 'localIPv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoLocalIPv4NetworkCidr :: Lens.Lens' ModifyVPNConnectionOptions (Lude.Maybe Lude.Text)
mvcoLocalIPv4NetworkCidr = Lens.lens (localIPv4NetworkCidr :: ModifyVPNConnectionOptions -> Lude.Maybe Lude.Text) (\s a -> s {localIPv4NetworkCidr = a} :: ModifyVPNConnectionOptions)
{-# DEPRECATED mvcoLocalIPv4NetworkCidr "Use generic-lens or generic-optics with 'localIPv4NetworkCidr' instead." #-}

-- | The IPv6 CIDR on the AWS side of the VPN connection.
--
-- Default: @::/0@
--
-- /Note:/ Consider using 'remoteIPv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoRemoteIPv6NetworkCidr :: Lens.Lens' ModifyVPNConnectionOptions (Lude.Maybe Lude.Text)
mvcoRemoteIPv6NetworkCidr = Lens.lens (remoteIPv6NetworkCidr :: ModifyVPNConnectionOptions -> Lude.Maybe Lude.Text) (\s a -> s {remoteIPv6NetworkCidr = a} :: ModifyVPNConnectionOptions)
{-# DEPRECATED mvcoRemoteIPv6NetworkCidr "Use generic-lens or generic-optics with 'remoteIPv6NetworkCidr' instead." #-}

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @::/0@
--
-- /Note:/ Consider using 'localIPv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoLocalIPv6NetworkCidr :: Lens.Lens' ModifyVPNConnectionOptions (Lude.Maybe Lude.Text)
mvcoLocalIPv6NetworkCidr = Lens.lens (localIPv6NetworkCidr :: ModifyVPNConnectionOptions -> Lude.Maybe Lude.Text) (\s a -> s {localIPv6NetworkCidr = a} :: ModifyVPNConnectionOptions)
{-# DEPRECATED mvcoLocalIPv6NetworkCidr "Use generic-lens or generic-optics with 'localIPv6NetworkCidr' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoDryRun :: Lens.Lens' ModifyVPNConnectionOptions (Lude.Maybe Lude.Bool)
mvcoDryRun = Lens.lens (dryRun :: ModifyVPNConnectionOptions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVPNConnectionOptions)
{-# DEPRECATED mvcoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Site-to-Site VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoVPNConnectionId :: Lens.Lens' ModifyVPNConnectionOptions Lude.Text
mvcoVPNConnectionId = Lens.lens (vpnConnectionId :: ModifyVPNConnectionOptions -> Lude.Text) (\s a -> s {vpnConnectionId = a} :: ModifyVPNConnectionOptions)
{-# DEPRECATED mvcoVPNConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead." #-}

instance Lude.AWSRequest ModifyVPNConnectionOptions where
  type
    Rs ModifyVPNConnectionOptions =
      ModifyVPNConnectionOptionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVPNConnectionOptionsResponse'
            Lude.<$> (x Lude..@? "vpnConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVPNConnectionOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPNConnectionOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPNConnectionOptions where
  toQuery ModifyVPNConnectionOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyVpnConnectionOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "RemoteIpv4NetworkCidr" Lude.=: remoteIPv4NetworkCidr,
        "LocalIpv4NetworkCidr" Lude.=: localIPv4NetworkCidr,
        "RemoteIpv6NetworkCidr" Lude.=: remoteIPv6NetworkCidr,
        "LocalIpv6NetworkCidr" Lude.=: localIPv6NetworkCidr,
        "DryRun" Lude.=: dryRun,
        "VpnConnectionId" Lude.=: vpnConnectionId
      ]

-- | /See:/ 'mkModifyVPNConnectionOptionsResponse' smart constructor.
data ModifyVPNConnectionOptionsResponse = ModifyVPNConnectionOptionsResponse'
  { vpnConnection ::
      Lude.Maybe
        VPNConnection,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPNConnectionOptionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpnConnection' - Undocumented field.
mkModifyVPNConnectionOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVPNConnectionOptionsResponse
mkModifyVPNConnectionOptionsResponse pResponseStatus_ =
  ModifyVPNConnectionOptionsResponse'
    { vpnConnection = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpnConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcorsVPNConnection :: Lens.Lens' ModifyVPNConnectionOptionsResponse (Lude.Maybe VPNConnection)
mvcorsVPNConnection = Lens.lens (vpnConnection :: ModifyVPNConnectionOptionsResponse -> Lude.Maybe VPNConnection) (\s a -> s {vpnConnection = a} :: ModifyVPNConnectionOptionsResponse)
{-# DEPRECATED mvcorsVPNConnection "Use generic-lens or generic-optics with 'vpnConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcorsResponseStatus :: Lens.Lens' ModifyVPNConnectionOptionsResponse Lude.Int
mvcorsResponseStatus = Lens.lens (responseStatus :: ModifyVPNConnectionOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVPNConnectionOptionsResponse)
{-# DEPRECATED mvcorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
