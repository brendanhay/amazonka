{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpnConnectionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the connection options for your Site-to-Site VPN connection.
--
-- When you modify the VPN connection options, the VPN endpoint IP addresses on the AWS side do not change, and the tunnel options do not change. Your VPN connection will be temporarily unavailable for a brief period while the VPN connection is updated.
module Network.AWS.EC2.ModifyVpnConnectionOptions
    (
    -- * Creating a request
      ModifyVpnConnectionOptions (..)
    , mkModifyVpnConnectionOptions
    -- ** Request lenses
    , mvcoVpnConnectionId
    , mvcoDryRun
    , mvcoLocalIpv4NetworkCidr
    , mvcoLocalIpv6NetworkCidr
    , mvcoRemoteIpv4NetworkCidr
    , mvcoRemoteIpv6NetworkCidr

    -- * Destructuring the response
    , ModifyVpnConnectionOptionsResponse (..)
    , mkModifyVpnConnectionOptionsResponse
    -- ** Response lenses
    , mvcorrsVpnConnection
    , mvcorrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVpnConnectionOptions' smart constructor.
data ModifyVpnConnectionOptions = ModifyVpnConnectionOptions'
  { vpnConnectionId :: Types.VpnConnectionId
    -- ^ The ID of the Site-to-Site VPN connection. 
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , localIpv4NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @0.0.0.0/0@ 
  , localIpv6NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @::/0@ 
  , remoteIpv4NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0/0@ 
  , remoteIpv6NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR on the AWS side of the VPN connection.
--
-- Default: @::/0@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpnConnectionOptions' value with any optional fields omitted.
mkModifyVpnConnectionOptions
    :: Types.VpnConnectionId -- ^ 'vpnConnectionId'
    -> ModifyVpnConnectionOptions
mkModifyVpnConnectionOptions vpnConnectionId
  = ModifyVpnConnectionOptions'{vpnConnectionId,
                                dryRun = Core.Nothing, localIpv4NetworkCidr = Core.Nothing,
                                localIpv6NetworkCidr = Core.Nothing,
                                remoteIpv4NetworkCidr = Core.Nothing,
                                remoteIpv6NetworkCidr = Core.Nothing}

-- | The ID of the Site-to-Site VPN connection. 
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoVpnConnectionId :: Lens.Lens' ModifyVpnConnectionOptions Types.VpnConnectionId
mvcoVpnConnectionId = Lens.field @"vpnConnectionId"
{-# INLINEABLE mvcoVpnConnectionId #-}
{-# DEPRECATED vpnConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoDryRun :: Lens.Lens' ModifyVpnConnectionOptions (Core.Maybe Core.Bool)
mvcoDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvcoDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @0.0.0.0/0@ 
--
-- /Note:/ Consider using 'localIpv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoLocalIpv4NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Core.Maybe Core.Text)
mvcoLocalIpv4NetworkCidr = Lens.field @"localIpv4NetworkCidr"
{-# INLINEABLE mvcoLocalIpv4NetworkCidr #-}
{-# DEPRECATED localIpv4NetworkCidr "Use generic-lens or generic-optics with 'localIpv4NetworkCidr' instead"  #-}

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @::/0@ 
--
-- /Note:/ Consider using 'localIpv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoLocalIpv6NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Core.Maybe Core.Text)
mvcoLocalIpv6NetworkCidr = Lens.field @"localIpv6NetworkCidr"
{-# INLINEABLE mvcoLocalIpv6NetworkCidr #-}
{-# DEPRECATED localIpv6NetworkCidr "Use generic-lens or generic-optics with 'localIpv6NetworkCidr' instead"  #-}

-- | The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0/0@ 
--
-- /Note:/ Consider using 'remoteIpv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoRemoteIpv4NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Core.Maybe Core.Text)
mvcoRemoteIpv4NetworkCidr = Lens.field @"remoteIpv4NetworkCidr"
{-# INLINEABLE mvcoRemoteIpv4NetworkCidr #-}
{-# DEPRECATED remoteIpv4NetworkCidr "Use generic-lens or generic-optics with 'remoteIpv4NetworkCidr' instead"  #-}

-- | The IPv6 CIDR on the AWS side of the VPN connection.
--
-- Default: @::/0@ 
--
-- /Note:/ Consider using 'remoteIpv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcoRemoteIpv6NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Core.Maybe Core.Text)
mvcoRemoteIpv6NetworkCidr = Lens.field @"remoteIpv6NetworkCidr"
{-# INLINEABLE mvcoRemoteIpv6NetworkCidr #-}
{-# DEPRECATED remoteIpv6NetworkCidr "Use generic-lens or generic-optics with 'remoteIpv6NetworkCidr' instead"  #-}

instance Core.ToQuery ModifyVpnConnectionOptions where
        toQuery ModifyVpnConnectionOptions{..}
          = Core.toQueryPair "Action"
              ("ModifyVpnConnectionOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpnConnectionId" vpnConnectionId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LocalIpv4NetworkCidr")
                localIpv4NetworkCidr
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LocalIpv6NetworkCidr")
                localIpv6NetworkCidr
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RemoteIpv4NetworkCidr")
                remoteIpv4NetworkCidr
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RemoteIpv6NetworkCidr")
                remoteIpv6NetworkCidr

instance Core.ToHeaders ModifyVpnConnectionOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpnConnectionOptions where
        type Rs ModifyVpnConnectionOptions =
             ModifyVpnConnectionOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ModifyVpnConnectionOptionsResponse' Core.<$>
                   (x Core..@? "vpnConnection") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpnConnectionOptionsResponse' smart constructor.
data ModifyVpnConnectionOptionsResponse = ModifyVpnConnectionOptionsResponse'
  { vpnConnection :: Core.Maybe Types.VpnConnection
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyVpnConnectionOptionsResponse' value with any optional fields omitted.
mkModifyVpnConnectionOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVpnConnectionOptionsResponse
mkModifyVpnConnectionOptionsResponse responseStatus
  = ModifyVpnConnectionOptionsResponse'{vpnConnection = Core.Nothing,
                                        responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpnConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcorrsVpnConnection :: Lens.Lens' ModifyVpnConnectionOptionsResponse (Core.Maybe Types.VpnConnection)
mvcorrsVpnConnection = Lens.field @"vpnConnection"
{-# INLINEABLE mvcorrsVpnConnection #-}
{-# DEPRECATED vpnConnection "Use generic-lens or generic-optics with 'vpnConnection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcorrsResponseStatus :: Lens.Lens' ModifyVpnConnectionOptionsResponse Core.Int
mvcorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mvcorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
