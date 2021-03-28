{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpnTunnelOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the options for a VPN tunnel in an AWS Site-to-Site VPN connection. You can modify multiple options for a tunnel in a single request, but you can only modify one tunnel at a time. For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPNTunnels.html Site-to-Site VPN Tunnel Options for Your Site-to-Site VPN Connection> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.ModifyVpnTunnelOptions
    (
    -- * Creating a request
      ModifyVpnTunnelOptions (..)
    , mkModifyVpnTunnelOptions
    -- ** Request lenses
    , mvtoVpnConnectionId
    , mvtoVpnTunnelOutsideIpAddress
    , mvtoTunnelOptions
    , mvtoDryRun

    -- * Destructuring the response
    , ModifyVpnTunnelOptionsResponse (..)
    , mkModifyVpnTunnelOptionsResponse
    -- ** Response lenses
    , mvtorrsVpnConnection
    , mvtorrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVpnTunnelOptions' smart constructor.
data ModifyVpnTunnelOptions = ModifyVpnTunnelOptions'
  { vpnConnectionId :: Types.VpnConnectionId
    -- ^ The ID of the AWS Site-to-Site VPN connection.
  , vpnTunnelOutsideIpAddress :: Core.Text
    -- ^ The external IP address of the VPN tunnel.
  , tunnelOptions :: Types.ModifyVpnTunnelOptionsSpecification
    -- ^ The tunnel options to modify.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpnTunnelOptions' value with any optional fields omitted.
mkModifyVpnTunnelOptions
    :: Types.VpnConnectionId -- ^ 'vpnConnectionId'
    -> Core.Text -- ^ 'vpnTunnelOutsideIpAddress'
    -> Types.ModifyVpnTunnelOptionsSpecification -- ^ 'tunnelOptions'
    -> ModifyVpnTunnelOptions
mkModifyVpnTunnelOptions vpnConnectionId vpnTunnelOutsideIpAddress
  tunnelOptions
  = ModifyVpnTunnelOptions'{vpnConnectionId,
                            vpnTunnelOutsideIpAddress, tunnelOptions, dryRun = Core.Nothing}

-- | The ID of the AWS Site-to-Site VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtoVpnConnectionId :: Lens.Lens' ModifyVpnTunnelOptions Types.VpnConnectionId
mvtoVpnConnectionId = Lens.field @"vpnConnectionId"
{-# INLINEABLE mvtoVpnConnectionId #-}
{-# DEPRECATED vpnConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead"  #-}

-- | The external IP address of the VPN tunnel.
--
-- /Note:/ Consider using 'vpnTunnelOutsideIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtoVpnTunnelOutsideIpAddress :: Lens.Lens' ModifyVpnTunnelOptions Core.Text
mvtoVpnTunnelOutsideIpAddress = Lens.field @"vpnTunnelOutsideIpAddress"
{-# INLINEABLE mvtoVpnTunnelOutsideIpAddress #-}
{-# DEPRECATED vpnTunnelOutsideIpAddress "Use generic-lens or generic-optics with 'vpnTunnelOutsideIpAddress' instead"  #-}

-- | The tunnel options to modify.
--
-- /Note:/ Consider using 'tunnelOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtoTunnelOptions :: Lens.Lens' ModifyVpnTunnelOptions Types.ModifyVpnTunnelOptionsSpecification
mvtoTunnelOptions = Lens.field @"tunnelOptions"
{-# INLINEABLE mvtoTunnelOptions #-}
{-# DEPRECATED tunnelOptions "Use generic-lens or generic-optics with 'tunnelOptions' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtoDryRun :: Lens.Lens' ModifyVpnTunnelOptions (Core.Maybe Core.Bool)
mvtoDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvtoDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ModifyVpnTunnelOptions where
        toQuery ModifyVpnTunnelOptions{..}
          = Core.toQueryPair "Action" ("ModifyVpnTunnelOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpnConnectionId" vpnConnectionId
              Core.<>
              Core.toQueryPair "VpnTunnelOutsideIpAddress"
                vpnTunnelOutsideIpAddress
              Core.<> Core.toQueryPair "TunnelOptions" tunnelOptions
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ModifyVpnTunnelOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpnTunnelOptions where
        type Rs ModifyVpnTunnelOptions = ModifyVpnTunnelOptionsResponse
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
                 ModifyVpnTunnelOptionsResponse' Core.<$>
                   (x Core..@? "vpnConnection") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpnTunnelOptionsResponse' smart constructor.
data ModifyVpnTunnelOptionsResponse = ModifyVpnTunnelOptionsResponse'
  { vpnConnection :: Core.Maybe Types.VpnConnection
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyVpnTunnelOptionsResponse' value with any optional fields omitted.
mkModifyVpnTunnelOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVpnTunnelOptionsResponse
mkModifyVpnTunnelOptionsResponse responseStatus
  = ModifyVpnTunnelOptionsResponse'{vpnConnection = Core.Nothing,
                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpnConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtorrsVpnConnection :: Lens.Lens' ModifyVpnTunnelOptionsResponse (Core.Maybe Types.VpnConnection)
mvtorrsVpnConnection = Lens.field @"vpnConnection"
{-# INLINEABLE mvtorrsVpnConnection #-}
{-# DEPRECATED vpnConnection "Use generic-lens or generic-optics with 'vpnConnection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtorrsResponseStatus :: Lens.Lens' ModifyVpnTunnelOptionsResponse Core.Int
mvtorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mvtorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
