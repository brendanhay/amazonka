{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpnTunnelCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the VPN tunnel endpoint certificate.
module Network.AWS.EC2.ModifyVpnTunnelCertificate
    (
    -- * Creating a request
      ModifyVpnTunnelCertificate (..)
    , mkModifyVpnTunnelCertificate
    -- ** Request lenses
    , mvtcVpnConnectionId
    , mvtcVpnTunnelOutsideIpAddress
    , mvtcDryRun

    -- * Destructuring the response
    , ModifyVpnTunnelCertificateResponse (..)
    , mkModifyVpnTunnelCertificateResponse
    -- ** Response lenses
    , mvtcrrsVpnConnection
    , mvtcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVpnTunnelCertificate' smart constructor.
data ModifyVpnTunnelCertificate = ModifyVpnTunnelCertificate'
  { vpnConnectionId :: Types.VpnConnectionId
    -- ^ The ID of the AWS Site-to-Site VPN connection.
  , vpnTunnelOutsideIpAddress :: Core.Text
    -- ^ The external IP address of the VPN tunnel.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpnTunnelCertificate' value with any optional fields omitted.
mkModifyVpnTunnelCertificate
    :: Types.VpnConnectionId -- ^ 'vpnConnectionId'
    -> Core.Text -- ^ 'vpnTunnelOutsideIpAddress'
    -> ModifyVpnTunnelCertificate
mkModifyVpnTunnelCertificate vpnConnectionId
  vpnTunnelOutsideIpAddress
  = ModifyVpnTunnelCertificate'{vpnConnectionId,
                                vpnTunnelOutsideIpAddress, dryRun = Core.Nothing}

-- | The ID of the AWS Site-to-Site VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtcVpnConnectionId :: Lens.Lens' ModifyVpnTunnelCertificate Types.VpnConnectionId
mvtcVpnConnectionId = Lens.field @"vpnConnectionId"
{-# INLINEABLE mvtcVpnConnectionId #-}
{-# DEPRECATED vpnConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead"  #-}

-- | The external IP address of the VPN tunnel.
--
-- /Note:/ Consider using 'vpnTunnelOutsideIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtcVpnTunnelOutsideIpAddress :: Lens.Lens' ModifyVpnTunnelCertificate Core.Text
mvtcVpnTunnelOutsideIpAddress = Lens.field @"vpnTunnelOutsideIpAddress"
{-# INLINEABLE mvtcVpnTunnelOutsideIpAddress #-}
{-# DEPRECATED vpnTunnelOutsideIpAddress "Use generic-lens or generic-optics with 'vpnTunnelOutsideIpAddress' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtcDryRun :: Lens.Lens' ModifyVpnTunnelCertificate (Core.Maybe Core.Bool)
mvtcDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvtcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ModifyVpnTunnelCertificate where
        toQuery ModifyVpnTunnelCertificate{..}
          = Core.toQueryPair "Action"
              ("ModifyVpnTunnelCertificate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpnConnectionId" vpnConnectionId
              Core.<>
              Core.toQueryPair "VpnTunnelOutsideIpAddress"
                vpnTunnelOutsideIpAddress
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ModifyVpnTunnelCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpnTunnelCertificate where
        type Rs ModifyVpnTunnelCertificate =
             ModifyVpnTunnelCertificateResponse
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
                 ModifyVpnTunnelCertificateResponse' Core.<$>
                   (x Core..@? "vpnConnection") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpnTunnelCertificateResponse' smart constructor.
data ModifyVpnTunnelCertificateResponse = ModifyVpnTunnelCertificateResponse'
  { vpnConnection :: Core.Maybe Types.VpnConnection
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyVpnTunnelCertificateResponse' value with any optional fields omitted.
mkModifyVpnTunnelCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVpnTunnelCertificateResponse
mkModifyVpnTunnelCertificateResponse responseStatus
  = ModifyVpnTunnelCertificateResponse'{vpnConnection = Core.Nothing,
                                        responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpnConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtcrrsVpnConnection :: Lens.Lens' ModifyVpnTunnelCertificateResponse (Core.Maybe Types.VpnConnection)
mvtcrrsVpnConnection = Lens.field @"vpnConnection"
{-# INLINEABLE mvtcrrsVpnConnection #-}
{-# DEPRECATED vpnConnection "Use generic-lens or generic-optics with 'vpnConnection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtcrrsResponseStatus :: Lens.Lens' ModifyVpnTunnelCertificateResponse Core.Int
mvtcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mvtcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
