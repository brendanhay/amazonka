{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVpnConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPN connection.
--
-- If you're deleting the VPC and its associated components, we recommend that you detach the virtual private gateway from the VPC and delete the VPC before deleting the VPN connection. If you believe that the tunnel credentials for your VPN connection have been compromised, you can delete the VPN connection and create a new one that has new keys, without needing to delete the VPC or virtual private gateway. If you create a new VPN connection, you must reconfigure the customer gateway device using the new configuration information returned with the new VPN connection ID.
-- For certificate-based authentication, delete all AWS Certificate Manager (ACM) private certificates used for the AWS-side tunnel endpoints for the VPN connection before deleting the VPN connection.
module Network.AWS.EC2.DeleteVpnConnection
    (
    -- * Creating a request
      DeleteVpnConnection (..)
    , mkDeleteVpnConnection
    -- ** Request lenses
    , dvcVpnConnectionId
    , dvcDryRun

    -- * Destructuring the response
    , DeleteVpnConnectionResponse (..)
    , mkDeleteVpnConnectionResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteVpnConnection.
--
-- /See:/ 'mkDeleteVpnConnection' smart constructor.
data DeleteVpnConnection = DeleteVpnConnection'
  { vpnConnectionId :: Types.VpnConnectionId
    -- ^ The ID of the VPN connection.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpnConnection' value with any optional fields omitted.
mkDeleteVpnConnection
    :: Types.VpnConnectionId -- ^ 'vpnConnectionId'
    -> DeleteVpnConnection
mkDeleteVpnConnection vpnConnectionId
  = DeleteVpnConnection'{vpnConnectionId, dryRun = Core.Nothing}

-- | The ID of the VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcVpnConnectionId :: Lens.Lens' DeleteVpnConnection Types.VpnConnectionId
dvcVpnConnectionId = Lens.field @"vpnConnectionId"
{-# INLINEABLE dvcVpnConnectionId #-}
{-# DEPRECATED vpnConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcDryRun :: Lens.Lens' DeleteVpnConnection (Core.Maybe Core.Bool)
dvcDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteVpnConnection where
        toQuery DeleteVpnConnection{..}
          = Core.toQueryPair "Action" ("DeleteVpnConnection" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpnConnectionId" vpnConnectionId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteVpnConnection where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVpnConnection where
        type Rs DeleteVpnConnection = DeleteVpnConnectionResponse
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
        parseResponse = Response.receiveNull DeleteVpnConnectionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVpnConnectionResponse' smart constructor.
data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpnConnectionResponse' value with any optional fields omitted.
mkDeleteVpnConnectionResponse
    :: DeleteVpnConnectionResponse
mkDeleteVpnConnectionResponse = DeleteVpnConnectionResponse'
