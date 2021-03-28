{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.TerminateClientVpnConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates active Client VPN endpoint connections. This action can be used to terminate a specific client connection, or up to five connections established by a specific user.
module Network.AWS.EC2.TerminateClientVpnConnections
    (
    -- * Creating a request
      TerminateClientVpnConnections (..)
    , mkTerminateClientVpnConnections
    -- ** Request lenses
    , tcvcClientVpnEndpointId
    , tcvcConnectionId
    , tcvcDryRun
    , tcvcUsername

    -- * Destructuring the response
    , TerminateClientVpnConnectionsResponse (..)
    , mkTerminateClientVpnConnectionsResponse
    -- ** Response lenses
    , tcvcrrsClientVpnEndpointId
    , tcvcrrsConnectionStatuses
    , tcvcrrsUsername
    , tcvcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTerminateClientVpnConnections' smart constructor.
data TerminateClientVpnConnections = TerminateClientVpnConnections'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint to which the client is connected.
  , connectionId :: Core.Maybe Types.ConnectionId
    -- ^ The ID of the client connection to be terminated.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , username :: Core.Maybe Core.Text
    -- ^ The name of the user who initiated the connection. Use this option to terminate all active connections for the specified user. This option can only be used if the user has established up to five connections.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateClientVpnConnections' value with any optional fields omitted.
mkTerminateClientVpnConnections
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> TerminateClientVpnConnections
mkTerminateClientVpnConnections clientVpnEndpointId
  = TerminateClientVpnConnections'{clientVpnEndpointId,
                                   connectionId = Core.Nothing, dryRun = Core.Nothing,
                                   username = Core.Nothing}

-- | The ID of the Client VPN endpoint to which the client is connected.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcClientVpnEndpointId :: Lens.Lens' TerminateClientVpnConnections Types.ClientVpnEndpointId
tcvcClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE tcvcClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The ID of the client connection to be terminated.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcConnectionId :: Lens.Lens' TerminateClientVpnConnections (Core.Maybe Types.ConnectionId)
tcvcConnectionId = Lens.field @"connectionId"
{-# INLINEABLE tcvcConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcDryRun :: Lens.Lens' TerminateClientVpnConnections (Core.Maybe Core.Bool)
tcvcDryRun = Lens.field @"dryRun"
{-# INLINEABLE tcvcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The name of the user who initiated the connection. Use this option to terminate all active connections for the specified user. This option can only be used if the user has established up to five connections.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcUsername :: Lens.Lens' TerminateClientVpnConnections (Core.Maybe Core.Text)
tcvcUsername = Lens.field @"username"
{-# INLINEABLE tcvcUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.ToQuery TerminateClientVpnConnections where
        toQuery TerminateClientVpnConnections{..}
          = Core.toQueryPair "Action"
              ("TerminateClientVpnConnections" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ConnectionId")
                connectionId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Username") username

instance Core.ToHeaders TerminateClientVpnConnections where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest TerminateClientVpnConnections where
        type Rs TerminateClientVpnConnections =
             TerminateClientVpnConnectionsResponse
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
                 TerminateClientVpnConnectionsResponse' Core.<$>
                   (x Core..@? "clientVpnEndpointId") Core.<*>
                     x Core..@? "connectionStatuses" Core..<@> Core.parseXMLList "item"
                     Core.<*> x Core..@? "username"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTerminateClientVpnConnectionsResponse' smart constructor.
data TerminateClientVpnConnectionsResponse = TerminateClientVpnConnectionsResponse'
  { clientVpnEndpointId :: Core.Maybe Core.Text
    -- ^ The ID of the Client VPN endpoint.
  , connectionStatuses :: Core.Maybe [Types.TerminateConnectionStatus]
    -- ^ The current state of the client connections.
  , username :: Core.Maybe Core.Text
    -- ^ The user who established the terminated client connections.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateClientVpnConnectionsResponse' value with any optional fields omitted.
mkTerminateClientVpnConnectionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TerminateClientVpnConnectionsResponse
mkTerminateClientVpnConnectionsResponse responseStatus
  = TerminateClientVpnConnectionsResponse'{clientVpnEndpointId =
                                             Core.Nothing,
                                           connectionStatuses = Core.Nothing,
                                           username = Core.Nothing, responseStatus}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcrrsClientVpnEndpointId :: Lens.Lens' TerminateClientVpnConnectionsResponse (Core.Maybe Core.Text)
tcvcrrsClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE tcvcrrsClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The current state of the client connections.
--
-- /Note:/ Consider using 'connectionStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcrrsConnectionStatuses :: Lens.Lens' TerminateClientVpnConnectionsResponse (Core.Maybe [Types.TerminateConnectionStatus])
tcvcrrsConnectionStatuses = Lens.field @"connectionStatuses"
{-# INLINEABLE tcvcrrsConnectionStatuses #-}
{-# DEPRECATED connectionStatuses "Use generic-lens or generic-optics with 'connectionStatuses' instead"  #-}

-- | The user who established the terminated client connections.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcrrsUsername :: Lens.Lens' TerminateClientVpnConnectionsResponse (Core.Maybe Core.Text)
tcvcrrsUsername = Lens.field @"username"
{-# INLINEABLE tcvcrrsUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcrrsResponseStatus :: Lens.Lens' TerminateClientVpnConnectionsResponse Core.Int
tcvcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tcvcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
