{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AcceptVpcPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept a VPC peering connection request. To accept a request, the VPC peering connection must be in the @pending-acceptance@ state, and you must be the owner of the peer VPC. Use 'DescribeVpcPeeringConnections' to view your outstanding VPC peering connection requests.
--
-- For an inter-Region VPC peering connection request, you must accept the VPC peering connection in the Region of the accepter VPC.
module Network.AWS.EC2.AcceptVpcPeeringConnection
    (
    -- * Creating a request
      AcceptVpcPeeringConnection (..)
    , mkAcceptVpcPeeringConnection
    -- ** Request lenses
    , avpcDryRun
    , avpcVpcPeeringConnectionId

    -- * Destructuring the response
    , AcceptVpcPeeringConnectionResponse (..)
    , mkAcceptVpcPeeringConnectionResponse
    -- ** Response lenses
    , avpcrrsVpcPeeringConnection
    , avpcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptVpcPeeringConnection' smart constructor.
data AcceptVpcPeeringConnection = AcceptVpcPeeringConnection'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , vpcPeeringConnectionId :: Core.Maybe Types.VpcPeeringConnectionId
    -- ^ The ID of the VPC peering connection. You must specify this parameter in the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptVpcPeeringConnection' value with any optional fields omitted.
mkAcceptVpcPeeringConnection
    :: AcceptVpcPeeringConnection
mkAcceptVpcPeeringConnection
  = AcceptVpcPeeringConnection'{dryRun = Core.Nothing,
                                vpcPeeringConnectionId = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcDryRun :: Lens.Lens' AcceptVpcPeeringConnection (Core.Maybe Core.Bool)
avpcDryRun = Lens.field @"dryRun"
{-# INLINEABLE avpcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the VPC peering connection. You must specify this parameter in the request.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcVpcPeeringConnectionId :: Lens.Lens' AcceptVpcPeeringConnection (Core.Maybe Types.VpcPeeringConnectionId)
avpcVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE avpcVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

instance Core.ToQuery AcceptVpcPeeringConnection where
        toQuery AcceptVpcPeeringConnection{..}
          = Core.toQueryPair "Action"
              ("AcceptVpcPeeringConnection" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VpcPeeringConnectionId")
                vpcPeeringConnectionId

instance Core.ToHeaders AcceptVpcPeeringConnection where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AcceptVpcPeeringConnection where
        type Rs AcceptVpcPeeringConnection =
             AcceptVpcPeeringConnectionResponse
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
                 AcceptVpcPeeringConnectionResponse' Core.<$>
                   (x Core..@? "vpcPeeringConnection") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAcceptVpcPeeringConnectionResponse' smart constructor.
data AcceptVpcPeeringConnectionResponse = AcceptVpcPeeringConnectionResponse'
  { vpcPeeringConnection :: Core.Maybe Types.VpcPeeringConnection
    -- ^ Information about the VPC peering connection.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AcceptVpcPeeringConnectionResponse' value with any optional fields omitted.
mkAcceptVpcPeeringConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptVpcPeeringConnectionResponse
mkAcceptVpcPeeringConnectionResponse responseStatus
  = AcceptVpcPeeringConnectionResponse'{vpcPeeringConnection =
                                          Core.Nothing,
                                        responseStatus}

-- | Information about the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcrrsVpcPeeringConnection :: Lens.Lens' AcceptVpcPeeringConnectionResponse (Core.Maybe Types.VpcPeeringConnection)
avpcrrsVpcPeeringConnection = Lens.field @"vpcPeeringConnection"
{-# INLINEABLE avpcrrsVpcPeeringConnection #-}
{-# DEPRECATED vpcPeeringConnection "Use generic-lens or generic-optics with 'vpcPeeringConnection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcrrsResponseStatus :: Lens.Lens' AcceptVpcPeeringConnectionResponse Core.Int
avpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE avpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
