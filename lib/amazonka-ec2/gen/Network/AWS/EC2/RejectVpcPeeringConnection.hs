{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RejectVpcPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a VPC peering connection request. The VPC peering connection must be in the @pending-acceptance@ state. Use the 'DescribeVpcPeeringConnections' request to view your outstanding VPC peering connection requests. To delete an active VPC peering connection, or to delete a VPC peering connection request that you initiated, use 'DeleteVpcPeeringConnection' .
module Network.AWS.EC2.RejectVpcPeeringConnection
    (
    -- * Creating a request
      RejectVpcPeeringConnection (..)
    , mkRejectVpcPeeringConnection
    -- ** Request lenses
    , rvpcVpcPeeringConnectionId
    , rvpcDryRun

    -- * Destructuring the response
    , RejectVpcPeeringConnectionResponse (..)
    , mkRejectVpcPeeringConnectionResponse
    -- ** Response lenses
    , rvpcrrsReturn
    , rvpcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRejectVpcPeeringConnection' smart constructor.
data RejectVpcPeeringConnection = RejectVpcPeeringConnection'
  { vpcPeeringConnectionId :: Types.VpcPeeringConnectionId
    -- ^ The ID of the VPC peering connection.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectVpcPeeringConnection' value with any optional fields omitted.
mkRejectVpcPeeringConnection
    :: Types.VpcPeeringConnectionId -- ^ 'vpcPeeringConnectionId'
    -> RejectVpcPeeringConnection
mkRejectVpcPeeringConnection vpcPeeringConnectionId
  = RejectVpcPeeringConnection'{vpcPeeringConnectionId,
                                dryRun = Core.Nothing}

-- | The ID of the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvpcVpcPeeringConnectionId :: Lens.Lens' RejectVpcPeeringConnection Types.VpcPeeringConnectionId
rvpcVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE rvpcVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvpcDryRun :: Lens.Lens' RejectVpcPeeringConnection (Core.Maybe Core.Bool)
rvpcDryRun = Lens.field @"dryRun"
{-# INLINEABLE rvpcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery RejectVpcPeeringConnection where
        toQuery RejectVpcPeeringConnection{..}
          = Core.toQueryPair "Action"
              ("RejectVpcPeeringConnection" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "VpcPeeringConnectionId" vpcPeeringConnectionId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders RejectVpcPeeringConnection where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RejectVpcPeeringConnection where
        type Rs RejectVpcPeeringConnection =
             RejectVpcPeeringConnectionResponse
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
                 RejectVpcPeeringConnectionResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRejectVpcPeeringConnectionResponse' smart constructor.
data RejectVpcPeeringConnectionResponse = RejectVpcPeeringConnectionResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectVpcPeeringConnectionResponse' value with any optional fields omitted.
mkRejectVpcPeeringConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RejectVpcPeeringConnectionResponse
mkRejectVpcPeeringConnectionResponse responseStatus
  = RejectVpcPeeringConnectionResponse'{return = Core.Nothing,
                                        responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvpcrrsReturn :: Lens.Lens' RejectVpcPeeringConnectionResponse (Core.Maybe Core.Bool)
rvpcrrsReturn = Lens.field @"return"
{-# INLINEABLE rvpcrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvpcrrsResponseStatus :: Lens.Lens' RejectVpcPeeringConnectionResponse Core.Int
rvpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rvpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
