{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVpcPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a VPC peering connection. Either the owner of the requester VPC or the owner of the accepter VPC can delete the VPC peering connection if it's in the @active@ state. The owner of the requester VPC can delete a VPC peering connection in the @pending-acceptance@ state. You cannot delete a VPC peering connection that's in the @failed@ state.
module Network.AWS.EC2.DeleteVpcPeeringConnection
    (
    -- * Creating a request
      DeleteVpcPeeringConnection (..)
    , mkDeleteVpcPeeringConnection
    -- ** Request lenses
    , dvpcVpcPeeringConnectionId
    , dvpcDryRun

    -- * Destructuring the response
    , DeleteVpcPeeringConnectionResponse (..)
    , mkDeleteVpcPeeringConnectionResponse
    -- ** Response lenses
    , dvpcrrsReturn
    , dvpcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteVpcPeeringConnection' smart constructor.
data DeleteVpcPeeringConnection = DeleteVpcPeeringConnection'
  { vpcPeeringConnectionId :: Types.VpcPeeringConnectionId
    -- ^ The ID of the VPC peering connection.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcPeeringConnection' value with any optional fields omitted.
mkDeleteVpcPeeringConnection
    :: Types.VpcPeeringConnectionId -- ^ 'vpcPeeringConnectionId'
    -> DeleteVpcPeeringConnection
mkDeleteVpcPeeringConnection vpcPeeringConnectionId
  = DeleteVpcPeeringConnection'{vpcPeeringConnectionId,
                                dryRun = Core.Nothing}

-- | The ID of the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcVpcPeeringConnectionId :: Lens.Lens' DeleteVpcPeeringConnection Types.VpcPeeringConnectionId
dvpcVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE dvpcVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcDryRun :: Lens.Lens' DeleteVpcPeeringConnection (Core.Maybe Core.Bool)
dvpcDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvpcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteVpcPeeringConnection where
        toQuery DeleteVpcPeeringConnection{..}
          = Core.toQueryPair "Action"
              ("DeleteVpcPeeringConnection" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "VpcPeeringConnectionId" vpcPeeringConnectionId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteVpcPeeringConnection where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVpcPeeringConnection where
        type Rs DeleteVpcPeeringConnection =
             DeleteVpcPeeringConnectionResponse
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
                 DeleteVpcPeeringConnectionResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVpcPeeringConnectionResponse' smart constructor.
data DeleteVpcPeeringConnectionResponse = DeleteVpcPeeringConnectionResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcPeeringConnectionResponse' value with any optional fields omitted.
mkDeleteVpcPeeringConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteVpcPeeringConnectionResponse
mkDeleteVpcPeeringConnectionResponse responseStatus
  = DeleteVpcPeeringConnectionResponse'{return = Core.Nothing,
                                        responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrrsReturn :: Lens.Lens' DeleteVpcPeeringConnectionResponse (Core.Maybe Core.Bool)
dvpcrrsReturn = Lens.field @"return"
{-# INLINEABLE dvpcrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrrsResponseStatus :: Lens.Lens' DeleteVpcPeeringConnectionResponse Core.Int
dvpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
