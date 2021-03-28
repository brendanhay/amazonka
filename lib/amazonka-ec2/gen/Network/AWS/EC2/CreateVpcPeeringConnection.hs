{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVpcPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a VPC peering connection between two VPCs: a requester VPC that you own and an accepter VPC with which to create the connection. The accepter VPC can belong to another AWS account and can be in a different Region to the requester VPC. The requester VPC and accepter VPC cannot have overlapping CIDR blocks.
--
-- The owner of the accepter VPC must accept the peering request to activate the peering connection. The VPC peering connection request expires after 7 days, after which it cannot be accepted or rejected.
-- If you create a VPC peering connection request between VPCs with overlapping CIDR blocks, the VPC peering connection has a status of @failed@ .
module Network.AWS.EC2.CreateVpcPeeringConnection
    (
    -- * Creating a request
      CreateVpcPeeringConnection (..)
    , mkCreateVpcPeeringConnection
    -- ** Request lenses
    , cvpcDryRun
    , cvpcPeerOwnerId
    , cvpcPeerRegion
    , cvpcPeerVpcId
    , cvpcTagSpecifications
    , cvpcVpcId

    -- * Destructuring the response
    , CreateVpcPeeringConnectionResponse (..)
    , mkCreateVpcPeeringConnectionResponse
    -- ** Response lenses
    , cvpcrrsVpcPeeringConnection
    , cvpcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateVpcPeeringConnection' smart constructor.
data CreateVpcPeeringConnection = CreateVpcPeeringConnection'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , peerOwnerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID of the owner of the accepter VPC.
--
-- Default: Your AWS account ID
  , peerRegion :: Core.Maybe Core.Text
    -- ^ The Region code for the accepter VPC, if the accepter VPC is located in a Region other than the Region in which you make the request.
--
-- Default: The Region in which you make the request.
  , peerVpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC with which you are creating the VPC peering connection. You must specify this parameter in the request.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to the peering connection.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The ID of the requester VPC. You must specify this parameter in the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpcPeeringConnection' value with any optional fields omitted.
mkCreateVpcPeeringConnection
    :: CreateVpcPeeringConnection
mkCreateVpcPeeringConnection
  = CreateVpcPeeringConnection'{dryRun = Core.Nothing,
                                peerOwnerId = Core.Nothing, peerRegion = Core.Nothing,
                                peerVpcId = Core.Nothing, tagSpecifications = Core.Nothing,
                                vpcId = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcDryRun :: Lens.Lens' CreateVpcPeeringConnection (Core.Maybe Core.Bool)
cvpcDryRun = Lens.field @"dryRun"
{-# INLINEABLE cvpcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The AWS account ID of the owner of the accepter VPC.
--
-- Default: Your AWS account ID
--
-- /Note:/ Consider using 'peerOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcPeerOwnerId :: Lens.Lens' CreateVpcPeeringConnection (Core.Maybe Core.Text)
cvpcPeerOwnerId = Lens.field @"peerOwnerId"
{-# INLINEABLE cvpcPeerOwnerId #-}
{-# DEPRECATED peerOwnerId "Use generic-lens or generic-optics with 'peerOwnerId' instead"  #-}

-- | The Region code for the accepter VPC, if the accepter VPC is located in a Region other than the Region in which you make the request.
--
-- Default: The Region in which you make the request.
--
-- /Note:/ Consider using 'peerRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcPeerRegion :: Lens.Lens' CreateVpcPeeringConnection (Core.Maybe Core.Text)
cvpcPeerRegion = Lens.field @"peerRegion"
{-# INLINEABLE cvpcPeerRegion #-}
{-# DEPRECATED peerRegion "Use generic-lens or generic-optics with 'peerRegion' instead"  #-}

-- | The ID of the VPC with which you are creating the VPC peering connection. You must specify this parameter in the request.
--
-- /Note:/ Consider using 'peerVpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcPeerVpcId :: Lens.Lens' CreateVpcPeeringConnection (Core.Maybe Core.Text)
cvpcPeerVpcId = Lens.field @"peerVpcId"
{-# INLINEABLE cvpcPeerVpcId #-}
{-# DEPRECATED peerVpcId "Use generic-lens or generic-optics with 'peerVpcId' instead"  #-}

-- | The tags to assign to the peering connection.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcTagSpecifications :: Lens.Lens' CreateVpcPeeringConnection (Core.Maybe [Types.TagSpecification])
cvpcTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cvpcTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

-- | The ID of the requester VPC. You must specify this parameter in the request.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcVpcId :: Lens.Lens' CreateVpcPeeringConnection (Core.Maybe Types.VpcId)
cvpcVpcId = Lens.field @"vpcId"
{-# INLINEABLE cvpcVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.ToQuery CreateVpcPeeringConnection where
        toQuery CreateVpcPeeringConnection{..}
          = Core.toQueryPair "Action"
              ("CreateVpcPeeringConnection" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PeerOwnerId") peerOwnerId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PeerRegion") peerRegion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PeerVpcId") peerVpcId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "VpcId") vpcId

instance Core.ToHeaders CreateVpcPeeringConnection where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateVpcPeeringConnection where
        type Rs CreateVpcPeeringConnection =
             CreateVpcPeeringConnectionResponse
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
                 CreateVpcPeeringConnectionResponse' Core.<$>
                   (x Core..@? "vpcPeeringConnection") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateVpcPeeringConnectionResponse' smart constructor.
data CreateVpcPeeringConnectionResponse = CreateVpcPeeringConnectionResponse'
  { vpcPeeringConnection :: Core.Maybe Types.VpcPeeringConnection
    -- ^ Information about the VPC peering connection.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateVpcPeeringConnectionResponse' value with any optional fields omitted.
mkCreateVpcPeeringConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateVpcPeeringConnectionResponse
mkCreateVpcPeeringConnectionResponse responseStatus
  = CreateVpcPeeringConnectionResponse'{vpcPeeringConnection =
                                          Core.Nothing,
                                        responseStatus}

-- | Information about the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcrrsVpcPeeringConnection :: Lens.Lens' CreateVpcPeeringConnectionResponse (Core.Maybe Types.VpcPeeringConnection)
cvpcrrsVpcPeeringConnection = Lens.field @"vpcPeeringConnection"
{-# INLINEABLE cvpcrrsVpcPeeringConnection #-}
{-# DEPRECATED vpcPeeringConnection "Use generic-lens or generic-optics with 'vpcPeeringConnection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcrrsResponseStatus :: Lens.Lens' CreateVpcPeeringConnectionResponse Core.Int
cvpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cvpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
