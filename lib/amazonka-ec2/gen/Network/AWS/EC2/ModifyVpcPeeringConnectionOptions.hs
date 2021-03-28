{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpcPeeringConnectionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the VPC peering connection options on one side of a VPC peering connection. You can do the following:
--
--
--     * Enable/disable communication over the peering connection between an EC2-Classic instance that's linked to your VPC (using ClassicLink) and instances in the peer VPC.
--
--
--     * Enable/disable communication over the peering connection between instances in your VPC and an EC2-Classic instance that's linked to the peer VPC.
--
--
--     * Enable/disable the ability to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.
--
--
-- If the peered VPCs are in the same AWS account, you can enable DNS resolution for queries from the local VPC. This ensures that queries from the local VPC resolve to private IP addresses in the peer VPC. This option is not available if the peered VPCs are in different AWS accounts or different Regions. For peered VPCs in different AWS accounts, each AWS account owner must initiate a separate request to modify the peering connection options. For inter-region peering connections, you must use the Region for the requester VPC to modify the requester VPC peering options and the Region for the accepter VPC to modify the accepter VPC peering options. To verify which VPCs are the accepter and the requester for a VPC peering connection, use the 'DescribeVpcPeeringConnections' command.
module Network.AWS.EC2.ModifyVpcPeeringConnectionOptions
    (
    -- * Creating a request
      ModifyVpcPeeringConnectionOptions (..)
    , mkModifyVpcPeeringConnectionOptions
    -- ** Request lenses
    , mvpcoVpcPeeringConnectionId
    , mvpcoAccepterPeeringConnectionOptions
    , mvpcoDryRun
    , mvpcoRequesterPeeringConnectionOptions

    -- * Destructuring the response
    , ModifyVpcPeeringConnectionOptionsResponse (..)
    , mkModifyVpcPeeringConnectionOptionsResponse
    -- ** Response lenses
    , mvpcorrsAccepterPeeringConnectionOptions
    , mvpcorrsRequesterPeeringConnectionOptions
    , mvpcorrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVpcPeeringConnectionOptions' smart constructor.
data ModifyVpcPeeringConnectionOptions = ModifyVpcPeeringConnectionOptions'
  { vpcPeeringConnectionId :: Types.VpcPeeringConnectionId
    -- ^ The ID of the VPC peering connection.
  , accepterPeeringConnectionOptions :: Core.Maybe Types.PeeringConnectionOptionsRequest
    -- ^ The VPC peering connection options for the accepter VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , requesterPeeringConnectionOptions :: Core.Maybe Types.PeeringConnectionOptionsRequest
    -- ^ The VPC peering connection options for the requester VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcPeeringConnectionOptions' value with any optional fields omitted.
mkModifyVpcPeeringConnectionOptions
    :: Types.VpcPeeringConnectionId -- ^ 'vpcPeeringConnectionId'
    -> ModifyVpcPeeringConnectionOptions
mkModifyVpcPeeringConnectionOptions vpcPeeringConnectionId
  = ModifyVpcPeeringConnectionOptions'{vpcPeeringConnectionId,
                                       accepterPeeringConnectionOptions = Core.Nothing,
                                       dryRun = Core.Nothing,
                                       requesterPeeringConnectionOptions = Core.Nothing}

-- | The ID of the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcoVpcPeeringConnectionId :: Lens.Lens' ModifyVpcPeeringConnectionOptions Types.VpcPeeringConnectionId
mvpcoVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE mvpcoVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

-- | The VPC peering connection options for the accepter VPC.
--
-- /Note:/ Consider using 'accepterPeeringConnectionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcoAccepterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Core.Maybe Types.PeeringConnectionOptionsRequest)
mvpcoAccepterPeeringConnectionOptions = Lens.field @"accepterPeeringConnectionOptions"
{-# INLINEABLE mvpcoAccepterPeeringConnectionOptions #-}
{-# DEPRECATED accepterPeeringConnectionOptions "Use generic-lens or generic-optics with 'accepterPeeringConnectionOptions' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcoDryRun :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Core.Maybe Core.Bool)
mvpcoDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvpcoDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The VPC peering connection options for the requester VPC.
--
-- /Note:/ Consider using 'requesterPeeringConnectionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcoRequesterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptions (Core.Maybe Types.PeeringConnectionOptionsRequest)
mvpcoRequesterPeeringConnectionOptions = Lens.field @"requesterPeeringConnectionOptions"
{-# INLINEABLE mvpcoRequesterPeeringConnectionOptions #-}
{-# DEPRECATED requesterPeeringConnectionOptions "Use generic-lens or generic-optics with 'requesterPeeringConnectionOptions' instead"  #-}

instance Core.ToQuery ModifyVpcPeeringConnectionOptions where
        toQuery ModifyVpcPeeringConnectionOptions{..}
          = Core.toQueryPair "Action"
              ("ModifyVpcPeeringConnectionOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "VpcPeeringConnectionId" vpcPeeringConnectionId
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AccepterPeeringConnectionOptions")
                accepterPeeringConnectionOptions
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "RequesterPeeringConnectionOptions")
                requesterPeeringConnectionOptions

instance Core.ToHeaders ModifyVpcPeeringConnectionOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpcPeeringConnectionOptions where
        type Rs ModifyVpcPeeringConnectionOptions =
             ModifyVpcPeeringConnectionOptionsResponse
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
                 ModifyVpcPeeringConnectionOptionsResponse' Core.<$>
                   (x Core..@? "accepterPeeringConnectionOptions") Core.<*>
                     x Core..@? "requesterPeeringConnectionOptions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpcPeeringConnectionOptionsResponse' smart constructor.
data ModifyVpcPeeringConnectionOptionsResponse = ModifyVpcPeeringConnectionOptionsResponse'
  { accepterPeeringConnectionOptions :: Core.Maybe Types.PeeringConnectionOptions
    -- ^ Information about the VPC peering connection options for the accepter VPC.
  , requesterPeeringConnectionOptions :: Core.Maybe Types.PeeringConnectionOptions
    -- ^ Information about the VPC peering connection options for the requester VPC.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcPeeringConnectionOptionsResponse' value with any optional fields omitted.
mkModifyVpcPeeringConnectionOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVpcPeeringConnectionOptionsResponse
mkModifyVpcPeeringConnectionOptionsResponse responseStatus
  = ModifyVpcPeeringConnectionOptionsResponse'{accepterPeeringConnectionOptions
                                                 = Core.Nothing,
                                               requesterPeeringConnectionOptions = Core.Nothing,
                                               responseStatus}

-- | Information about the VPC peering connection options for the accepter VPC.
--
-- /Note:/ Consider using 'accepterPeeringConnectionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcorrsAccepterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptionsResponse (Core.Maybe Types.PeeringConnectionOptions)
mvpcorrsAccepterPeeringConnectionOptions = Lens.field @"accepterPeeringConnectionOptions"
{-# INLINEABLE mvpcorrsAccepterPeeringConnectionOptions #-}
{-# DEPRECATED accepterPeeringConnectionOptions "Use generic-lens or generic-optics with 'accepterPeeringConnectionOptions' instead"  #-}

-- | Information about the VPC peering connection options for the requester VPC.
--
-- /Note:/ Consider using 'requesterPeeringConnectionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcorrsRequesterPeeringConnectionOptions :: Lens.Lens' ModifyVpcPeeringConnectionOptionsResponse (Core.Maybe Types.PeeringConnectionOptions)
mvpcorrsRequesterPeeringConnectionOptions = Lens.field @"requesterPeeringConnectionOptions"
{-# INLINEABLE mvpcorrsRequesterPeeringConnectionOptions #-}
{-# DEPRECATED requesterPeeringConnectionOptions "Use generic-lens or generic-optics with 'requesterPeeringConnectionOptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvpcorrsResponseStatus :: Lens.Lens' ModifyVpcPeeringConnectionOptionsResponse Core.Int
mvpcorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mvpcorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
