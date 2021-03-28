{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateLocalGatewayRouteTableVpcAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified VPC with the specified local gateway route table.
module Network.AWS.EC2.CreateLocalGatewayRouteTableVpcAssociation
    (
    -- * Creating a request
      CreateLocalGatewayRouteTableVpcAssociation (..)
    , mkCreateLocalGatewayRouteTableVpcAssociation
    -- ** Request lenses
    , clgrtvaLocalGatewayRouteTableId
    , clgrtvaVpcId
    , clgrtvaDryRun
    , clgrtvaTagSpecifications

    -- * Destructuring the response
    , CreateLocalGatewayRouteTableVpcAssociationResponse (..)
    , mkCreateLocalGatewayRouteTableVpcAssociationResponse
    -- ** Response lenses
    , clgrtvarrsLocalGatewayRouteTableVpcAssociation
    , clgrtvarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLocalGatewayRouteTableVpcAssociation' smart constructor.
data CreateLocalGatewayRouteTableVpcAssociation = CreateLocalGatewayRouteTableVpcAssociation'
  { localGatewayRouteTableId :: Types.LocalGatewayRoutetableId
    -- ^ The ID of the local gateway route table.
  , vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to the local gateway route table VPC association.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLocalGatewayRouteTableVpcAssociation' value with any optional fields omitted.
mkCreateLocalGatewayRouteTableVpcAssociation
    :: Types.LocalGatewayRoutetableId -- ^ 'localGatewayRouteTableId'
    -> Types.VpcId -- ^ 'vpcId'
    -> CreateLocalGatewayRouteTableVpcAssociation
mkCreateLocalGatewayRouteTableVpcAssociation
  localGatewayRouteTableId vpcId
  = CreateLocalGatewayRouteTableVpcAssociation'{localGatewayRouteTableId,
                                                vpcId, dryRun = Core.Nothing,
                                                tagSpecifications = Core.Nothing}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvaLocalGatewayRouteTableId :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociation Types.LocalGatewayRoutetableId
clgrtvaLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# INLINEABLE clgrtvaLocalGatewayRouteTableId #-}
{-# DEPRECATED localGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvaVpcId :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociation Types.VpcId
clgrtvaVpcId = Lens.field @"vpcId"
{-# INLINEABLE clgrtvaVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvaDryRun :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociation (Core.Maybe Core.Bool)
clgrtvaDryRun = Lens.field @"dryRun"
{-# INLINEABLE clgrtvaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to assign to the local gateway route table VPC association.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvaTagSpecifications :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociation (Core.Maybe [Types.TagSpecification])
clgrtvaTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE clgrtvaTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateLocalGatewayRouteTableVpcAssociation
         where
        toQuery CreateLocalGatewayRouteTableVpcAssociation{..}
          = Core.toQueryPair "Action"
              ("CreateLocalGatewayRouteTableVpcAssociation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "LocalGatewayRouteTableId"
                localGatewayRouteTableId
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateLocalGatewayRouteTableVpcAssociation
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateLocalGatewayRouteTableVpcAssociation
         where
        type Rs CreateLocalGatewayRouteTableVpcAssociation =
             CreateLocalGatewayRouteTableVpcAssociationResponse
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
                 CreateLocalGatewayRouteTableVpcAssociationResponse' Core.<$>
                   (x Core..@? "localGatewayRouteTableVpcAssociation") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLocalGatewayRouteTableVpcAssociationResponse' smart constructor.
data CreateLocalGatewayRouteTableVpcAssociationResponse = CreateLocalGatewayRouteTableVpcAssociationResponse'
  { localGatewayRouteTableVpcAssociation :: Core.Maybe Types.LocalGatewayRouteTableVpcAssociation
    -- ^ Information about the association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLocalGatewayRouteTableVpcAssociationResponse' value with any optional fields omitted.
mkCreateLocalGatewayRouteTableVpcAssociationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLocalGatewayRouteTableVpcAssociationResponse
mkCreateLocalGatewayRouteTableVpcAssociationResponse responseStatus
  = CreateLocalGatewayRouteTableVpcAssociationResponse'{localGatewayRouteTableVpcAssociation
                                                          = Core.Nothing,
                                                        responseStatus}

-- | Information about the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableVpcAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvarrsLocalGatewayRouteTableVpcAssociation :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociationResponse (Core.Maybe Types.LocalGatewayRouteTableVpcAssociation)
clgrtvarrsLocalGatewayRouteTableVpcAssociation = Lens.field @"localGatewayRouteTableVpcAssociation"
{-# INLINEABLE clgrtvarrsLocalGatewayRouteTableVpcAssociation #-}
{-# DEPRECATED localGatewayRouteTableVpcAssociation "Use generic-lens or generic-optics with 'localGatewayRouteTableVpcAssociation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvarrsResponseStatus :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociationResponse Core.Int
clgrtvarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clgrtvarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
