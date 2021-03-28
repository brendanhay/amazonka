{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpcEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes of a specified VPC endpoint. The attributes that you can modify depend on the type of VPC endpoint (interface, gateway, or Gateway Load Balancer). For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-endpoints.html VPC Endpoints> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.ModifyVpcEndpoint
    (
    -- * Creating a request
      ModifyVpcEndpoint (..)
    , mkModifyVpcEndpoint
    -- ** Request lenses
    , mveVpcEndpointId
    , mveAddRouteTableIds
    , mveAddSecurityGroupIds
    , mveAddSubnetIds
    , mveDryRun
    , mvePolicyDocument
    , mvePrivateDnsEnabled
    , mveRemoveRouteTableIds
    , mveRemoveSecurityGroupIds
    , mveRemoveSubnetIds
    , mveResetPolicy

    -- * Destructuring the response
    , ModifyVpcEndpointResponse (..)
    , mkModifyVpcEndpointResponse
    -- ** Response lenses
    , mverrsReturn
    , mverrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyVpcEndpoint.
--
-- /See:/ 'mkModifyVpcEndpoint' smart constructor.
data ModifyVpcEndpoint = ModifyVpcEndpoint'
  { vpcEndpointId :: Types.VpcEndpointId
    -- ^ The ID of the endpoint.
  , addRouteTableIds :: Core.Maybe [Types.RouteTableId]
    -- ^ (Gateway endpoint) One or more route tables IDs to associate with the endpoint.
  , addSecurityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ (Interface endpoint) One or more security group IDs to associate with the network interface.
  , addSubnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ (Interface and Gateway Load Balancer endpoints) One or more subnet IDs in which to serve the endpoint. For a Gateway Load Balancer endpoint, you can specify only one subnet.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , policyDocument :: Core.Maybe Core.Text
    -- ^ (Interface and gateway endpoints) A policy to attach to the endpoint that controls access to the service. The policy must be in valid JSON format.
  , privateDnsEnabled :: Core.Maybe Core.Bool
    -- ^ (Interface endpoint) Indicates whether a private hosted zone is associated with the VPC.
  , removeRouteTableIds :: Core.Maybe [Types.RouteTableId]
    -- ^ (Gateway endpoint) One or more route table IDs to disassociate from the endpoint.
  , removeSecurityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ (Interface endpoint) One or more security group IDs to disassociate from the network interface.
  , removeSubnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ (Interface endpoint) One or more subnets IDs in which to remove the endpoint.
  , resetPolicy :: Core.Maybe Core.Bool
    -- ^ (Gateway endpoint) Specify @true@ to reset the policy document to the default policy. The default policy allows full access to the service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcEndpoint' value with any optional fields omitted.
mkModifyVpcEndpoint
    :: Types.VpcEndpointId -- ^ 'vpcEndpointId'
    -> ModifyVpcEndpoint
mkModifyVpcEndpoint vpcEndpointId
  = ModifyVpcEndpoint'{vpcEndpointId,
                       addRouteTableIds = Core.Nothing,
                       addSecurityGroupIds = Core.Nothing, addSubnetIds = Core.Nothing,
                       dryRun = Core.Nothing, policyDocument = Core.Nothing,
                       privateDnsEnabled = Core.Nothing,
                       removeRouteTableIds = Core.Nothing,
                       removeSecurityGroupIds = Core.Nothing,
                       removeSubnetIds = Core.Nothing, resetPolicy = Core.Nothing}

-- | The ID of the endpoint.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveVpcEndpointId :: Lens.Lens' ModifyVpcEndpoint Types.VpcEndpointId
mveVpcEndpointId = Lens.field @"vpcEndpointId"
{-# INLINEABLE mveVpcEndpointId #-}
{-# DEPRECATED vpcEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead"  #-}

-- | (Gateway endpoint) One or more route tables IDs to associate with the endpoint.
--
-- /Note:/ Consider using 'addRouteTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveAddRouteTableIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Types.RouteTableId])
mveAddRouteTableIds = Lens.field @"addRouteTableIds"
{-# INLINEABLE mveAddRouteTableIds #-}
{-# DEPRECATED addRouteTableIds "Use generic-lens or generic-optics with 'addRouteTableIds' instead"  #-}

-- | (Interface endpoint) One or more security group IDs to associate with the network interface.
--
-- /Note:/ Consider using 'addSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveAddSecurityGroupIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Types.SecurityGroupId])
mveAddSecurityGroupIds = Lens.field @"addSecurityGroupIds"
{-# INLINEABLE mveAddSecurityGroupIds #-}
{-# DEPRECATED addSecurityGroupIds "Use generic-lens or generic-optics with 'addSecurityGroupIds' instead"  #-}

-- | (Interface and Gateway Load Balancer endpoints) One or more subnet IDs in which to serve the endpoint. For a Gateway Load Balancer endpoint, you can specify only one subnet.
--
-- /Note:/ Consider using 'addSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveAddSubnetIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Types.SubnetId])
mveAddSubnetIds = Lens.field @"addSubnetIds"
{-# INLINEABLE mveAddSubnetIds #-}
{-# DEPRECATED addSubnetIds "Use generic-lens or generic-optics with 'addSubnetIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveDryRun :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe Core.Bool)
mveDryRun = Lens.field @"dryRun"
{-# INLINEABLE mveDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | (Interface and gateway endpoints) A policy to attach to the endpoint that controls access to the service. The policy must be in valid JSON format.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvePolicyDocument :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe Core.Text)
mvePolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE mvePolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

-- | (Interface endpoint) Indicates whether a private hosted zone is associated with the VPC.
--
-- /Note:/ Consider using 'privateDnsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvePrivateDnsEnabled :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe Core.Bool)
mvePrivateDnsEnabled = Lens.field @"privateDnsEnabled"
{-# INLINEABLE mvePrivateDnsEnabled #-}
{-# DEPRECATED privateDnsEnabled "Use generic-lens or generic-optics with 'privateDnsEnabled' instead"  #-}

-- | (Gateway endpoint) One or more route table IDs to disassociate from the endpoint.
--
-- /Note:/ Consider using 'removeRouteTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveRemoveRouteTableIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Types.RouteTableId])
mveRemoveRouteTableIds = Lens.field @"removeRouteTableIds"
{-# INLINEABLE mveRemoveRouteTableIds #-}
{-# DEPRECATED removeRouteTableIds "Use generic-lens or generic-optics with 'removeRouteTableIds' instead"  #-}

-- | (Interface endpoint) One or more security group IDs to disassociate from the network interface.
--
-- /Note:/ Consider using 'removeSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveRemoveSecurityGroupIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Types.SecurityGroupId])
mveRemoveSecurityGroupIds = Lens.field @"removeSecurityGroupIds"
{-# INLINEABLE mveRemoveSecurityGroupIds #-}
{-# DEPRECATED removeSecurityGroupIds "Use generic-lens or generic-optics with 'removeSecurityGroupIds' instead"  #-}

-- | (Interface endpoint) One or more subnets IDs in which to remove the endpoint.
--
-- /Note:/ Consider using 'removeSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveRemoveSubnetIds :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe [Types.SubnetId])
mveRemoveSubnetIds = Lens.field @"removeSubnetIds"
{-# INLINEABLE mveRemoveSubnetIds #-}
{-# DEPRECATED removeSubnetIds "Use generic-lens or generic-optics with 'removeSubnetIds' instead"  #-}

-- | (Gateway endpoint) Specify @true@ to reset the policy document to the default policy. The default policy allows full access to the service.
--
-- /Note:/ Consider using 'resetPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveResetPolicy :: Lens.Lens' ModifyVpcEndpoint (Core.Maybe Core.Bool)
mveResetPolicy = Lens.field @"resetPolicy"
{-# INLINEABLE mveResetPolicy #-}
{-# DEPRECATED resetPolicy "Use generic-lens or generic-optics with 'resetPolicy' instead"  #-}

instance Core.ToQuery ModifyVpcEndpoint where
        toQuery ModifyVpcEndpoint{..}
          = Core.toQueryPair "Action" ("ModifyVpcEndpoint" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcEndpointId" vpcEndpointId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AddRouteTableId")
                addRouteTableIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AddSecurityGroupId")
                addSecurityGroupIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AddSubnetId")
                addSubnetIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PolicyDocument")
                policyDocument
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrivateDnsEnabled")
                privateDnsEnabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RemoveRouteTableId")
                removeRouteTableIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RemoveSecurityGroupId")
                removeSecurityGroupIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RemoveSubnetId")
                removeSubnetIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResetPolicy") resetPolicy

instance Core.ToHeaders ModifyVpcEndpoint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpcEndpoint where
        type Rs ModifyVpcEndpoint = ModifyVpcEndpointResponse
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
                 ModifyVpcEndpointResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpcEndpointResponse' smart constructor.
data ModifyVpcEndpointResponse = ModifyVpcEndpointResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcEndpointResponse' value with any optional fields omitted.
mkModifyVpcEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVpcEndpointResponse
mkModifyVpcEndpointResponse responseStatus
  = ModifyVpcEndpointResponse'{return = Core.Nothing, responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mverrsReturn :: Lens.Lens' ModifyVpcEndpointResponse (Core.Maybe Core.Bool)
mverrsReturn = Lens.field @"return"
{-# INLINEABLE mverrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mverrsResponseStatus :: Lens.Lens' ModifyVpcEndpointResponse Core.Int
mverrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mverrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
