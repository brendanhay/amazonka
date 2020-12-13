{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPCEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes of a specified VPC endpoint. The attributes that you can modify depend on the type of VPC endpoint (interface, gateway, or Gateway Load Balancer). For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-endpoints.html VPC Endpoints> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.ModifyVPCEndpoint
  ( -- * Creating a request
    ModifyVPCEndpoint (..),
    mkModifyVPCEndpoint,

    -- ** Request lenses
    mvePolicyDocument,
    mveRemoveRouteTableIds,
    mveResetPolicy,
    mveAddRouteTableIds,
    mvePrivateDNSEnabled,
    mveAddSubnetIds,
    mveRemoveSubnetIds,
    mveVPCEndpointId,
    mveAddSecurityGroupIds,
    mveDryRun,
    mveRemoveSecurityGroupIds,

    -- * Destructuring the response
    ModifyVPCEndpointResponse (..),
    mkModifyVPCEndpointResponse,

    -- ** Response lenses
    mversReturn,
    mversResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ModifyVpcEndpoint.
--
-- /See:/ 'mkModifyVPCEndpoint' smart constructor.
data ModifyVPCEndpoint = ModifyVPCEndpoint'
  { -- | (Interface and gateway endpoints) A policy to attach to the endpoint that controls access to the service. The policy must be in valid JSON format.
    policyDocument :: Lude.Maybe Lude.Text,
    -- | (Gateway endpoint) One or more route table IDs to disassociate from the endpoint.
    removeRouteTableIds :: Lude.Maybe [Lude.Text],
    -- | (Gateway endpoint) Specify @true@ to reset the policy document to the default policy. The default policy allows full access to the service.
    resetPolicy :: Lude.Maybe Lude.Bool,
    -- | (Gateway endpoint) One or more route tables IDs to associate with the endpoint.
    addRouteTableIds :: Lude.Maybe [Lude.Text],
    -- | (Interface endpoint) Indicates whether a private hosted zone is associated with the VPC.
    privateDNSEnabled :: Lude.Maybe Lude.Bool,
    -- | (Interface and Gateway Load Balancer endpoints) One or more subnet IDs in which to serve the endpoint. For a Gateway Load Balancer endpoint, you can specify only one subnet.
    addSubnetIds :: Lude.Maybe [Lude.Text],
    -- | (Interface endpoint) One or more subnets IDs in which to remove the endpoint.
    removeSubnetIds :: Lude.Maybe [Lude.Text],
    -- | The ID of the endpoint.
    vpcEndpointId :: Lude.Text,
    -- | (Interface endpoint) One or more security group IDs to associate with the network interface.
    addSecurityGroupIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | (Interface endpoint) One or more security group IDs to disassociate from the network interface.
    removeSecurityGroupIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCEndpoint' with the minimum fields required to make a request.
--
-- * 'policyDocument' - (Interface and gateway endpoints) A policy to attach to the endpoint that controls access to the service. The policy must be in valid JSON format.
-- * 'removeRouteTableIds' - (Gateway endpoint) One or more route table IDs to disassociate from the endpoint.
-- * 'resetPolicy' - (Gateway endpoint) Specify @true@ to reset the policy document to the default policy. The default policy allows full access to the service.
-- * 'addRouteTableIds' - (Gateway endpoint) One or more route tables IDs to associate with the endpoint.
-- * 'privateDNSEnabled' - (Interface endpoint) Indicates whether a private hosted zone is associated with the VPC.
-- * 'addSubnetIds' - (Interface and Gateway Load Balancer endpoints) One or more subnet IDs in which to serve the endpoint. For a Gateway Load Balancer endpoint, you can specify only one subnet.
-- * 'removeSubnetIds' - (Interface endpoint) One or more subnets IDs in which to remove the endpoint.
-- * 'vpcEndpointId' - The ID of the endpoint.
-- * 'addSecurityGroupIds' - (Interface endpoint) One or more security group IDs to associate with the network interface.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'removeSecurityGroupIds' - (Interface endpoint) One or more security group IDs to disassociate from the network interface.
mkModifyVPCEndpoint ::
  -- | 'vpcEndpointId'
  Lude.Text ->
  ModifyVPCEndpoint
mkModifyVPCEndpoint pVPCEndpointId_ =
  ModifyVPCEndpoint'
    { policyDocument = Lude.Nothing,
      removeRouteTableIds = Lude.Nothing,
      resetPolicy = Lude.Nothing,
      addRouteTableIds = Lude.Nothing,
      privateDNSEnabled = Lude.Nothing,
      addSubnetIds = Lude.Nothing,
      removeSubnetIds = Lude.Nothing,
      vpcEndpointId = pVPCEndpointId_,
      addSecurityGroupIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      removeSecurityGroupIds = Lude.Nothing
    }

-- | (Interface and gateway endpoints) A policy to attach to the endpoint that controls access to the service. The policy must be in valid JSON format.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvePolicyDocument :: Lens.Lens' ModifyVPCEndpoint (Lude.Maybe Lude.Text)
mvePolicyDocument = Lens.lens (policyDocument :: ModifyVPCEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {policyDocument = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mvePolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | (Gateway endpoint) One or more route table IDs to disassociate from the endpoint.
--
-- /Note:/ Consider using 'removeRouteTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveRemoveRouteTableIds :: Lens.Lens' ModifyVPCEndpoint (Lude.Maybe [Lude.Text])
mveRemoveRouteTableIds = Lens.lens (removeRouteTableIds :: ModifyVPCEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {removeRouteTableIds = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mveRemoveRouteTableIds "Use generic-lens or generic-optics with 'removeRouteTableIds' instead." #-}

-- | (Gateway endpoint) Specify @true@ to reset the policy document to the default policy. The default policy allows full access to the service.
--
-- /Note:/ Consider using 'resetPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveResetPolicy :: Lens.Lens' ModifyVPCEndpoint (Lude.Maybe Lude.Bool)
mveResetPolicy = Lens.lens (resetPolicy :: ModifyVPCEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {resetPolicy = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mveResetPolicy "Use generic-lens or generic-optics with 'resetPolicy' instead." #-}

-- | (Gateway endpoint) One or more route tables IDs to associate with the endpoint.
--
-- /Note:/ Consider using 'addRouteTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveAddRouteTableIds :: Lens.Lens' ModifyVPCEndpoint (Lude.Maybe [Lude.Text])
mveAddRouteTableIds = Lens.lens (addRouteTableIds :: ModifyVPCEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {addRouteTableIds = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mveAddRouteTableIds "Use generic-lens or generic-optics with 'addRouteTableIds' instead." #-}

-- | (Interface endpoint) Indicates whether a private hosted zone is associated with the VPC.
--
-- /Note:/ Consider using 'privateDNSEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvePrivateDNSEnabled :: Lens.Lens' ModifyVPCEndpoint (Lude.Maybe Lude.Bool)
mvePrivateDNSEnabled = Lens.lens (privateDNSEnabled :: ModifyVPCEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {privateDNSEnabled = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mvePrivateDNSEnabled "Use generic-lens or generic-optics with 'privateDNSEnabled' instead." #-}

-- | (Interface and Gateway Load Balancer endpoints) One or more subnet IDs in which to serve the endpoint. For a Gateway Load Balancer endpoint, you can specify only one subnet.
--
-- /Note:/ Consider using 'addSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveAddSubnetIds :: Lens.Lens' ModifyVPCEndpoint (Lude.Maybe [Lude.Text])
mveAddSubnetIds = Lens.lens (addSubnetIds :: ModifyVPCEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {addSubnetIds = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mveAddSubnetIds "Use generic-lens or generic-optics with 'addSubnetIds' instead." #-}

-- | (Interface endpoint) One or more subnets IDs in which to remove the endpoint.
--
-- /Note:/ Consider using 'removeSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveRemoveSubnetIds :: Lens.Lens' ModifyVPCEndpoint (Lude.Maybe [Lude.Text])
mveRemoveSubnetIds = Lens.lens (removeSubnetIds :: ModifyVPCEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {removeSubnetIds = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mveRemoveSubnetIds "Use generic-lens or generic-optics with 'removeSubnetIds' instead." #-}

-- | The ID of the endpoint.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveVPCEndpointId :: Lens.Lens' ModifyVPCEndpoint Lude.Text
mveVPCEndpointId = Lens.lens (vpcEndpointId :: ModifyVPCEndpoint -> Lude.Text) (\s a -> s {vpcEndpointId = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mveVPCEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead." #-}

-- | (Interface endpoint) One or more security group IDs to associate with the network interface.
--
-- /Note:/ Consider using 'addSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveAddSecurityGroupIds :: Lens.Lens' ModifyVPCEndpoint (Lude.Maybe [Lude.Text])
mveAddSecurityGroupIds = Lens.lens (addSecurityGroupIds :: ModifyVPCEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {addSecurityGroupIds = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mveAddSecurityGroupIds "Use generic-lens or generic-optics with 'addSecurityGroupIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveDryRun :: Lens.Lens' ModifyVPCEndpoint (Lude.Maybe Lude.Bool)
mveDryRun = Lens.lens (dryRun :: ModifyVPCEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mveDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | (Interface endpoint) One or more security group IDs to disassociate from the network interface.
--
-- /Note:/ Consider using 'removeSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mveRemoveSecurityGroupIds :: Lens.Lens' ModifyVPCEndpoint (Lude.Maybe [Lude.Text])
mveRemoveSecurityGroupIds = Lens.lens (removeSecurityGroupIds :: ModifyVPCEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {removeSecurityGroupIds = a} :: ModifyVPCEndpoint)
{-# DEPRECATED mveRemoveSecurityGroupIds "Use generic-lens or generic-optics with 'removeSecurityGroupIds' instead." #-}

instance Lude.AWSRequest ModifyVPCEndpoint where
  type Rs ModifyVPCEndpoint = ModifyVPCEndpointResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVPCEndpointResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVPCEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPCEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPCEndpoint where
  toQuery ModifyVPCEndpoint' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyVpcEndpoint" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "PolicyDocument" Lude.=: policyDocument,
        Lude.toQuery
          ( Lude.toQueryList "RemoveRouteTableId"
              Lude.<$> removeRouteTableIds
          ),
        "ResetPolicy" Lude.=: resetPolicy,
        Lude.toQuery
          (Lude.toQueryList "AddRouteTableId" Lude.<$> addRouteTableIds),
        "PrivateDnsEnabled" Lude.=: privateDNSEnabled,
        Lude.toQuery
          (Lude.toQueryList "AddSubnetId" Lude.<$> addSubnetIds),
        Lude.toQuery
          (Lude.toQueryList "RemoveSubnetId" Lude.<$> removeSubnetIds),
        "VpcEndpointId" Lude.=: vpcEndpointId,
        Lude.toQuery
          ( Lude.toQueryList "AddSecurityGroupId"
              Lude.<$> addSecurityGroupIds
          ),
        "DryRun" Lude.=: dryRun,
        Lude.toQuery
          ( Lude.toQueryList "RemoveSecurityGroupId"
              Lude.<$> removeSecurityGroupIds
          )
      ]

-- | /See:/ 'mkModifyVPCEndpointResponse' smart constructor.
data ModifyVPCEndpointResponse = ModifyVPCEndpointResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCEndpointResponse' with the minimum fields required to make a request.
--
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
-- * 'responseStatus' - The response status code.
mkModifyVPCEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVPCEndpointResponse
mkModifyVPCEndpointResponse pResponseStatus_ =
  ModifyVPCEndpointResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mversReturn :: Lens.Lens' ModifyVPCEndpointResponse (Lude.Maybe Lude.Bool)
mversReturn = Lens.lens (return :: ModifyVPCEndpointResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ModifyVPCEndpointResponse)
{-# DEPRECATED mversReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mversResponseStatus :: Lens.Lens' ModifyVPCEndpointResponse Lude.Int
mversResponseStatus = Lens.lens (responseStatus :: ModifyVPCEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVPCEndpointResponse)
{-# DEPRECATED mversResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
