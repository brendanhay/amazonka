{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNatGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a NAT gateway in the specified public subnet. This action creates a network interface in the specified subnet with a private IP address from the IP address range of the subnet. Internet-bound traffic from a private subnet can be routed to the NAT gateway, therefore enabling instances in the private subnet to connect to the internet. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html NAT Gateways> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateNatGateway
  ( -- * Creating a request
    CreateNatGateway (..),
    mkCreateNatGateway,

    -- ** Request lenses
    cngClientToken,
    cngTagSpecifications,
    cngDryRun,
    cngAllocationId,
    cngSubnetId,

    -- * Destructuring the response
    CreateNatGatewayResponse (..),
    mkCreateNatGatewayResponse,

    -- ** Response lenses
    cngrsClientToken,
    cngrsNatGateway,
    cngrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateNatGateway' smart constructor.
data CreateNatGateway = CreateNatGateway'
  { clientToken ::
      Lude.Maybe Lude.Text,
    tagSpecifications :: Lude.Maybe [TagSpecification],
    dryRun :: Lude.Maybe Lude.Bool,
    allocationId :: Lude.Text,
    subnetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNatGateway' with the minimum fields required to make a request.
--
-- * 'allocationId' - The allocation ID of an Elastic IP address to associate with the NAT gateway. If the Elastic IP address is associated with another resource, you must first disassociate it.
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- Constraint: Maximum 64 ASCII characters.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'subnetId' - The subnet in which to create the NAT gateway.
-- * 'tagSpecifications' - The tags to assign to the NAT gateway.
mkCreateNatGateway ::
  -- | 'allocationId'
  Lude.Text ->
  -- | 'subnetId'
  Lude.Text ->
  CreateNatGateway
mkCreateNatGateway pAllocationId_ pSubnetId_ =
  CreateNatGateway'
    { clientToken = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      dryRun = Lude.Nothing,
      allocationId = pAllocationId_,
      subnetId = pSubnetId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- Constraint: Maximum 64 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngClientToken :: Lens.Lens' CreateNatGateway (Lude.Maybe Lude.Text)
cngClientToken = Lens.lens (clientToken :: CreateNatGateway -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateNatGateway)
{-# DEPRECATED cngClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The tags to assign to the NAT gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngTagSpecifications :: Lens.Lens' CreateNatGateway (Lude.Maybe [TagSpecification])
cngTagSpecifications = Lens.lens (tagSpecifications :: CreateNatGateway -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateNatGateway)
{-# DEPRECATED cngTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngDryRun :: Lens.Lens' CreateNatGateway (Lude.Maybe Lude.Bool)
cngDryRun = Lens.lens (dryRun :: CreateNatGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateNatGateway)
{-# DEPRECATED cngDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The allocation ID of an Elastic IP address to associate with the NAT gateway. If the Elastic IP address is associated with another resource, you must first disassociate it.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngAllocationId :: Lens.Lens' CreateNatGateway Lude.Text
cngAllocationId = Lens.lens (allocationId :: CreateNatGateway -> Lude.Text) (\s a -> s {allocationId = a} :: CreateNatGateway)
{-# DEPRECATED cngAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The subnet in which to create the NAT gateway.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngSubnetId :: Lens.Lens' CreateNatGateway Lude.Text
cngSubnetId = Lens.lens (subnetId :: CreateNatGateway -> Lude.Text) (\s a -> s {subnetId = a} :: CreateNatGateway)
{-# DEPRECATED cngSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

instance Lude.AWSRequest CreateNatGateway where
  type Rs CreateNatGateway = CreateNatGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateNatGatewayResponse'
            Lude.<$> (x Lude..@? "clientToken")
            Lude.<*> (x Lude..@? "natGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNatGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateNatGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNatGateway where
  toQuery CreateNatGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateNatGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun,
        "AllocationId" Lude.=: allocationId,
        "SubnetId" Lude.=: subnetId
      ]

-- | /See:/ 'mkCreateNatGatewayResponse' smart constructor.
data CreateNatGatewayResponse = CreateNatGatewayResponse'
  { clientToken ::
      Lude.Maybe Lude.Text,
    natGateway :: Lude.Maybe NatGateway,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNatGatewayResponse' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier to ensure the idempotency of the request. Only returned if a client token was provided in the request.
-- * 'natGateway' - Information about the NAT gateway.
-- * 'responseStatus' - The response status code.
mkCreateNatGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNatGatewayResponse
mkCreateNatGatewayResponse pResponseStatus_ =
  CreateNatGatewayResponse'
    { clientToken = Lude.Nothing,
      natGateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier to ensure the idempotency of the request. Only returned if a client token was provided in the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngrsClientToken :: Lens.Lens' CreateNatGatewayResponse (Lude.Maybe Lude.Text)
cngrsClientToken = Lens.lens (clientToken :: CreateNatGatewayResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateNatGatewayResponse)
{-# DEPRECATED cngrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Information about the NAT gateway.
--
-- /Note:/ Consider using 'natGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngrsNatGateway :: Lens.Lens' CreateNatGatewayResponse (Lude.Maybe NatGateway)
cngrsNatGateway = Lens.lens (natGateway :: CreateNatGatewayResponse -> Lude.Maybe NatGateway) (\s a -> s {natGateway = a} :: CreateNatGatewayResponse)
{-# DEPRECATED cngrsNatGateway "Use generic-lens or generic-optics with 'natGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngrsResponseStatus :: Lens.Lens' CreateNatGatewayResponse Lude.Int
cngrsResponseStatus = Lens.lens (responseStatus :: CreateNatGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNatGatewayResponse)
{-# DEPRECATED cngrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
