{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a subnet in your VPC or an internet gateway or virtual private gateway attached to your VPC with a route table in your VPC. This association causes traffic from the subnet or gateway to be routed according to the routes in the route table. The action returns an association ID, which you need in order to disassociate the route table later. A route table can be associated with multiple subnets.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.AssociateRouteTable
  ( -- * Creating a request
    AssociateRouteTable (..),
    mkAssociateRouteTable,

    -- ** Request lenses
    artSubnetId,
    artGatewayId,
    artDryRun,
    artRouteTableId,

    -- * Destructuring the response
    AssociateRouteTableResponse (..),
    mkAssociateRouteTableResponse,

    -- ** Response lenses
    artrsAssociationId,
    artrsAssociationState,
    artrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateRouteTable' smart constructor.
data AssociateRouteTable = AssociateRouteTable'
  { subnetId ::
      Lude.Maybe Lude.Text,
    gatewayId :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    routeTableId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateRouteTable' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'gatewayId' - The ID of the internet gateway or virtual private gateway.
-- * 'routeTableId' - The ID of the route table.
-- * 'subnetId' - The ID of the subnet.
mkAssociateRouteTable ::
  -- | 'routeTableId'
  Lude.Text ->
  AssociateRouteTable
mkAssociateRouteTable pRouteTableId_ =
  AssociateRouteTable'
    { subnetId = Lude.Nothing,
      gatewayId = Lude.Nothing,
      dryRun = Lude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artSubnetId :: Lens.Lens' AssociateRouteTable (Lude.Maybe Lude.Text)
artSubnetId = Lens.lens (subnetId :: AssociateRouteTable -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: AssociateRouteTable)
{-# DEPRECATED artSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the internet gateway or virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artGatewayId :: Lens.Lens' AssociateRouteTable (Lude.Maybe Lude.Text)
artGatewayId = Lens.lens (gatewayId :: AssociateRouteTable -> Lude.Maybe Lude.Text) (\s a -> s {gatewayId = a} :: AssociateRouteTable)
{-# DEPRECATED artGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artDryRun :: Lens.Lens' AssociateRouteTable (Lude.Maybe Lude.Bool)
artDryRun = Lens.lens (dryRun :: AssociateRouteTable -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AssociateRouteTable)
{-# DEPRECATED artDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artRouteTableId :: Lens.Lens' AssociateRouteTable Lude.Text
artRouteTableId = Lens.lens (routeTableId :: AssociateRouteTable -> Lude.Text) (\s a -> s {routeTableId = a} :: AssociateRouteTable)
{-# DEPRECATED artRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

instance Lude.AWSRequest AssociateRouteTable where
  type Rs AssociateRouteTable = AssociateRouteTableResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssociateRouteTableResponse'
            Lude.<$> (x Lude..@? "associationId")
            Lude.<*> (x Lude..@? "associationState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateRouteTable where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateRouteTable where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateRouteTable where
  toQuery AssociateRouteTable' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AssociateRouteTable" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "SubnetId" Lude.=: subnetId,
        "GatewayId" Lude.=: gatewayId,
        "DryRun" Lude.=: dryRun,
        "RouteTableId" Lude.=: routeTableId
      ]

-- | /See:/ 'mkAssociateRouteTableResponse' smart constructor.
data AssociateRouteTableResponse = AssociateRouteTableResponse'
  { associationId ::
      Lude.Maybe Lude.Text,
    associationState ::
      Lude.Maybe
        RouteTableAssociationState,
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

-- | Creates a value of 'AssociateRouteTableResponse' with the minimum fields required to make a request.
--
-- * 'associationId' - The route table association ID. This ID is required for disassociating the route table.
-- * 'associationState' - The state of the association.
-- * 'responseStatus' - The response status code.
mkAssociateRouteTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateRouteTableResponse
mkAssociateRouteTableResponse pResponseStatus_ =
  AssociateRouteTableResponse'
    { associationId = Lude.Nothing,
      associationState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The route table association ID. This ID is required for disassociating the route table.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artrsAssociationId :: Lens.Lens' AssociateRouteTableResponse (Lude.Maybe Lude.Text)
artrsAssociationId = Lens.lens (associationId :: AssociateRouteTableResponse -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: AssociateRouteTableResponse)
{-# DEPRECATED artrsAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The state of the association.
--
-- /Note:/ Consider using 'associationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artrsAssociationState :: Lens.Lens' AssociateRouteTableResponse (Lude.Maybe RouteTableAssociationState)
artrsAssociationState = Lens.lens (associationState :: AssociateRouteTableResponse -> Lude.Maybe RouteTableAssociationState) (\s a -> s {associationState = a} :: AssociateRouteTableResponse)
{-# DEPRECATED artrsAssociationState "Use generic-lens or generic-optics with 'associationState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artrsResponseStatus :: Lens.Lens' AssociateRouteTableResponse Lude.Int
artrsResponseStatus = Lens.lens (responseStatus :: AssociateRouteTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateRouteTableResponse)
{-# DEPRECATED artrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
