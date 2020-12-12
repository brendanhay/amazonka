{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTableAssociation
  ( RouteTableAssociation (..),

    -- * Smart constructor
    mkRouteTableAssociation,

    -- * Lenses
    rtaRouteTableId,
    rtaRouteTableAssociationId,
    rtaMain,
    rtaSubnetId,
    rtaGatewayId,
    rtaAssociationState,
  )
where

import Network.AWS.EC2.Types.RouteTableAssociationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an association between a route table and a subnet or gateway.
--
-- /See:/ 'mkRouteTableAssociation' smart constructor.
data RouteTableAssociation = RouteTableAssociation'
  { routeTableId ::
      Lude.Maybe Lude.Text,
    routeTableAssociationId :: Lude.Maybe Lude.Text,
    main :: Lude.Maybe Lude.Bool,
    subnetId :: Lude.Maybe Lude.Text,
    gatewayId :: Lude.Maybe Lude.Text,
    associationState ::
      Lude.Maybe RouteTableAssociationState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RouteTableAssociation' with the minimum fields required to make a request.
--
-- * 'associationState' - The state of the association.
-- * 'gatewayId' - The ID of the internet gateway or virtual private gateway.
-- * 'main' - Indicates whether this is the main route table.
-- * 'routeTableAssociationId' - The ID of the association.
-- * 'routeTableId' - The ID of the route table.
-- * 'subnetId' - The ID of the subnet. A subnet ID is not returned for an implicit association.
mkRouteTableAssociation ::
  RouteTableAssociation
mkRouteTableAssociation =
  RouteTableAssociation'
    { routeTableId = Lude.Nothing,
      routeTableAssociationId = Lude.Nothing,
      main = Lude.Nothing,
      subnetId = Lude.Nothing,
      gatewayId = Lude.Nothing,
      associationState = Lude.Nothing
    }

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaRouteTableId :: Lens.Lens' RouteTableAssociation (Lude.Maybe Lude.Text)
rtaRouteTableId = Lens.lens (routeTableId :: RouteTableAssociation -> Lude.Maybe Lude.Text) (\s a -> s {routeTableId = a} :: RouteTableAssociation)
{-# DEPRECATED rtaRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'routeTableAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaRouteTableAssociationId :: Lens.Lens' RouteTableAssociation (Lude.Maybe Lude.Text)
rtaRouteTableAssociationId = Lens.lens (routeTableAssociationId :: RouteTableAssociation -> Lude.Maybe Lude.Text) (\s a -> s {routeTableAssociationId = a} :: RouteTableAssociation)
{-# DEPRECATED rtaRouteTableAssociationId "Use generic-lens or generic-optics with 'routeTableAssociationId' instead." #-}

-- | Indicates whether this is the main route table.
--
-- /Note:/ Consider using 'main' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaMain :: Lens.Lens' RouteTableAssociation (Lude.Maybe Lude.Bool)
rtaMain = Lens.lens (main :: RouteTableAssociation -> Lude.Maybe Lude.Bool) (\s a -> s {main = a} :: RouteTableAssociation)
{-# DEPRECATED rtaMain "Use generic-lens or generic-optics with 'main' instead." #-}

-- | The ID of the subnet. A subnet ID is not returned for an implicit association.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaSubnetId :: Lens.Lens' RouteTableAssociation (Lude.Maybe Lude.Text)
rtaSubnetId = Lens.lens (subnetId :: RouteTableAssociation -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: RouteTableAssociation)
{-# DEPRECATED rtaSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the internet gateway or virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaGatewayId :: Lens.Lens' RouteTableAssociation (Lude.Maybe Lude.Text)
rtaGatewayId = Lens.lens (gatewayId :: RouteTableAssociation -> Lude.Maybe Lude.Text) (\s a -> s {gatewayId = a} :: RouteTableAssociation)
{-# DEPRECATED rtaGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The state of the association.
--
-- /Note:/ Consider using 'associationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaAssociationState :: Lens.Lens' RouteTableAssociation (Lude.Maybe RouteTableAssociationState)
rtaAssociationState = Lens.lens (associationState :: RouteTableAssociation -> Lude.Maybe RouteTableAssociationState) (\s a -> s {associationState = a} :: RouteTableAssociation)
{-# DEPRECATED rtaAssociationState "Use generic-lens or generic-optics with 'associationState' instead." #-}

instance Lude.FromXML RouteTableAssociation where
  parseXML x =
    RouteTableAssociation'
      Lude.<$> (x Lude..@? "routeTableId")
      Lude.<*> (x Lude..@? "routeTableAssociationId")
      Lude.<*> (x Lude..@? "main")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "gatewayId")
      Lude.<*> (x Lude..@? "associationState")
