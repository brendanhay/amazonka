-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteTable
  ( TransitGatewayRouteTable (..),

    -- * Smart constructor
    mkTransitGatewayRouteTable,

    -- * Lenses
    tgrtCreationTime,
    tgrtState,
    tgrtDefaultPropagationRouteTable,
    tgrtTransitGatewayRouteTableId,
    tgrtTransitGatewayId,
    tgrtDefaultAssociationRouteTable,
    tgrtTags,
  )
where

import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayRouteTableState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a transit gateway route table.
--
-- /See:/ 'mkTransitGatewayRouteTable' smart constructor.
data TransitGatewayRouteTable = TransitGatewayRouteTable'
  { creationTime ::
      Lude.Maybe Lude.ISO8601,
    state ::
      Lude.Maybe TransitGatewayRouteTableState,
    defaultPropagationRouteTable ::
      Lude.Maybe Lude.Bool,
    transitGatewayRouteTableId ::
      Lude.Maybe Lude.Text,
    transitGatewayId :: Lude.Maybe Lude.Text,
    defaultAssociationRouteTable ::
      Lude.Maybe Lude.Bool,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayRouteTable' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time.
-- * 'defaultAssociationRouteTable' - Indicates whether this is the default association route table for the transit gateway.
-- * 'defaultPropagationRouteTable' - Indicates whether this is the default propagation route table for the transit gateway.
-- * 'state' - The state of the transit gateway route table.
-- * 'tags' - Any tags assigned to the route table.
-- * 'transitGatewayId' - The ID of the transit gateway.
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
mkTransitGatewayRouteTable ::
  TransitGatewayRouteTable
mkTransitGatewayRouteTable =
  TransitGatewayRouteTable'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      defaultPropagationRouteTable = Lude.Nothing,
      transitGatewayRouteTableId = Lude.Nothing,
      transitGatewayId = Lude.Nothing,
      defaultAssociationRouteTable = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtCreationTime :: Lens.Lens' TransitGatewayRouteTable (Lude.Maybe Lude.ISO8601)
tgrtCreationTime = Lens.lens (creationTime :: TransitGatewayRouteTable -> Lude.Maybe Lude.ISO8601) (\s a -> s {creationTime = a} :: TransitGatewayRouteTable)
{-# DEPRECATED tgrtCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the transit gateway route table.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtState :: Lens.Lens' TransitGatewayRouteTable (Lude.Maybe TransitGatewayRouteTableState)
tgrtState = Lens.lens (state :: TransitGatewayRouteTable -> Lude.Maybe TransitGatewayRouteTableState) (\s a -> s {state = a} :: TransitGatewayRouteTable)
{-# DEPRECATED tgrtState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Indicates whether this is the default propagation route table for the transit gateway.
--
-- /Note:/ Consider using 'defaultPropagationRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtDefaultPropagationRouteTable :: Lens.Lens' TransitGatewayRouteTable (Lude.Maybe Lude.Bool)
tgrtDefaultPropagationRouteTable = Lens.lens (defaultPropagationRouteTable :: TransitGatewayRouteTable -> Lude.Maybe Lude.Bool) (\s a -> s {defaultPropagationRouteTable = a} :: TransitGatewayRouteTable)
{-# DEPRECATED tgrtDefaultPropagationRouteTable "Use generic-lens or generic-optics with 'defaultPropagationRouteTable' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayRouteTable (Lude.Maybe Lude.Text)
tgrtTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: TransitGatewayRouteTable -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: TransitGatewayRouteTable)
{-# DEPRECATED tgrtTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtTransitGatewayId :: Lens.Lens' TransitGatewayRouteTable (Lude.Maybe Lude.Text)
tgrtTransitGatewayId = Lens.lens (transitGatewayId :: TransitGatewayRouteTable -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: TransitGatewayRouteTable)
{-# DEPRECATED tgrtTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | Indicates whether this is the default association route table for the transit gateway.
--
-- /Note:/ Consider using 'defaultAssociationRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtDefaultAssociationRouteTable :: Lens.Lens' TransitGatewayRouteTable (Lude.Maybe Lude.Bool)
tgrtDefaultAssociationRouteTable = Lens.lens (defaultAssociationRouteTable :: TransitGatewayRouteTable -> Lude.Maybe Lude.Bool) (\s a -> s {defaultAssociationRouteTable = a} :: TransitGatewayRouteTable)
{-# DEPRECATED tgrtDefaultAssociationRouteTable "Use generic-lens or generic-optics with 'defaultAssociationRouteTable' instead." #-}

-- | Any tags assigned to the route table.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtTags :: Lens.Lens' TransitGatewayRouteTable (Lude.Maybe [Tag])
tgrtTags = Lens.lens (tags :: TransitGatewayRouteTable -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TransitGatewayRouteTable)
{-# DEPRECATED tgrtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML TransitGatewayRouteTable where
  parseXML x =
    TransitGatewayRouteTable'
      Lude.<$> (x Lude..@? "creationTime")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "defaultPropagationRouteTable")
      Lude.<*> (x Lude..@? "transitGatewayRouteTableId")
      Lude.<*> (x Lude..@? "transitGatewayId")
      Lude.<*> (x Lude..@? "defaultAssociationRouteTable")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
