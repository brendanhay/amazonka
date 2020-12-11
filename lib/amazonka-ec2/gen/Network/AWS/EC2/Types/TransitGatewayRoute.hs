-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRoute
  ( TransitGatewayRoute (..),

    -- * Smart constructor
    mkTransitGatewayRoute,

    -- * Lenses
    tgrState,
    tgrPrefixListId,
    tgrTransitGatewayAttachments,
    tgrType,
    tgrDestinationCidrBlock,
  )
where

import Network.AWS.EC2.Types.TransitGatewayRouteAttachment
import Network.AWS.EC2.Types.TransitGatewayRouteState
import Network.AWS.EC2.Types.TransitGatewayRouteType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a route for a transit gateway route table.
--
-- /See:/ 'mkTransitGatewayRoute' smart constructor.
data TransitGatewayRoute = TransitGatewayRoute'
  { state ::
      Lude.Maybe TransitGatewayRouteState,
    prefixListId :: Lude.Maybe Lude.Text,
    transitGatewayAttachments ::
      Lude.Maybe [TransitGatewayRouteAttachment],
    type' :: Lude.Maybe TransitGatewayRouteType,
    destinationCidrBlock :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayRoute' with the minimum fields required to make a request.
--
-- * 'destinationCidrBlock' - The CIDR block used for destination matches.
-- * 'prefixListId' - The ID of the prefix list used for destination matches.
-- * 'state' - The state of the route.
-- * 'transitGatewayAttachments' - The attachments.
-- * 'type'' - The route type.
mkTransitGatewayRoute ::
  TransitGatewayRoute
mkTransitGatewayRoute =
  TransitGatewayRoute'
    { state = Lude.Nothing,
      prefixListId = Lude.Nothing,
      transitGatewayAttachments = Lude.Nothing,
      type' = Lude.Nothing,
      destinationCidrBlock = Lude.Nothing
    }

-- | The state of the route.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrState :: Lens.Lens' TransitGatewayRoute (Lude.Maybe TransitGatewayRouteState)
tgrState = Lens.lens (state :: TransitGatewayRoute -> Lude.Maybe TransitGatewayRouteState) (\s a -> s {state = a} :: TransitGatewayRoute)
{-# DEPRECATED tgrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the prefix list used for destination matches.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrPrefixListId :: Lens.Lens' TransitGatewayRoute (Lude.Maybe Lude.Text)
tgrPrefixListId = Lens.lens (prefixListId :: TransitGatewayRoute -> Lude.Maybe Lude.Text) (\s a -> s {prefixListId = a} :: TransitGatewayRoute)
{-# DEPRECATED tgrPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | The attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrTransitGatewayAttachments :: Lens.Lens' TransitGatewayRoute (Lude.Maybe [TransitGatewayRouteAttachment])
tgrTransitGatewayAttachments = Lens.lens (transitGatewayAttachments :: TransitGatewayRoute -> Lude.Maybe [TransitGatewayRouteAttachment]) (\s a -> s {transitGatewayAttachments = a} :: TransitGatewayRoute)
{-# DEPRECATED tgrTransitGatewayAttachments "Use generic-lens or generic-optics with 'transitGatewayAttachments' instead." #-}

-- | The route type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrType :: Lens.Lens' TransitGatewayRoute (Lude.Maybe TransitGatewayRouteType)
tgrType = Lens.lens (type' :: TransitGatewayRoute -> Lude.Maybe TransitGatewayRouteType) (\s a -> s {type' = a} :: TransitGatewayRoute)
{-# DEPRECATED tgrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The CIDR block used for destination matches.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrDestinationCidrBlock :: Lens.Lens' TransitGatewayRoute (Lude.Maybe Lude.Text)
tgrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: TransitGatewayRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationCidrBlock = a} :: TransitGatewayRoute)
{-# DEPRECATED tgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.FromXML TransitGatewayRoute where
  parseXML x =
    TransitGatewayRoute'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "prefixListId")
      Lude.<*> ( x Lude..@? "transitGatewayAttachments" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "type")
      Lude.<*> (x Lude..@? "destinationCidrBlock")
