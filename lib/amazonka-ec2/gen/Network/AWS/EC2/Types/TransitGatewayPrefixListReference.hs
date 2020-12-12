{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPrefixListReference
  ( TransitGatewayPrefixListReference (..),

    -- * Smart constructor
    mkTransitGatewayPrefixListReference,

    -- * Lenses
    tgplrState,
    tgplrTransitGatewayRouteTableId,
    tgplrPrefixListOwnerId,
    tgplrBlackhole,
    tgplrPrefixListId,
    tgplrTransitGatewayAttachment,
  )
where

import Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
import Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a prefix list reference.
--
-- /See:/ 'mkTransitGatewayPrefixListReference' smart constructor.
data TransitGatewayPrefixListReference = TransitGatewayPrefixListReference'
  { state ::
      Lude.Maybe
        TransitGatewayPrefixListReferenceState,
    transitGatewayRouteTableId ::
      Lude.Maybe Lude.Text,
    prefixListOwnerId ::
      Lude.Maybe Lude.Text,
    blackhole ::
      Lude.Maybe Lude.Bool,
    prefixListId ::
      Lude.Maybe Lude.Text,
    transitGatewayAttachment ::
      Lude.Maybe
        TransitGatewayPrefixListAttachment
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayPrefixListReference' with the minimum fields required to make a request.
--
-- * 'blackhole' - Indicates whether traffic that matches this route is dropped.
-- * 'prefixListId' - The ID of the prefix list.
-- * 'prefixListOwnerId' - The ID of the prefix list owner.
-- * 'state' - The state of the prefix list reference.
-- * 'transitGatewayAttachment' - Information about the transit gateway attachment.
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
mkTransitGatewayPrefixListReference ::
  TransitGatewayPrefixListReference
mkTransitGatewayPrefixListReference =
  TransitGatewayPrefixListReference'
    { state = Lude.Nothing,
      transitGatewayRouteTableId = Lude.Nothing,
      prefixListOwnerId = Lude.Nothing,
      blackhole = Lude.Nothing,
      prefixListId = Lude.Nothing,
      transitGatewayAttachment = Lude.Nothing
    }

-- | The state of the prefix list reference.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrState :: Lens.Lens' TransitGatewayPrefixListReference (Lude.Maybe TransitGatewayPrefixListReferenceState)
tgplrState = Lens.lens (state :: TransitGatewayPrefixListReference -> Lude.Maybe TransitGatewayPrefixListReferenceState) (\s a -> s {state = a} :: TransitGatewayPrefixListReference)
{-# DEPRECATED tgplrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayPrefixListReference (Lude.Maybe Lude.Text)
tgplrTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: TransitGatewayPrefixListReference -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: TransitGatewayPrefixListReference)
{-# DEPRECATED tgplrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | The ID of the prefix list owner.
--
-- /Note:/ Consider using 'prefixListOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrPrefixListOwnerId :: Lens.Lens' TransitGatewayPrefixListReference (Lude.Maybe Lude.Text)
tgplrPrefixListOwnerId = Lens.lens (prefixListOwnerId :: TransitGatewayPrefixListReference -> Lude.Maybe Lude.Text) (\s a -> s {prefixListOwnerId = a} :: TransitGatewayPrefixListReference)
{-# DEPRECATED tgplrPrefixListOwnerId "Use generic-lens or generic-optics with 'prefixListOwnerId' instead." #-}

-- | Indicates whether traffic that matches this route is dropped.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrBlackhole :: Lens.Lens' TransitGatewayPrefixListReference (Lude.Maybe Lude.Bool)
tgplrBlackhole = Lens.lens (blackhole :: TransitGatewayPrefixListReference -> Lude.Maybe Lude.Bool) (\s a -> s {blackhole = a} :: TransitGatewayPrefixListReference)
{-# DEPRECATED tgplrBlackhole "Use generic-lens or generic-optics with 'blackhole' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrPrefixListId :: Lens.Lens' TransitGatewayPrefixListReference (Lude.Maybe Lude.Text)
tgplrPrefixListId = Lens.lens (prefixListId :: TransitGatewayPrefixListReference -> Lude.Maybe Lude.Text) (\s a -> s {prefixListId = a} :: TransitGatewayPrefixListReference)
{-# DEPRECATED tgplrPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | Information about the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrTransitGatewayAttachment :: Lens.Lens' TransitGatewayPrefixListReference (Lude.Maybe TransitGatewayPrefixListAttachment)
tgplrTransitGatewayAttachment = Lens.lens (transitGatewayAttachment :: TransitGatewayPrefixListReference -> Lude.Maybe TransitGatewayPrefixListAttachment) (\s a -> s {transitGatewayAttachment = a} :: TransitGatewayPrefixListReference)
{-# DEPRECATED tgplrTransitGatewayAttachment "Use generic-lens or generic-optics with 'transitGatewayAttachment' instead." #-}

instance Lude.FromXML TransitGatewayPrefixListReference where
  parseXML x =
    TransitGatewayPrefixListReference'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "transitGatewayRouteTableId")
      Lude.<*> (x Lude..@? "prefixListOwnerId")
      Lude.<*> (x Lude..@? "blackhole")
      Lude.<*> (x Lude..@? "prefixListId")
      Lude.<*> (x Lude..@? "transitGatewayAttachment")
