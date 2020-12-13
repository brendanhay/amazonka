{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomain
  ( TransitGatewayMulticastDomain (..),

    -- * Smart constructor
    mkTransitGatewayMulticastDomain,

    -- * Lenses
    tgmdCreationTime,
    tgmdState,
    tgmdTransitGatewayMulticastDomainId,
    tgmdTransitGatewayId,
    tgmdTags,
  )
where

import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the transit gateway multicast domain.
--
-- /See:/ 'mkTransitGatewayMulticastDomain' smart constructor.
data TransitGatewayMulticastDomain = TransitGatewayMulticastDomain'
  { -- | The time the transit gateway multicast domain was created.
    creationTime :: Lude.Maybe Lude.DateTime,
    -- | The state of the transit gateway multicast domain.
    state :: Lude.Maybe TransitGatewayMulticastDomainState,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Lude.Maybe Lude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Lude.Maybe Lude.Text,
    -- | The tags for the transit gateway multicast domain.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time the transit gateway multicast domain was created.
-- * 'state' - The state of the transit gateway multicast domain.
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
-- * 'transitGatewayId' - The ID of the transit gateway.
-- * 'tags' - The tags for the transit gateway multicast domain.
mkTransitGatewayMulticastDomain ::
  TransitGatewayMulticastDomain
mkTransitGatewayMulticastDomain =
  TransitGatewayMulticastDomain'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      transitGatewayId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The time the transit gateway multicast domain was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdCreationTime :: Lens.Lens' TransitGatewayMulticastDomain (Lude.Maybe Lude.DateTime)
tgmdCreationTime = Lens.lens (creationTime :: TransitGatewayMulticastDomain -> Lude.Maybe Lude.DateTime) (\s a -> s {creationTime = a} :: TransitGatewayMulticastDomain)
{-# DEPRECATED tgmdCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdState :: Lens.Lens' TransitGatewayMulticastDomain (Lude.Maybe TransitGatewayMulticastDomainState)
tgmdState = Lens.lens (state :: TransitGatewayMulticastDomain -> Lude.Maybe TransitGatewayMulticastDomainState) (\s a -> s {state = a} :: TransitGatewayMulticastDomain)
{-# DEPRECATED tgmdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDomain (Lude.Maybe Lude.Text)
tgmdTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: TransitGatewayMulticastDomain -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDomain)
{-# DEPRECATED tgmdTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdTransitGatewayId :: Lens.Lens' TransitGatewayMulticastDomain (Lude.Maybe Lude.Text)
tgmdTransitGatewayId = Lens.lens (transitGatewayId :: TransitGatewayMulticastDomain -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: TransitGatewayMulticastDomain)
{-# DEPRECATED tgmdTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The tags for the transit gateway multicast domain.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdTags :: Lens.Lens' TransitGatewayMulticastDomain (Lude.Maybe [Tag])
tgmdTags = Lens.lens (tags :: TransitGatewayMulticastDomain -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TransitGatewayMulticastDomain)
{-# DEPRECATED tgmdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML TransitGatewayMulticastDomain where
  parseXML x =
    TransitGatewayMulticastDomain'
      Lude.<$> (x Lude..@? "creationTime")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "transitGatewayMulticastDomainId")
      Lude.<*> (x Lude..@? "transitGatewayId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
