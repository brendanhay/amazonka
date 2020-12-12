{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
  ( TransitGatewayMulticastRegisteredGroupSources (..),

    -- * Smart constructor
    mkTransitGatewayMulticastRegisteredGroupSources,

    -- * Lenses
    tgmrgsTransitGatewayMulticastDomainId,
    tgmrgsRegisteredNetworkInterfaceIds,
    tgmrgsGroupIPAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the members registered with the transit gateway multicast group.
--
-- /See:/ 'mkTransitGatewayMulticastRegisteredGroupSources' smart constructor.
data TransitGatewayMulticastRegisteredGroupSources = TransitGatewayMulticastRegisteredGroupSources'
  { transitGatewayMulticastDomainId ::
      Lude.Maybe
        Lude.Text,
    registeredNetworkInterfaceIds ::
      Lude.Maybe
        [Lude.Text],
    groupIPAddress ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'TransitGatewayMulticastRegisteredGroupSources' with the minimum fields required to make a request.
--
-- * 'groupIPAddress' - The IP address assigned to the transit gateway multicast group.
-- * 'registeredNetworkInterfaceIds' - The IDs of the network interfaces members registered with the transit gateway multicast group.
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
mkTransitGatewayMulticastRegisteredGroupSources ::
  TransitGatewayMulticastRegisteredGroupSources
mkTransitGatewayMulticastRegisteredGroupSources =
  TransitGatewayMulticastRegisteredGroupSources'
    { transitGatewayMulticastDomainId =
        Lude.Nothing,
      registeredNetworkInterfaceIds = Lude.Nothing,
      groupIPAddress = Lude.Nothing
    }

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgsTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Lude.Maybe Lude.Text)
tgmrgsTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: TransitGatewayMulticastRegisteredGroupSources -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastRegisteredGroupSources)
{-# DEPRECATED tgmrgsTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The IDs of the network interfaces members registered with the transit gateway multicast group.
--
-- /Note:/ Consider using 'registeredNetworkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgsRegisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Lude.Maybe [Lude.Text])
tgmrgsRegisteredNetworkInterfaceIds = Lens.lens (registeredNetworkInterfaceIds :: TransitGatewayMulticastRegisteredGroupSources -> Lude.Maybe [Lude.Text]) (\s a -> s {registeredNetworkInterfaceIds = a} :: TransitGatewayMulticastRegisteredGroupSources)
{-# DEPRECATED tgmrgsRegisteredNetworkInterfaceIds "Use generic-lens or generic-optics with 'registeredNetworkInterfaceIds' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgsGroupIPAddress :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Lude.Maybe Lude.Text)
tgmrgsGroupIPAddress = Lens.lens (groupIPAddress :: TransitGatewayMulticastRegisteredGroupSources -> Lude.Maybe Lude.Text) (\s a -> s {groupIPAddress = a} :: TransitGatewayMulticastRegisteredGroupSources)
{-# DEPRECATED tgmrgsGroupIPAddress "Use generic-lens or generic-optics with 'groupIPAddress' instead." #-}

instance Lude.FromXML TransitGatewayMulticastRegisteredGroupSources where
  parseXML x =
    TransitGatewayMulticastRegisteredGroupSources'
      Lude.<$> (x Lude..@? "transitGatewayMulticastDomainId")
      Lude.<*> ( x Lude..@? "registeredNetworkInterfaceIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "groupIpAddress")
