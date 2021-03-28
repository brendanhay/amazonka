{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LocalGatewayVirtualInterface
  ( LocalGatewayVirtualInterface (..)
  -- * Smart constructor
  , mkLocalGatewayVirtualInterface
  -- * Lenses
  , lgviLocalAddress
  , lgviLocalBgpAsn
  , lgviLocalGatewayId
  , lgviLocalGatewayVirtualInterfaceId
  , lgviOwnerId
  , lgviPeerAddress
  , lgviPeerBgpAsn
  , lgviTags
  , lgviVlan
  ) where

import qualified Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceId as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a local gateway virtual interface.
--
-- /See:/ 'mkLocalGatewayVirtualInterface' smart constructor.
data LocalGatewayVirtualInterface = LocalGatewayVirtualInterface'
  { localAddress :: Core.Maybe Core.Text
    -- ^ The local address.
  , localBgpAsn :: Core.Maybe Core.Int
    -- ^ The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the local gateway.
  , localGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the local gateway.
  , localGatewayVirtualInterfaceId :: Core.Maybe Types.LocalGatewayVirtualInterfaceId
    -- ^ The ID of the virtual interface.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID that owns the local gateway virtual interface.
  , peerAddress :: Core.Maybe Core.Text
    -- ^ The peer address.
  , peerBgpAsn :: Core.Maybe Core.Int
    -- ^ The peer BGP ASN.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags assigned to the virtual interface.
  , vlan :: Core.Maybe Core.Int
    -- ^ The ID of the VLAN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalGatewayVirtualInterface' value with any optional fields omitted.
mkLocalGatewayVirtualInterface
    :: LocalGatewayVirtualInterface
mkLocalGatewayVirtualInterface
  = LocalGatewayVirtualInterface'{localAddress = Core.Nothing,
                                  localBgpAsn = Core.Nothing, localGatewayId = Core.Nothing,
                                  localGatewayVirtualInterfaceId = Core.Nothing,
                                  ownerId = Core.Nothing, peerAddress = Core.Nothing,
                                  peerBgpAsn = Core.Nothing, tags = Core.Nothing,
                                  vlan = Core.Nothing}

-- | The local address.
--
-- /Note:/ Consider using 'localAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviLocalAddress :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Text)
lgviLocalAddress = Lens.field @"localAddress"
{-# INLINEABLE lgviLocalAddress #-}
{-# DEPRECATED localAddress "Use generic-lens or generic-optics with 'localAddress' instead"  #-}

-- | The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the local gateway.
--
-- /Note:/ Consider using 'localBgpAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviLocalBgpAsn :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Int)
lgviLocalBgpAsn = Lens.field @"localBgpAsn"
{-# INLINEABLE lgviLocalBgpAsn #-}
{-# DEPRECATED localBgpAsn "Use generic-lens or generic-optics with 'localBgpAsn' instead"  #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviLocalGatewayId :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Text)
lgviLocalGatewayId = Lens.field @"localGatewayId"
{-# INLINEABLE lgviLocalGatewayId #-}
{-# DEPRECATED localGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead"  #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviLocalGatewayVirtualInterfaceId :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Types.LocalGatewayVirtualInterfaceId)
lgviLocalGatewayVirtualInterfaceId = Lens.field @"localGatewayVirtualInterfaceId"
{-# INLINEABLE lgviLocalGatewayVirtualInterfaceId #-}
{-# DEPRECATED localGatewayVirtualInterfaceId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceId' instead"  #-}

-- | The AWS account ID that owns the local gateway virtual interface.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviOwnerId :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Text)
lgviOwnerId = Lens.field @"ownerId"
{-# INLINEABLE lgviOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The peer address.
--
-- /Note:/ Consider using 'peerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviPeerAddress :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Text)
lgviPeerAddress = Lens.field @"peerAddress"
{-# INLINEABLE lgviPeerAddress #-}
{-# DEPRECATED peerAddress "Use generic-lens or generic-optics with 'peerAddress' instead"  #-}

-- | The peer BGP ASN.
--
-- /Note:/ Consider using 'peerBgpAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviPeerBgpAsn :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Int)
lgviPeerBgpAsn = Lens.field @"peerBgpAsn"
{-# INLINEABLE lgviPeerBgpAsn #-}
{-# DEPRECATED peerBgpAsn "Use generic-lens or generic-optics with 'peerBgpAsn' instead"  #-}

-- | The tags assigned to the virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviTags :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe [Types.Tag])
lgviTags = Lens.field @"tags"
{-# INLINEABLE lgviTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviVlan :: Lens.Lens' LocalGatewayVirtualInterface (Core.Maybe Core.Int)
lgviVlan = Lens.field @"vlan"
{-# INLINEABLE lgviVlan #-}
{-# DEPRECATED vlan "Use generic-lens or generic-optics with 'vlan' instead"  #-}

instance Core.FromXML LocalGatewayVirtualInterface where
        parseXML x
          = LocalGatewayVirtualInterface' Core.<$>
              (x Core..@? "localAddress") Core.<*> x Core..@? "localBgpAsn"
                Core.<*> x Core..@? "localGatewayId"
                Core.<*> x Core..@? "localGatewayVirtualInterfaceId"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "peerAddress"
                Core.<*> x Core..@? "peerBgpAsn"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "vlan"
