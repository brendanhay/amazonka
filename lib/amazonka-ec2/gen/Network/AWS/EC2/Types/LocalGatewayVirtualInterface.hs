-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayVirtualInterface
  ( LocalGatewayVirtualInterface (..),

    -- * Smart constructor
    mkLocalGatewayVirtualInterface,

    -- * Lenses
    lgviLocalGatewayVirtualInterfaceId,
    lgviLocalBGPASN,
    lgviVLAN,
    lgviLocalGatewayId,
    lgviLocalAddress,
    lgviPeerBGPASN,
    lgviOwnerId,
    lgviPeerAddress,
    lgviTags,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a local gateway virtual interface.
--
-- /See:/ 'mkLocalGatewayVirtualInterface' smart constructor.
data LocalGatewayVirtualInterface = LocalGatewayVirtualInterface'
  { localGatewayVirtualInterfaceId ::
      Lude.Maybe Lude.Text,
    localBGPASN ::
      Lude.Maybe Lude.Int,
    vlan :: Lude.Maybe Lude.Int,
    localGatewayId ::
      Lude.Maybe Lude.Text,
    localAddress ::
      Lude.Maybe Lude.Text,
    peerBGPASN :: Lude.Maybe Lude.Int,
    ownerId :: Lude.Maybe Lude.Text,
    peerAddress ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'LocalGatewayVirtualInterface' with the minimum fields required to make a request.
--
-- * 'localAddress' - The local address.
-- * 'localBGPASN' - The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the local gateway.
-- * 'localGatewayId' - The ID of the local gateway.
-- * 'localGatewayVirtualInterfaceId' - The ID of the virtual interface.
-- * 'ownerId' - The AWS account ID that owns the local gateway virtual interface.
-- * 'peerAddress' - The peer address.
-- * 'peerBGPASN' - The peer BGP ASN.
-- * 'tags' - The tags assigned to the virtual interface.
-- * 'vlan' - The ID of the VLAN.
mkLocalGatewayVirtualInterface ::
  LocalGatewayVirtualInterface
mkLocalGatewayVirtualInterface =
  LocalGatewayVirtualInterface'
    { localGatewayVirtualInterfaceId =
        Lude.Nothing,
      localBGPASN = Lude.Nothing,
      vlan = Lude.Nothing,
      localGatewayId = Lude.Nothing,
      localAddress = Lude.Nothing,
      peerBGPASN = Lude.Nothing,
      ownerId = Lude.Nothing,
      peerAddress = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviLocalGatewayVirtualInterfaceId :: Lens.Lens' LocalGatewayVirtualInterface (Lude.Maybe Lude.Text)
lgviLocalGatewayVirtualInterfaceId = Lens.lens (localGatewayVirtualInterfaceId :: LocalGatewayVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayVirtualInterfaceId = a} :: LocalGatewayVirtualInterface)
{-# DEPRECATED lgviLocalGatewayVirtualInterfaceId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceId' instead." #-}

-- | The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the local gateway.
--
-- /Note:/ Consider using 'localBGPASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviLocalBGPASN :: Lens.Lens' LocalGatewayVirtualInterface (Lude.Maybe Lude.Int)
lgviLocalBGPASN = Lens.lens (localBGPASN :: LocalGatewayVirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {localBGPASN = a} :: LocalGatewayVirtualInterface)
{-# DEPRECATED lgviLocalBGPASN "Use generic-lens or generic-optics with 'localBGPASN' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviVLAN :: Lens.Lens' LocalGatewayVirtualInterface (Lude.Maybe Lude.Int)
lgviVLAN = Lens.lens (vlan :: LocalGatewayVirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {vlan = a} :: LocalGatewayVirtualInterface)
{-# DEPRECATED lgviVLAN "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviLocalGatewayId :: Lens.Lens' LocalGatewayVirtualInterface (Lude.Maybe Lude.Text)
lgviLocalGatewayId = Lens.lens (localGatewayId :: LocalGatewayVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayId = a} :: LocalGatewayVirtualInterface)
{-# DEPRECATED lgviLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | The local address.
--
-- /Note:/ Consider using 'localAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviLocalAddress :: Lens.Lens' LocalGatewayVirtualInterface (Lude.Maybe Lude.Text)
lgviLocalAddress = Lens.lens (localAddress :: LocalGatewayVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {localAddress = a} :: LocalGatewayVirtualInterface)
{-# DEPRECATED lgviLocalAddress "Use generic-lens or generic-optics with 'localAddress' instead." #-}

-- | The peer BGP ASN.
--
-- /Note:/ Consider using 'peerBGPASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviPeerBGPASN :: Lens.Lens' LocalGatewayVirtualInterface (Lude.Maybe Lude.Int)
lgviPeerBGPASN = Lens.lens (peerBGPASN :: LocalGatewayVirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {peerBGPASN = a} :: LocalGatewayVirtualInterface)
{-# DEPRECATED lgviPeerBGPASN "Use generic-lens or generic-optics with 'peerBGPASN' instead." #-}

-- | The AWS account ID that owns the local gateway virtual interface.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviOwnerId :: Lens.Lens' LocalGatewayVirtualInterface (Lude.Maybe Lude.Text)
lgviOwnerId = Lens.lens (ownerId :: LocalGatewayVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: LocalGatewayVirtualInterface)
{-# DEPRECATED lgviOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The peer address.
--
-- /Note:/ Consider using 'peerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviPeerAddress :: Lens.Lens' LocalGatewayVirtualInterface (Lude.Maybe Lude.Text)
lgviPeerAddress = Lens.lens (peerAddress :: LocalGatewayVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {peerAddress = a} :: LocalGatewayVirtualInterface)
{-# DEPRECATED lgviPeerAddress "Use generic-lens or generic-optics with 'peerAddress' instead." #-}

-- | The tags assigned to the virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgviTags :: Lens.Lens' LocalGatewayVirtualInterface (Lude.Maybe [Tag])
lgviTags = Lens.lens (tags :: LocalGatewayVirtualInterface -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LocalGatewayVirtualInterface)
{-# DEPRECATED lgviTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML LocalGatewayVirtualInterface where
  parseXML x =
    LocalGatewayVirtualInterface'
      Lude.<$> (x Lude..@? "localGatewayVirtualInterfaceId")
      Lude.<*> (x Lude..@? "localBgpAsn")
      Lude.<*> (x Lude..@? "vlan")
      Lude.<*> (x Lude..@? "localGatewayId")
      Lude.<*> (x Lude..@? "localAddress")
      Lude.<*> (x Lude..@? "peerBgpAsn")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "peerAddress")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
