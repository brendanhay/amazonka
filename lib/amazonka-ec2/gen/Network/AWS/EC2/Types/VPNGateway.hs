{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNGateway
  ( VPNGateway (..),

    -- * Smart constructor
    mkVPNGateway,

    -- * Lenses
    vgState,
    vgVPCAttachments,
    vgVPNGatewayId,
    vgAmazonSideASN,
    vgAvailabilityZone,
    vgType,
    vgTags,
  )
where

import Network.AWS.EC2.Types.GatewayType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VPCAttachment
import Network.AWS.EC2.Types.VPNState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a virtual private gateway.
--
-- /See:/ 'mkVPNGateway' smart constructor.
data VPNGateway = VPNGateway'
  { state :: Lude.Maybe VPNState,
    vpcAttachments :: Lude.Maybe [VPCAttachment],
    vpnGatewayId :: Lude.Maybe Lude.Text,
    amazonSideASN :: Lude.Maybe Lude.Integer,
    availabilityZone :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe GatewayType,
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

-- | Creates a value of 'VPNGateway' with the minimum fields required to make a request.
--
-- * 'amazonSideASN' - The private Autonomous System Number (ASN) for the Amazon side of a BGP session.
-- * 'availabilityZone' - The Availability Zone where the virtual private gateway was created, if applicable. This field may be empty or not returned.
-- * 'state' - The current state of the virtual private gateway.
-- * 'tags' - Any tags assigned to the virtual private gateway.
-- * 'type'' - The type of VPN connection the virtual private gateway supports.
-- * 'vpcAttachments' - Any VPCs attached to the virtual private gateway.
-- * 'vpnGatewayId' - The ID of the virtual private gateway.
mkVPNGateway ::
  VPNGateway
mkVPNGateway =
  VPNGateway'
    { state = Lude.Nothing,
      vpcAttachments = Lude.Nothing,
      vpnGatewayId = Lude.Nothing,
      amazonSideASN = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      type' = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The current state of the virtual private gateway.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgState :: Lens.Lens' VPNGateway (Lude.Maybe VPNState)
vgState = Lens.lens (state :: VPNGateway -> Lude.Maybe VPNState) (\s a -> s {state = a} :: VPNGateway)
{-# DEPRECATED vgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Any VPCs attached to the virtual private gateway.
--
-- /Note:/ Consider using 'vpcAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgVPCAttachments :: Lens.Lens' VPNGateway (Lude.Maybe [VPCAttachment])
vgVPCAttachments = Lens.lens (vpcAttachments :: VPNGateway -> Lude.Maybe [VPCAttachment]) (\s a -> s {vpcAttachments = a} :: VPNGateway)
{-# DEPRECATED vgVPCAttachments "Use generic-lens or generic-optics with 'vpcAttachments' instead." #-}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgVPNGatewayId :: Lens.Lens' VPNGateway (Lude.Maybe Lude.Text)
vgVPNGatewayId = Lens.lens (vpnGatewayId :: VPNGateway -> Lude.Maybe Lude.Text) (\s a -> s {vpnGatewayId = a} :: VPNGateway)
{-# DEPRECATED vgVPNGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead." #-}

-- | The private Autonomous System Number (ASN) for the Amazon side of a BGP session.
--
-- /Note:/ Consider using 'amazonSideASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgAmazonSideASN :: Lens.Lens' VPNGateway (Lude.Maybe Lude.Integer)
vgAmazonSideASN = Lens.lens (amazonSideASN :: VPNGateway -> Lude.Maybe Lude.Integer) (\s a -> s {amazonSideASN = a} :: VPNGateway)
{-# DEPRECATED vgAmazonSideASN "Use generic-lens or generic-optics with 'amazonSideASN' instead." #-}

-- | The Availability Zone where the virtual private gateway was created, if applicable. This field may be empty or not returned.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgAvailabilityZone :: Lens.Lens' VPNGateway (Lude.Maybe Lude.Text)
vgAvailabilityZone = Lens.lens (availabilityZone :: VPNGateway -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: VPNGateway)
{-# DEPRECATED vgAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The type of VPN connection the virtual private gateway supports.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgType :: Lens.Lens' VPNGateway (Lude.Maybe GatewayType)
vgType = Lens.lens (type' :: VPNGateway -> Lude.Maybe GatewayType) (\s a -> s {type' = a} :: VPNGateway)
{-# DEPRECATED vgType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Any tags assigned to the virtual private gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgTags :: Lens.Lens' VPNGateway (Lude.Maybe [Tag])
vgTags = Lens.lens (tags :: VPNGateway -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: VPNGateway)
{-# DEPRECATED vgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML VPNGateway where
  parseXML x =
    VPNGateway'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> ( x Lude..@? "attachments" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpnGatewayId")
      Lude.<*> (x Lude..@? "amazonSideAsn")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "type")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
