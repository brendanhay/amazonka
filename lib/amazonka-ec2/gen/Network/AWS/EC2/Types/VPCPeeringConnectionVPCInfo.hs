{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnectionVPCInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCPeeringConnectionVPCInfo
  ( VPCPeeringConnectionVPCInfo (..),

    -- * Smart constructor
    mkVPCPeeringConnectionVPCInfo,

    -- * Lenses
    vpcviCidrBlockSet,
    vpcviVPCId,
    vpcviOwnerId,
    vpcviPeeringOptions,
    vpcviCidrBlock,
    vpcviRegion,
    vpcviIPv6CidrBlockSet,
  )
where

import Network.AWS.EC2.Types.CidrBlock
import Network.AWS.EC2.Types.IPv6CidrBlock
import Network.AWS.EC2.Types.VPCPeeringConnectionOptionsDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a VPC in a VPC peering connection.
--
-- /See:/ 'mkVPCPeeringConnectionVPCInfo' smart constructor.
data VPCPeeringConnectionVPCInfo = VPCPeeringConnectionVPCInfo'
  { cidrBlockSet ::
      Lude.Maybe [CidrBlock],
    vpcId :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Text,
    peeringOptions ::
      Lude.Maybe
        VPCPeeringConnectionOptionsDescription,
    cidrBlock :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    ipv6CidrBlockSet ::
      Lude.Maybe [IPv6CidrBlock]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCPeeringConnectionVPCInfo' with the minimum fields required to make a request.
--
-- * 'cidrBlock' - The IPv4 CIDR block for the VPC.
-- * 'cidrBlockSet' - Information about the IPv4 CIDR blocks for the VPC.
-- * 'ipv6CidrBlockSet' - The IPv6 CIDR block for the VPC.
-- * 'ownerId' - The AWS account ID of the VPC owner.
-- * 'peeringOptions' - Information about the VPC peering connection options for the accepter or requester VPC.
-- * 'region' - The Region in which the VPC is located.
-- * 'vpcId' - The ID of the VPC.
mkVPCPeeringConnectionVPCInfo ::
  VPCPeeringConnectionVPCInfo
mkVPCPeeringConnectionVPCInfo =
  VPCPeeringConnectionVPCInfo'
    { cidrBlockSet = Lude.Nothing,
      vpcId = Lude.Nothing,
      ownerId = Lude.Nothing,
      peeringOptions = Lude.Nothing,
      cidrBlock = Lude.Nothing,
      region = Lude.Nothing,
      ipv6CidrBlockSet = Lude.Nothing
    }

-- | Information about the IPv4 CIDR blocks for the VPC.
--
-- /Note:/ Consider using 'cidrBlockSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviCidrBlockSet :: Lens.Lens' VPCPeeringConnectionVPCInfo (Lude.Maybe [CidrBlock])
vpcviCidrBlockSet = Lens.lens (cidrBlockSet :: VPCPeeringConnectionVPCInfo -> Lude.Maybe [CidrBlock]) (\s a -> s {cidrBlockSet = a} :: VPCPeeringConnectionVPCInfo)
{-# DEPRECATED vpcviCidrBlockSet "Use generic-lens or generic-optics with 'cidrBlockSet' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviVPCId :: Lens.Lens' VPCPeeringConnectionVPCInfo (Lude.Maybe Lude.Text)
vpcviVPCId = Lens.lens (vpcId :: VPCPeeringConnectionVPCInfo -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: VPCPeeringConnectionVPCInfo)
{-# DEPRECATED vpcviVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The AWS account ID of the VPC owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviOwnerId :: Lens.Lens' VPCPeeringConnectionVPCInfo (Lude.Maybe Lude.Text)
vpcviOwnerId = Lens.lens (ownerId :: VPCPeeringConnectionVPCInfo -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: VPCPeeringConnectionVPCInfo)
{-# DEPRECATED vpcviOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | Information about the VPC peering connection options for the accepter or requester VPC.
--
-- /Note:/ Consider using 'peeringOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviPeeringOptions :: Lens.Lens' VPCPeeringConnectionVPCInfo (Lude.Maybe VPCPeeringConnectionOptionsDescription)
vpcviPeeringOptions = Lens.lens (peeringOptions :: VPCPeeringConnectionVPCInfo -> Lude.Maybe VPCPeeringConnectionOptionsDescription) (\s a -> s {peeringOptions = a} :: VPCPeeringConnectionVPCInfo)
{-# DEPRECATED vpcviPeeringOptions "Use generic-lens or generic-optics with 'peeringOptions' instead." #-}

-- | The IPv4 CIDR block for the VPC.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviCidrBlock :: Lens.Lens' VPCPeeringConnectionVPCInfo (Lude.Maybe Lude.Text)
vpcviCidrBlock = Lens.lens (cidrBlock :: VPCPeeringConnectionVPCInfo -> Lude.Maybe Lude.Text) (\s a -> s {cidrBlock = a} :: VPCPeeringConnectionVPCInfo)
{-# DEPRECATED vpcviCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | The Region in which the VPC is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviRegion :: Lens.Lens' VPCPeeringConnectionVPCInfo (Lude.Maybe Lude.Text)
vpcviRegion = Lens.lens (region :: VPCPeeringConnectionVPCInfo -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: VPCPeeringConnectionVPCInfo)
{-# DEPRECATED vpcviRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The IPv6 CIDR block for the VPC.
--
-- /Note:/ Consider using 'ipv6CidrBlockSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviIPv6CidrBlockSet :: Lens.Lens' VPCPeeringConnectionVPCInfo (Lude.Maybe [IPv6CidrBlock])
vpcviIPv6CidrBlockSet = Lens.lens (ipv6CidrBlockSet :: VPCPeeringConnectionVPCInfo -> Lude.Maybe [IPv6CidrBlock]) (\s a -> s {ipv6CidrBlockSet = a} :: VPCPeeringConnectionVPCInfo)
{-# DEPRECATED vpcviIPv6CidrBlockSet "Use generic-lens or generic-optics with 'ipv6CidrBlockSet' instead." #-}

instance Lude.FromXML VPCPeeringConnectionVPCInfo where
  parseXML x =
    VPCPeeringConnectionVPCInfo'
      Lude.<$> ( x Lude..@? "cidrBlockSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "peeringOptions")
      Lude.<*> (x Lude..@? "cidrBlock")
      Lude.<*> (x Lude..@? "region")
      Lude.<*> ( x Lude..@? "ipv6CidrBlockSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
