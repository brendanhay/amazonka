{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPC
  ( VPC (..),

    -- * Smart constructor
    mkVPC,

    -- * Lenses
    vpcIPv6CidrBlockAssociationSet,
    vpcCidrBlockAssociationSet,
    vpcOwnerId,
    vpcTags,
    vpcIsDefault,
    vpcCidrBlock,
    vpcDHCPOptionsId,
    vpcInstanceTenancy,
    vpcState,
    vpcVPCId,
  )
where

import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.EC2.Types.VPCCidrBlockAssociation
import Network.AWS.EC2.Types.VPCIPv6CidrBlockAssociation
import Network.AWS.EC2.Types.VPCState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a VPC.
--
-- /See:/ 'mkVPC' smart constructor.
data VPC = VPC'
  { ipv6CidrBlockAssociationSet ::
      Lude.Maybe [VPCIPv6CidrBlockAssociation],
    cidrBlockAssociationSet :: Lude.Maybe [VPCCidrBlockAssociation],
    ownerId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    isDefault :: Lude.Maybe Lude.Bool,
    cidrBlock :: Lude.Text,
    dhcpOptionsId :: Lude.Text,
    instanceTenancy :: Tenancy,
    state :: VPCState,
    vpcId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPC' with the minimum fields required to make a request.
--
-- * 'cidrBlock' - The primary IPv4 CIDR block for the VPC.
-- * 'cidrBlockAssociationSet' - Information about the IPv4 CIDR blocks associated with the VPC.
-- * 'dhcpOptionsId' - The ID of the set of DHCP options you've associated with the VPC.
-- * 'instanceTenancy' - The allowed tenancy of instances launched into the VPC.
-- * 'ipv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the VPC.
-- * 'isDefault' - Indicates whether the VPC is the default VPC.
-- * 'ownerId' - The ID of the AWS account that owns the VPC.
-- * 'state' - The current state of the VPC.
-- * 'tags' - Any tags assigned to the VPC.
-- * 'vpcId' - The ID of the VPC.
mkVPC ::
  -- | 'cidrBlock'
  Lude.Text ->
  -- | 'dhcpOptionsId'
  Lude.Text ->
  -- | 'instanceTenancy'
  Tenancy ->
  -- | 'state'
  VPCState ->
  -- | 'vpcId'
  Lude.Text ->
  VPC
mkVPC pCidrBlock_ pDHCPOptionsId_ pInstanceTenancy_ pState_ pVPCId_ =
  VPC'
    { ipv6CidrBlockAssociationSet = Lude.Nothing,
      cidrBlockAssociationSet = Lude.Nothing,
      ownerId = Lude.Nothing,
      tags = Lude.Nothing,
      isDefault = Lude.Nothing,
      cidrBlock = pCidrBlock_,
      dhcpOptionsId = pDHCPOptionsId_,
      instanceTenancy = pInstanceTenancy_,
      state = pState_,
      vpcId = pVPCId_
    }

-- | Information about the IPv6 CIDR blocks associated with the VPC.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcIPv6CidrBlockAssociationSet :: Lens.Lens' VPC (Lude.Maybe [VPCIPv6CidrBlockAssociation])
vpcIPv6CidrBlockAssociationSet = Lens.lens (ipv6CidrBlockAssociationSet :: VPC -> Lude.Maybe [VPCIPv6CidrBlockAssociation]) (\s a -> s {ipv6CidrBlockAssociationSet = a} :: VPC)
{-# DEPRECATED vpcIPv6CidrBlockAssociationSet "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociationSet' instead." #-}

-- | Information about the IPv4 CIDR blocks associated with the VPC.
--
-- /Note:/ Consider using 'cidrBlockAssociationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcCidrBlockAssociationSet :: Lens.Lens' VPC (Lude.Maybe [VPCCidrBlockAssociation])
vpcCidrBlockAssociationSet = Lens.lens (cidrBlockAssociationSet :: VPC -> Lude.Maybe [VPCCidrBlockAssociation]) (\s a -> s {cidrBlockAssociationSet = a} :: VPC)
{-# DEPRECATED vpcCidrBlockAssociationSet "Use generic-lens or generic-optics with 'cidrBlockAssociationSet' instead." #-}

-- | The ID of the AWS account that owns the VPC.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcOwnerId :: Lens.Lens' VPC (Lude.Maybe Lude.Text)
vpcOwnerId = Lens.lens (ownerId :: VPC -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: VPC)
{-# DEPRECATED vpcOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | Any tags assigned to the VPC.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcTags :: Lens.Lens' VPC (Lude.Maybe [Tag])
vpcTags = Lens.lens (tags :: VPC -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: VPC)
{-# DEPRECATED vpcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Indicates whether the VPC is the default VPC.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcIsDefault :: Lens.Lens' VPC (Lude.Maybe Lude.Bool)
vpcIsDefault = Lens.lens (isDefault :: VPC -> Lude.Maybe Lude.Bool) (\s a -> s {isDefault = a} :: VPC)
{-# DEPRECATED vpcIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

-- | The primary IPv4 CIDR block for the VPC.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcCidrBlock :: Lens.Lens' VPC Lude.Text
vpcCidrBlock = Lens.lens (cidrBlock :: VPC -> Lude.Text) (\s a -> s {cidrBlock = a} :: VPC)
{-# DEPRECATED vpcCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | The ID of the set of DHCP options you've associated with the VPC.
--
-- /Note:/ Consider using 'dhcpOptionsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcDHCPOptionsId :: Lens.Lens' VPC Lude.Text
vpcDHCPOptionsId = Lens.lens (dhcpOptionsId :: VPC -> Lude.Text) (\s a -> s {dhcpOptionsId = a} :: VPC)
{-# DEPRECATED vpcDHCPOptionsId "Use generic-lens or generic-optics with 'dhcpOptionsId' instead." #-}

-- | The allowed tenancy of instances launched into the VPC.
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcInstanceTenancy :: Lens.Lens' VPC Tenancy
vpcInstanceTenancy = Lens.lens (instanceTenancy :: VPC -> Tenancy) (\s a -> s {instanceTenancy = a} :: VPC)
{-# DEPRECATED vpcInstanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead." #-}

-- | The current state of the VPC.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcState :: Lens.Lens' VPC VPCState
vpcState = Lens.lens (state :: VPC -> VPCState) (\s a -> s {state = a} :: VPC)
{-# DEPRECATED vpcState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcVPCId :: Lens.Lens' VPC Lude.Text
vpcVPCId = Lens.lens (vpcId :: VPC -> Lude.Text) (\s a -> s {vpcId = a} :: VPC)
{-# DEPRECATED vpcVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.FromXML VPC where
  parseXML x =
    VPC'
      Lude.<$> ( x Lude..@? "ipv6CidrBlockAssociationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "cidrBlockAssociationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "isDefault")
      Lude.<*> (x Lude..@ "cidrBlock")
      Lude.<*> (x Lude..@ "dhcpOptionsId")
      Lude.<*> (x Lude..@ "instanceTenancy")
      Lude.<*> (x Lude..@ "state")
      Lude.<*> (x Lude..@ "vpcId")
