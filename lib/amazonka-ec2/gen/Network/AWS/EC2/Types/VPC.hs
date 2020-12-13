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
    vfState,
    vfIPv6CidrBlockAssociationSet,
    vfVPCId,
    vfCidrBlockAssociationSet,
    vfOwnerId,
    vfDHCPOptionsId,
    vfCidrBlock,
    vfInstanceTenancy,
    vfTags,
    vfIsDefault,
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
  { -- | The current state of the VPC.
    state :: VPCState,
    -- | Information about the IPv6 CIDR blocks associated with the VPC.
    ipv6CidrBlockAssociationSet :: Lude.Maybe [VPCIPv6CidrBlockAssociation],
    -- | The ID of the VPC.
    vpcId :: Lude.Text,
    -- | Information about the IPv4 CIDR blocks associated with the VPC.
    cidrBlockAssociationSet :: Lude.Maybe [VPCCidrBlockAssociation],
    -- | The ID of the AWS account that owns the VPC.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The ID of the set of DHCP options you've associated with the VPC.
    dhcpOptionsId :: Lude.Text,
    -- | The primary IPv4 CIDR block for the VPC.
    cidrBlock :: Lude.Text,
    -- | The allowed tenancy of instances launched into the VPC.
    instanceTenancy :: Tenancy,
    -- | Any tags assigned to the VPC.
    tags :: Lude.Maybe [Tag],
    -- | Indicates whether the VPC is the default VPC.
    isDefault :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPC' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the VPC.
-- * 'ipv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the VPC.
-- * 'vpcId' - The ID of the VPC.
-- * 'cidrBlockAssociationSet' - Information about the IPv4 CIDR blocks associated with the VPC.
-- * 'ownerId' - The ID of the AWS account that owns the VPC.
-- * 'dhcpOptionsId' - The ID of the set of DHCP options you've associated with the VPC.
-- * 'cidrBlock' - The primary IPv4 CIDR block for the VPC.
-- * 'instanceTenancy' - The allowed tenancy of instances launched into the VPC.
-- * 'tags' - Any tags assigned to the VPC.
-- * 'isDefault' - Indicates whether the VPC is the default VPC.
mkVPC ::
  -- | 'state'
  VPCState ->
  -- | 'vpcId'
  Lude.Text ->
  -- | 'dhcpOptionsId'
  Lude.Text ->
  -- | 'cidrBlock'
  Lude.Text ->
  -- | 'instanceTenancy'
  Tenancy ->
  VPC
mkVPC pState_ pVPCId_ pDHCPOptionsId_ pCidrBlock_ pInstanceTenancy_ =
  VPC'
    { state = pState_,
      ipv6CidrBlockAssociationSet = Lude.Nothing,
      vpcId = pVPCId_,
      cidrBlockAssociationSet = Lude.Nothing,
      ownerId = Lude.Nothing,
      dhcpOptionsId = pDHCPOptionsId_,
      cidrBlock = pCidrBlock_,
      instanceTenancy = pInstanceTenancy_,
      tags = Lude.Nothing,
      isDefault = Lude.Nothing
    }

-- | The current state of the VPC.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfState :: Lens.Lens' VPC VPCState
vfState = Lens.lens (state :: VPC -> VPCState) (\s a -> s {state = a} :: VPC)
{-# DEPRECATED vfState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Information about the IPv6 CIDR blocks associated with the VPC.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfIPv6CidrBlockAssociationSet :: Lens.Lens' VPC (Lude.Maybe [VPCIPv6CidrBlockAssociation])
vfIPv6CidrBlockAssociationSet = Lens.lens (ipv6CidrBlockAssociationSet :: VPC -> Lude.Maybe [VPCIPv6CidrBlockAssociation]) (\s a -> s {ipv6CidrBlockAssociationSet = a} :: VPC)
{-# DEPRECATED vfIPv6CidrBlockAssociationSet "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociationSet' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfVPCId :: Lens.Lens' VPC Lude.Text
vfVPCId = Lens.lens (vpcId :: VPC -> Lude.Text) (\s a -> s {vpcId = a} :: VPC)
{-# DEPRECATED vfVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Information about the IPv4 CIDR blocks associated with the VPC.
--
-- /Note:/ Consider using 'cidrBlockAssociationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfCidrBlockAssociationSet :: Lens.Lens' VPC (Lude.Maybe [VPCCidrBlockAssociation])
vfCidrBlockAssociationSet = Lens.lens (cidrBlockAssociationSet :: VPC -> Lude.Maybe [VPCCidrBlockAssociation]) (\s a -> s {cidrBlockAssociationSet = a} :: VPC)
{-# DEPRECATED vfCidrBlockAssociationSet "Use generic-lens or generic-optics with 'cidrBlockAssociationSet' instead." #-}

-- | The ID of the AWS account that owns the VPC.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfOwnerId :: Lens.Lens' VPC (Lude.Maybe Lude.Text)
vfOwnerId = Lens.lens (ownerId :: VPC -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: VPC)
{-# DEPRECATED vfOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the set of DHCP options you've associated with the VPC.
--
-- /Note:/ Consider using 'dhcpOptionsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfDHCPOptionsId :: Lens.Lens' VPC Lude.Text
vfDHCPOptionsId = Lens.lens (dhcpOptionsId :: VPC -> Lude.Text) (\s a -> s {dhcpOptionsId = a} :: VPC)
{-# DEPRECATED vfDHCPOptionsId "Use generic-lens or generic-optics with 'dhcpOptionsId' instead." #-}

-- | The primary IPv4 CIDR block for the VPC.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfCidrBlock :: Lens.Lens' VPC Lude.Text
vfCidrBlock = Lens.lens (cidrBlock :: VPC -> Lude.Text) (\s a -> s {cidrBlock = a} :: VPC)
{-# DEPRECATED vfCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | The allowed tenancy of instances launched into the VPC.
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfInstanceTenancy :: Lens.Lens' VPC Tenancy
vfInstanceTenancy = Lens.lens (instanceTenancy :: VPC -> Tenancy) (\s a -> s {instanceTenancy = a} :: VPC)
{-# DEPRECATED vfInstanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead." #-}

-- | Any tags assigned to the VPC.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfTags :: Lens.Lens' VPC (Lude.Maybe [Tag])
vfTags = Lens.lens (tags :: VPC -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: VPC)
{-# DEPRECATED vfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Indicates whether the VPC is the default VPC.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfIsDefault :: Lens.Lens' VPC (Lude.Maybe Lude.Bool)
vfIsDefault = Lens.lens (isDefault :: VPC -> Lude.Maybe Lude.Bool) (\s a -> s {isDefault = a} :: VPC)
{-# DEPRECATED vfIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

instance Lude.FromXML VPC where
  parseXML x =
    VPC'
      Lude.<$> (x Lude..@ "state")
      Lude.<*> ( x Lude..@? "ipv6CidrBlockAssociationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "vpcId")
      Lude.<*> ( x Lude..@? "cidrBlockAssociationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@ "dhcpOptionsId")
      Lude.<*> (x Lude..@ "cidrBlock")
      Lude.<*> (x Lude..@ "instanceTenancy")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "isDefault")
