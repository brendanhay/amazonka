{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCIPv6CidrBlockAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCIPv6CidrBlockAssociation
  ( VPCIPv6CidrBlockAssociation (..),

    -- * Smart constructor
    mkVPCIPv6CidrBlockAssociation,

    -- * Lenses
    vicbaAssociationId,
    vicbaIPv6CidrBlock,
    vicbaNetworkBorderGroup,
    vicbaIPv6CidrBlockState,
    vicbaIPv6Pool,
  )
where

import Network.AWS.EC2.Types.VPCCidrBlockState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv6 CIDR block associated with a VPC.
--
-- /See:/ 'mkVPCIPv6CidrBlockAssociation' smart constructor.
data VPCIPv6CidrBlockAssociation = VPCIPv6CidrBlockAssociation'
  { associationId ::
      Lude.Maybe Lude.Text,
    ipv6CidrBlock ::
      Lude.Maybe Lude.Text,
    networkBorderGroup ::
      Lude.Maybe Lude.Text,
    ipv6CidrBlockState ::
      Lude.Maybe VPCCidrBlockState,
    ipv6Pool :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCIPv6CidrBlockAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID for the IPv6 CIDR block.
-- * 'ipv6CidrBlock' - The IPv6 CIDR block.
-- * 'ipv6CidrBlockState' - Information about the state of the CIDR block.
-- * 'ipv6Pool' - The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.
-- * 'networkBorderGroup' - The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses, for example, @us-east-1-wl1-bos-wlz-1@ .
mkVPCIPv6CidrBlockAssociation ::
  VPCIPv6CidrBlockAssociation
mkVPCIPv6CidrBlockAssociation =
  VPCIPv6CidrBlockAssociation'
    { associationId = Lude.Nothing,
      ipv6CidrBlock = Lude.Nothing,
      networkBorderGroup = Lude.Nothing,
      ipv6CidrBlockState = Lude.Nothing,
      ipv6Pool = Lude.Nothing
    }

-- | The association ID for the IPv6 CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vicbaAssociationId :: Lens.Lens' VPCIPv6CidrBlockAssociation (Lude.Maybe Lude.Text)
vicbaAssociationId = Lens.lens (associationId :: VPCIPv6CidrBlockAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: VPCIPv6CidrBlockAssociation)
{-# DEPRECATED vicbaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vicbaIPv6CidrBlock :: Lens.Lens' VPCIPv6CidrBlockAssociation (Lude.Maybe Lude.Text)
vicbaIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: VPCIPv6CidrBlockAssociation -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: VPCIPv6CidrBlockAssociation)
{-# DEPRECATED vicbaIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

-- | The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses, for example, @us-east-1-wl1-bos-wlz-1@ .
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vicbaNetworkBorderGroup :: Lens.Lens' VPCIPv6CidrBlockAssociation (Lude.Maybe Lude.Text)
vicbaNetworkBorderGroup = Lens.lens (networkBorderGroup :: VPCIPv6CidrBlockAssociation -> Lude.Maybe Lude.Text) (\s a -> s {networkBorderGroup = a} :: VPCIPv6CidrBlockAssociation)
{-# DEPRECATED vicbaNetworkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead." #-}

-- | Information about the state of the CIDR block.
--
-- /Note:/ Consider using 'ipv6CidrBlockState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vicbaIPv6CidrBlockState :: Lens.Lens' VPCIPv6CidrBlockAssociation (Lude.Maybe VPCCidrBlockState)
vicbaIPv6CidrBlockState = Lens.lens (ipv6CidrBlockState :: VPCIPv6CidrBlockAssociation -> Lude.Maybe VPCCidrBlockState) (\s a -> s {ipv6CidrBlockState = a} :: VPCIPv6CidrBlockAssociation)
{-# DEPRECATED vicbaIPv6CidrBlockState "Use generic-lens or generic-optics with 'ipv6CidrBlockState' instead." #-}

-- | The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.
--
-- /Note:/ Consider using 'ipv6Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vicbaIPv6Pool :: Lens.Lens' VPCIPv6CidrBlockAssociation (Lude.Maybe Lude.Text)
vicbaIPv6Pool = Lens.lens (ipv6Pool :: VPCIPv6CidrBlockAssociation -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Pool = a} :: VPCIPv6CidrBlockAssociation)
{-# DEPRECATED vicbaIPv6Pool "Use generic-lens or generic-optics with 'ipv6Pool' instead." #-}

instance Lude.FromXML VPCIPv6CidrBlockAssociation where
  parseXML x =
    VPCIPv6CidrBlockAssociation'
      Lude.<$> (x Lude..@? "associationId")
      Lude.<*> (x Lude..@? "ipv6CidrBlock")
      Lude.<*> (x Lude..@? "networkBorderGroup")
      Lude.<*> (x Lude..@? "ipv6CidrBlockState")
      Lude.<*> (x Lude..@? "ipv6Pool")
