{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetIPv6CidrBlockAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetIPv6CidrBlockAssociation
  ( SubnetIPv6CidrBlockAssociation (..),

    -- * Smart constructor
    mkSubnetIPv6CidrBlockAssociation,

    -- * Lenses
    sicbaAssociationId,
    sicbaIPv6CidrBlock,
    sicbaIPv6CidrBlockState,
  )
where

import Network.AWS.EC2.Types.SubnetCidrBlockState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv6 CIDR block associated with a subnet.
--
-- /See:/ 'mkSubnetIPv6CidrBlockAssociation' smart constructor.
data SubnetIPv6CidrBlockAssociation = SubnetIPv6CidrBlockAssociation'
  { -- | The association ID for the CIDR block.
    associationId :: Lude.Maybe Lude.Text,
    -- | The IPv6 CIDR block.
    ipv6CidrBlock :: Lude.Maybe Lude.Text,
    -- | Information about the state of the CIDR block.
    ipv6CidrBlockState :: Lude.Maybe SubnetCidrBlockState
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubnetIPv6CidrBlockAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID for the CIDR block.
-- * 'ipv6CidrBlock' - The IPv6 CIDR block.
-- * 'ipv6CidrBlockState' - Information about the state of the CIDR block.
mkSubnetIPv6CidrBlockAssociation ::
  SubnetIPv6CidrBlockAssociation
mkSubnetIPv6CidrBlockAssociation =
  SubnetIPv6CidrBlockAssociation'
    { associationId = Lude.Nothing,
      ipv6CidrBlock = Lude.Nothing,
      ipv6CidrBlockState = Lude.Nothing
    }

-- | The association ID for the CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicbaAssociationId :: Lens.Lens' SubnetIPv6CidrBlockAssociation (Lude.Maybe Lude.Text)
sicbaAssociationId = Lens.lens (associationId :: SubnetIPv6CidrBlockAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: SubnetIPv6CidrBlockAssociation)
{-# DEPRECATED sicbaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicbaIPv6CidrBlock :: Lens.Lens' SubnetIPv6CidrBlockAssociation (Lude.Maybe Lude.Text)
sicbaIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: SubnetIPv6CidrBlockAssociation -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: SubnetIPv6CidrBlockAssociation)
{-# DEPRECATED sicbaIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

-- | Information about the state of the CIDR block.
--
-- /Note:/ Consider using 'ipv6CidrBlockState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicbaIPv6CidrBlockState :: Lens.Lens' SubnetIPv6CidrBlockAssociation (Lude.Maybe SubnetCidrBlockState)
sicbaIPv6CidrBlockState = Lens.lens (ipv6CidrBlockState :: SubnetIPv6CidrBlockAssociation -> Lude.Maybe SubnetCidrBlockState) (\s a -> s {ipv6CidrBlockState = a} :: SubnetIPv6CidrBlockAssociation)
{-# DEPRECATED sicbaIPv6CidrBlockState "Use generic-lens or generic-optics with 'ipv6CidrBlockState' instead." #-}

instance Lude.FromXML SubnetIPv6CidrBlockAssociation where
  parseXML x =
    SubnetIPv6CidrBlockAssociation'
      Lude.<$> (x Lude..@? "associationId")
      Lude.<*> (x Lude..@? "ipv6CidrBlock")
      Lude.<*> (x Lude..@? "ipv6CidrBlockState")
