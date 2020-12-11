-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCCidrBlockAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCCidrBlockAssociation
  ( VPCCidrBlockAssociation (..),

    -- * Smart constructor
    mkVPCCidrBlockAssociation,

    -- * Lenses
    vcbaAssociationId,
    vcbaCidrBlockState,
    vcbaCidrBlock,
  )
where

import Network.AWS.EC2.Types.VPCCidrBlockState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv4 CIDR block associated with a VPC.
--
-- /See:/ 'mkVPCCidrBlockAssociation' smart constructor.
data VPCCidrBlockAssociation = VPCCidrBlockAssociation'
  { associationId ::
      Lude.Maybe Lude.Text,
    cidrBlockState ::
      Lude.Maybe VPCCidrBlockState,
    cidrBlock :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCCidrBlockAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID for the IPv4 CIDR block.
-- * 'cidrBlock' - The IPv4 CIDR block.
-- * 'cidrBlockState' - Information about the state of the CIDR block.
mkVPCCidrBlockAssociation ::
  VPCCidrBlockAssociation
mkVPCCidrBlockAssociation =
  VPCCidrBlockAssociation'
    { associationId = Lude.Nothing,
      cidrBlockState = Lude.Nothing,
      cidrBlock = Lude.Nothing
    }

-- | The association ID for the IPv4 CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcbaAssociationId :: Lens.Lens' VPCCidrBlockAssociation (Lude.Maybe Lude.Text)
vcbaAssociationId = Lens.lens (associationId :: VPCCidrBlockAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: VPCCidrBlockAssociation)
{-# DEPRECATED vcbaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | Information about the state of the CIDR block.
--
-- /Note:/ Consider using 'cidrBlockState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcbaCidrBlockState :: Lens.Lens' VPCCidrBlockAssociation (Lude.Maybe VPCCidrBlockState)
vcbaCidrBlockState = Lens.lens (cidrBlockState :: VPCCidrBlockAssociation -> Lude.Maybe VPCCidrBlockState) (\s a -> s {cidrBlockState = a} :: VPCCidrBlockAssociation)
{-# DEPRECATED vcbaCidrBlockState "Use generic-lens or generic-optics with 'cidrBlockState' instead." #-}

-- | The IPv4 CIDR block.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcbaCidrBlock :: Lens.Lens' VPCCidrBlockAssociation (Lude.Maybe Lude.Text)
vcbaCidrBlock = Lens.lens (cidrBlock :: VPCCidrBlockAssociation -> Lude.Maybe Lude.Text) (\s a -> s {cidrBlock = a} :: VPCCidrBlockAssociation)
{-# DEPRECATED vcbaCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

instance Lude.FromXML VPCCidrBlockAssociation where
  parseXML x =
    VPCCidrBlockAssociation'
      Lude.<$> (x Lude..@? "associationId")
      Lude.<*> (x Lude..@? "cidrBlockState")
      Lude.<*> (x Lude..@? "cidrBlock")
