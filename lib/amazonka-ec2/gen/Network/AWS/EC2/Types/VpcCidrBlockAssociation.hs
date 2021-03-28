{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcCidrBlockAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcCidrBlockAssociation
  ( VpcCidrBlockAssociation (..)
  -- * Smart constructor
  , mkVpcCidrBlockAssociation
  -- * Lenses
  , vcbaAssociationId
  , vcbaCidrBlock
  , vcbaCidrBlockState
  ) where

import qualified Network.AWS.EC2.Types.VpcCidrBlockState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv4 CIDR block associated with a VPC.
--
-- /See:/ 'mkVpcCidrBlockAssociation' smart constructor.
data VpcCidrBlockAssociation = VpcCidrBlockAssociation'
  { associationId :: Core.Maybe Core.Text
    -- ^ The association ID for the IPv4 CIDR block.
  , cidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR block.
  , cidrBlockState :: Core.Maybe Types.VpcCidrBlockState
    -- ^ Information about the state of the CIDR block.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcCidrBlockAssociation' value with any optional fields omitted.
mkVpcCidrBlockAssociation
    :: VpcCidrBlockAssociation
mkVpcCidrBlockAssociation
  = VpcCidrBlockAssociation'{associationId = Core.Nothing,
                             cidrBlock = Core.Nothing, cidrBlockState = Core.Nothing}

-- | The association ID for the IPv4 CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcbaAssociationId :: Lens.Lens' VpcCidrBlockAssociation (Core.Maybe Core.Text)
vcbaAssociationId = Lens.field @"associationId"
{-# INLINEABLE vcbaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The IPv4 CIDR block.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcbaCidrBlock :: Lens.Lens' VpcCidrBlockAssociation (Core.Maybe Core.Text)
vcbaCidrBlock = Lens.field @"cidrBlock"
{-# INLINEABLE vcbaCidrBlock #-}
{-# DEPRECATED cidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead"  #-}

-- | Information about the state of the CIDR block.
--
-- /Note:/ Consider using 'cidrBlockState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcbaCidrBlockState :: Lens.Lens' VpcCidrBlockAssociation (Core.Maybe Types.VpcCidrBlockState)
vcbaCidrBlockState = Lens.field @"cidrBlockState"
{-# INLINEABLE vcbaCidrBlockState #-}
{-# DEPRECATED cidrBlockState "Use generic-lens or generic-optics with 'cidrBlockState' instead"  #-}

instance Core.FromXML VpcCidrBlockAssociation where
        parseXML x
          = VpcCidrBlockAssociation' Core.<$>
              (x Core..@? "associationId") Core.<*> x Core..@? "cidrBlock"
                Core.<*> x Core..@? "cidrBlockState"
