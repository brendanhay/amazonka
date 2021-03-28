{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetIpv6CidrBlockAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SubnetIpv6CidrBlockAssociation
  ( SubnetIpv6CidrBlockAssociation (..)
  -- * Smart constructor
  , mkSubnetIpv6CidrBlockAssociation
  -- * Lenses
  , sicbaAssociationId
  , sicbaIpv6CidrBlock
  , sicbaIpv6CidrBlockState
  ) where

import qualified Network.AWS.EC2.Types.SubnetCidrBlockState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv6 CIDR block associated with a subnet.
--
-- /See:/ 'mkSubnetIpv6CidrBlockAssociation' smart constructor.
data SubnetIpv6CidrBlockAssociation = SubnetIpv6CidrBlockAssociation'
  { associationId :: Core.Maybe Core.Text
    -- ^ The association ID for the CIDR block.
  , ipv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR block.
  , ipv6CidrBlockState :: Core.Maybe Types.SubnetCidrBlockState
    -- ^ Information about the state of the CIDR block.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubnetIpv6CidrBlockAssociation' value with any optional fields omitted.
mkSubnetIpv6CidrBlockAssociation
    :: SubnetIpv6CidrBlockAssociation
mkSubnetIpv6CidrBlockAssociation
  = SubnetIpv6CidrBlockAssociation'{associationId = Core.Nothing,
                                    ipv6CidrBlock = Core.Nothing, ipv6CidrBlockState = Core.Nothing}

-- | The association ID for the CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicbaAssociationId :: Lens.Lens' SubnetIpv6CidrBlockAssociation (Core.Maybe Core.Text)
sicbaAssociationId = Lens.field @"associationId"
{-# INLINEABLE sicbaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicbaIpv6CidrBlock :: Lens.Lens' SubnetIpv6CidrBlockAssociation (Core.Maybe Core.Text)
sicbaIpv6CidrBlock = Lens.field @"ipv6CidrBlock"
{-# INLINEABLE sicbaIpv6CidrBlock #-}
{-# DEPRECATED ipv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead"  #-}

-- | Information about the state of the CIDR block.
--
-- /Note:/ Consider using 'ipv6CidrBlockState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicbaIpv6CidrBlockState :: Lens.Lens' SubnetIpv6CidrBlockAssociation (Core.Maybe Types.SubnetCidrBlockState)
sicbaIpv6CidrBlockState = Lens.field @"ipv6CidrBlockState"
{-# INLINEABLE sicbaIpv6CidrBlockState #-}
{-# DEPRECATED ipv6CidrBlockState "Use generic-lens or generic-optics with 'ipv6CidrBlockState' instead"  #-}

instance Core.FromXML SubnetIpv6CidrBlockAssociation where
        parseXML x
          = SubnetIpv6CidrBlockAssociation' Core.<$>
              (x Core..@? "associationId") Core.<*> x Core..@? "ipv6CidrBlock"
                Core.<*> x Core..@? "ipv6CidrBlockState"
