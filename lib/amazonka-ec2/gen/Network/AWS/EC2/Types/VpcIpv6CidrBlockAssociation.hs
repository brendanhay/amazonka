{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcIpv6CidrBlockAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcIpv6CidrBlockAssociation
  ( VpcIpv6CidrBlockAssociation (..)
  -- * Smart constructor
  , mkVpcIpv6CidrBlockAssociation
  -- * Lenses
  , vicbaAssociationId
  , vicbaIpv6CidrBlock
  , vicbaIpv6CidrBlockState
  , vicbaIpv6Pool
  , vicbaNetworkBorderGroup
  ) where

import qualified Network.AWS.EC2.Types.VpcCidrBlockState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv6 CIDR block associated with a VPC.
--
-- /See:/ 'mkVpcIpv6CidrBlockAssociation' smart constructor.
data VpcIpv6CidrBlockAssociation = VpcIpv6CidrBlockAssociation'
  { associationId :: Core.Maybe Core.Text
    -- ^ The association ID for the IPv6 CIDR block.
  , ipv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR block.
  , ipv6CidrBlockState :: Core.Maybe Types.VpcCidrBlockState
    -- ^ Information about the state of the CIDR block.
  , ipv6Pool :: Core.Maybe Core.Text
    -- ^ The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.
  , networkBorderGroup :: Core.Maybe Core.Text
    -- ^ The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses, for example, @us-east-1-wl1-bos-wlz-1@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcIpv6CidrBlockAssociation' value with any optional fields omitted.
mkVpcIpv6CidrBlockAssociation
    :: VpcIpv6CidrBlockAssociation
mkVpcIpv6CidrBlockAssociation
  = VpcIpv6CidrBlockAssociation'{associationId = Core.Nothing,
                                 ipv6CidrBlock = Core.Nothing, ipv6CidrBlockState = Core.Nothing,
                                 ipv6Pool = Core.Nothing, networkBorderGroup = Core.Nothing}

-- | The association ID for the IPv6 CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vicbaAssociationId :: Lens.Lens' VpcIpv6CidrBlockAssociation (Core.Maybe Core.Text)
vicbaAssociationId = Lens.field @"associationId"
{-# INLINEABLE vicbaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vicbaIpv6CidrBlock :: Lens.Lens' VpcIpv6CidrBlockAssociation (Core.Maybe Core.Text)
vicbaIpv6CidrBlock = Lens.field @"ipv6CidrBlock"
{-# INLINEABLE vicbaIpv6CidrBlock #-}
{-# DEPRECATED ipv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead"  #-}

-- | Information about the state of the CIDR block.
--
-- /Note:/ Consider using 'ipv6CidrBlockState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vicbaIpv6CidrBlockState :: Lens.Lens' VpcIpv6CidrBlockAssociation (Core.Maybe Types.VpcCidrBlockState)
vicbaIpv6CidrBlockState = Lens.field @"ipv6CidrBlockState"
{-# INLINEABLE vicbaIpv6CidrBlockState #-}
{-# DEPRECATED ipv6CidrBlockState "Use generic-lens or generic-optics with 'ipv6CidrBlockState' instead"  #-}

-- | The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.
--
-- /Note:/ Consider using 'ipv6Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vicbaIpv6Pool :: Lens.Lens' VpcIpv6CidrBlockAssociation (Core.Maybe Core.Text)
vicbaIpv6Pool = Lens.field @"ipv6Pool"
{-# INLINEABLE vicbaIpv6Pool #-}
{-# DEPRECATED ipv6Pool "Use generic-lens or generic-optics with 'ipv6Pool' instead"  #-}

-- | The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses, for example, @us-east-1-wl1-bos-wlz-1@ .
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vicbaNetworkBorderGroup :: Lens.Lens' VpcIpv6CidrBlockAssociation (Core.Maybe Core.Text)
vicbaNetworkBorderGroup = Lens.field @"networkBorderGroup"
{-# INLINEABLE vicbaNetworkBorderGroup #-}
{-# DEPRECATED networkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead"  #-}

instance Core.FromXML VpcIpv6CidrBlockAssociation where
        parseXML x
          = VpcIpv6CidrBlockAssociation' Core.<$>
              (x Core..@? "associationId") Core.<*> x Core..@? "ipv6CidrBlock"
                Core.<*> x Core..@? "ipv6CidrBlockState"
                Core.<*> x Core..@? "ipv6Pool"
                Core.<*> x Core..@? "networkBorderGroup"
