{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Ipv6CidrAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Ipv6CidrAssociation
  ( Ipv6CidrAssociation (..)
  -- * Smart constructor
  , mkIpv6CidrAssociation
  -- * Lenses
  , icaAssociatedResource
  , icaIpv6Cidr
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv6 CIDR block association.
--
-- /See:/ 'mkIpv6CidrAssociation' smart constructor.
data Ipv6CidrAssociation = Ipv6CidrAssociation'
  { associatedResource :: Core.Maybe Core.Text
    -- ^ The resource that's associated with the IPv6 CIDR block.
  , ipv6Cidr :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR block.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ipv6CidrAssociation' value with any optional fields omitted.
mkIpv6CidrAssociation
    :: Ipv6CidrAssociation
mkIpv6CidrAssociation
  = Ipv6CidrAssociation'{associatedResource = Core.Nothing,
                         ipv6Cidr = Core.Nothing}

-- | The resource that's associated with the IPv6 CIDR block.
--
-- /Note:/ Consider using 'associatedResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icaAssociatedResource :: Lens.Lens' Ipv6CidrAssociation (Core.Maybe Core.Text)
icaAssociatedResource = Lens.field @"associatedResource"
{-# INLINEABLE icaAssociatedResource #-}
{-# DEPRECATED associatedResource "Use generic-lens or generic-optics with 'associatedResource' instead"  #-}

-- | The IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6Cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icaIpv6Cidr :: Lens.Lens' Ipv6CidrAssociation (Core.Maybe Core.Text)
icaIpv6Cidr = Lens.field @"ipv6Cidr"
{-# INLINEABLE icaIpv6Cidr #-}
{-# DEPRECATED ipv6Cidr "Use generic-lens or generic-optics with 'ipv6Cidr' instead"  #-}

instance Core.FromXML Ipv6CidrAssociation where
        parseXML x
          = Ipv6CidrAssociation' Core.<$>
              (x Core..@? "associatedResource") Core.<*> x Core..@? "ipv6Cidr"
