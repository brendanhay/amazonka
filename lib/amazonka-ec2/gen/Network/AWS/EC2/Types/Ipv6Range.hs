{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Ipv6Range
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Ipv6Range
  ( Ipv6Range (..)
  -- * Smart constructor
  , mkIpv6Range
  -- * Lenses
  , irCidrIpv6
  , irDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | [EC2-VPC only] Describes an IPv6 range.
--
-- /See:/ 'mkIpv6Range' smart constructor.
data Ipv6Range = Ipv6Range'
  { cidrIpv6 :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv6 address, use the /128 prefix length.
  , description :: Core.Maybe Core.Text
    -- ^ A description for the security group rule that references this IPv6 address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ipv6Range' value with any optional fields omitted.
mkIpv6Range
    :: Ipv6Range
mkIpv6Range
  = Ipv6Range'{cidrIpv6 = Core.Nothing, description = Core.Nothing}

-- | The IPv6 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv6 address, use the /128 prefix length.
--
-- /Note:/ Consider using 'cidrIpv6' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irCidrIpv6 :: Lens.Lens' Ipv6Range (Core.Maybe Core.Text)
irCidrIpv6 = Lens.field @"cidrIpv6"
{-# INLINEABLE irCidrIpv6 #-}
{-# DEPRECATED cidrIpv6 "Use generic-lens or generic-optics with 'cidrIpv6' instead"  #-}

-- | A description for the security group rule that references this IPv6 address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irDescription :: Lens.Lens' Ipv6Range (Core.Maybe Core.Text)
irDescription = Lens.field @"description"
{-# INLINEABLE irDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery Ipv6Range where
        toQuery Ipv6Range{..}
          = Core.maybe Core.mempty (Core.toQueryPair "CidrIpv6") cidrIpv6
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description

instance Core.FromXML Ipv6Range where
        parseXML x
          = Ipv6Range' Core.<$>
              (x Core..@? "cidrIpv6") Core.<*> x Core..@? "description"
