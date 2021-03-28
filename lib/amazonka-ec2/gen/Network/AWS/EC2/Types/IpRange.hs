{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IpRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.IpRange
  ( IpRange (..)
  -- * Smart constructor
  , mkIpRange
  -- * Lenses
  , iCidrIp
  , iDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv4 range.
--
-- /See:/ 'mkIpRange' smart constructor.
data IpRange = IpRange'
  { cidrIp :: Core.Text
    -- ^ The IPv4 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv4 address, use the /32 prefix length.
  , description :: Core.Maybe Core.Text
    -- ^ A description for the security group rule that references this IPv4 address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IpRange' value with any optional fields omitted.
mkIpRange
    :: Core.Text -- ^ 'cidrIp'
    -> IpRange
mkIpRange cidrIp = IpRange'{cidrIp, description = Core.Nothing}

-- | The IPv4 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv4 address, use the /32 prefix length.
--
-- /Note:/ Consider using 'cidrIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCidrIp :: Lens.Lens' IpRange Core.Text
iCidrIp = Lens.field @"cidrIp"
{-# INLINEABLE iCidrIp #-}
{-# DEPRECATED cidrIp "Use generic-lens or generic-optics with 'cidrIp' instead"  #-}

-- | A description for the security group rule that references this IPv4 address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDescription :: Lens.Lens' IpRange (Core.Maybe Core.Text)
iDescription = Lens.field @"description"
{-# INLINEABLE iDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery IpRange where
        toQuery IpRange{..}
          = Core.toQueryPair "CidrIp" cidrIp Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description

instance Core.FromXML IpRange where
        parseXML x
          = IpRange' Core.<$>
              (x Core..@ "cidrIp") Core.<*> x Core..@? "description"
