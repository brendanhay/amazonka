{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StaleIpPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.StaleIpPermission
  ( StaleIpPermission (..)
  -- * Smart constructor
  , mkStaleIpPermission
  -- * Lenses
  , sipFromPort
  , sipIpProtocol
  , sipIpRanges
  , sipPrefixListIds
  , sipToPort
  , sipUserIdGroupPairs
  ) where

import qualified Network.AWS.EC2.Types.UserIdGroupPair as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a stale rule in a security group.
--
-- /See:/ 'mkStaleIpPermission' smart constructor.
data StaleIpPermission = StaleIpPermission'
  { fromPort :: Core.Maybe Core.Int
    -- ^ The start of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types. 
  , ipProtocol :: Core.Maybe Core.Text
    -- ^ The IP protocol name (for @tcp@ , @udp@ , and @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)> .
  , ipRanges :: Core.Maybe [Core.Text]
    -- ^ The IP ranges. Not applicable for stale security group rules.
  , prefixListIds :: Core.Maybe [Core.Text]
    -- ^ The prefix list IDs. Not applicable for stale security group rules.
  , toPort :: Core.Maybe Core.Int
    -- ^ The end of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types. 
  , userIdGroupPairs :: Core.Maybe [Types.UserIdGroupPair]
    -- ^ The security group pairs. Returns the ID of the referenced security group and VPC, and the ID and status of the VPC peering connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StaleIpPermission' value with any optional fields omitted.
mkStaleIpPermission
    :: StaleIpPermission
mkStaleIpPermission
  = StaleIpPermission'{fromPort = Core.Nothing,
                       ipProtocol = Core.Nothing, ipRanges = Core.Nothing,
                       prefixListIds = Core.Nothing, toPort = Core.Nothing,
                       userIdGroupPairs = Core.Nothing}

-- | The start of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types. 
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipFromPort :: Lens.Lens' StaleIpPermission (Core.Maybe Core.Int)
sipFromPort = Lens.field @"fromPort"
{-# INLINEABLE sipFromPort #-}
{-# DEPRECATED fromPort "Use generic-lens or generic-optics with 'fromPort' instead"  #-}

-- | The IP protocol name (for @tcp@ , @udp@ , and @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)> .
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipIpProtocol :: Lens.Lens' StaleIpPermission (Core.Maybe Core.Text)
sipIpProtocol = Lens.field @"ipProtocol"
{-# INLINEABLE sipIpProtocol #-}
{-# DEPRECATED ipProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead"  #-}

-- | The IP ranges. Not applicable for stale security group rules.
--
-- /Note:/ Consider using 'ipRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipIpRanges :: Lens.Lens' StaleIpPermission (Core.Maybe [Core.Text])
sipIpRanges = Lens.field @"ipRanges"
{-# INLINEABLE sipIpRanges #-}
{-# DEPRECATED ipRanges "Use generic-lens or generic-optics with 'ipRanges' instead"  #-}

-- | The prefix list IDs. Not applicable for stale security group rules.
--
-- /Note:/ Consider using 'prefixListIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipPrefixListIds :: Lens.Lens' StaleIpPermission (Core.Maybe [Core.Text])
sipPrefixListIds = Lens.field @"prefixListIds"
{-# INLINEABLE sipPrefixListIds #-}
{-# DEPRECATED prefixListIds "Use generic-lens or generic-optics with 'prefixListIds' instead"  #-}

-- | The end of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types. 
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipToPort :: Lens.Lens' StaleIpPermission (Core.Maybe Core.Int)
sipToPort = Lens.field @"toPort"
{-# INLINEABLE sipToPort #-}
{-# DEPRECATED toPort "Use generic-lens or generic-optics with 'toPort' instead"  #-}

-- | The security group pairs. Returns the ID of the referenced security group and VPC, and the ID and status of the VPC peering connection.
--
-- /Note:/ Consider using 'userIdGroupPairs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipUserIdGroupPairs :: Lens.Lens' StaleIpPermission (Core.Maybe [Types.UserIdGroupPair])
sipUserIdGroupPairs = Lens.field @"userIdGroupPairs"
{-# INLINEABLE sipUserIdGroupPairs #-}
{-# DEPRECATED userIdGroupPairs "Use generic-lens or generic-optics with 'userIdGroupPairs' instead"  #-}

instance Core.FromXML StaleIpPermission where
        parseXML x
          = StaleIpPermission' Core.<$>
              (x Core..@? "fromPort") Core.<*> x Core..@? "ipProtocol" Core.<*>
                x Core..@? "ipRanges" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "prefixListIds" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "toPort"
                Core.<*> x Core..@? "groups" Core..<@> Core.parseXMLList "item"
