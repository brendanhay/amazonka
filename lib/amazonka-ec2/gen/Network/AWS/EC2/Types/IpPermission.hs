{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IpPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.IpPermission
  ( IpPermission (..)
  -- * Smart constructor
  , mkIpPermission
  -- * Lenses
  , ipFromPort
  , ipIpProtocol
  , ipIpRanges
  , ipIpv6Ranges
  , ipPrefixListIds
  , ipToPort
  , ipUserIdGroupPairs
  ) where

import qualified Network.AWS.EC2.Types.IpRange as Types
import qualified Network.AWS.EC2.Types.Ipv6Range as Types
import qualified Network.AWS.EC2.Types.PrefixListId as Types
import qualified Network.AWS.EC2.Types.UserIdGroupPair as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a set of permissions for a security group rule.
--
-- /See:/ 'mkIpPermission' smart constructor.
data IpPermission = IpPermission'
  { fromPort :: Core.Maybe Core.Int
    -- ^ The start of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types. If you specify all ICMP/ICMPv6 types, you must specify all codes.
  , ipProtocol :: Core.Text
    -- ^ The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ).
--
-- [VPC only] Use @-1@ to specify all protocols. When authorizing security group rules, specifying @-1@ or a protocol number other than @tcp@ , @udp@ , @icmp@ , or @icmpv6@ allows traffic on all ports, regardless of any port range you specify. For @tcp@ , @udp@ , and @icmp@ , you must specify a port range. For @icmpv6@ , the port range is optional; if you omit the port range, traffic for all types and codes is allowed.
  , ipRanges :: Core.Maybe [Types.IpRange]
    -- ^ The IPv4 ranges.
  , ipv6Ranges :: Core.Maybe [Types.Ipv6Range]
    -- ^ [VPC only] The IPv6 ranges.
  , prefixListIds :: Core.Maybe [Types.PrefixListId]
    -- ^ [VPC only] The prefix list IDs.
  , toPort :: Core.Maybe Core.Int
    -- ^ The end of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes. If you specify all ICMP/ICMPv6 types, you must specify all codes.
  , userIdGroupPairs :: Core.Maybe [Types.UserIdGroupPair]
    -- ^ The security group and AWS account ID pairs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IpPermission' value with any optional fields omitted.
mkIpPermission
    :: Core.Text -- ^ 'ipProtocol'
    -> IpPermission
mkIpPermission ipProtocol
  = IpPermission'{fromPort = Core.Nothing, ipProtocol,
                  ipRanges = Core.Nothing, ipv6Ranges = Core.Nothing,
                  prefixListIds = Core.Nothing, toPort = Core.Nothing,
                  userIdGroupPairs = Core.Nothing}

-- | The start of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types. If you specify all ICMP/ICMPv6 types, you must specify all codes.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipFromPort :: Lens.Lens' IpPermission (Core.Maybe Core.Int)
ipFromPort = Lens.field @"fromPort"
{-# INLINEABLE ipFromPort #-}
{-# DEPRECATED fromPort "Use generic-lens or generic-optics with 'fromPort' instead"  #-}

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ).
--
-- [VPC only] Use @-1@ to specify all protocols. When authorizing security group rules, specifying @-1@ or a protocol number other than @tcp@ , @udp@ , @icmp@ , or @icmpv6@ allows traffic on all ports, regardless of any port range you specify. For @tcp@ , @udp@ , and @icmp@ , you must specify a port range. For @icmpv6@ , the port range is optional; if you omit the port range, traffic for all types and codes is allowed.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIpProtocol :: Lens.Lens' IpPermission Core.Text
ipIpProtocol = Lens.field @"ipProtocol"
{-# INLINEABLE ipIpProtocol #-}
{-# DEPRECATED ipProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead"  #-}

-- | The IPv4 ranges.
--
-- /Note:/ Consider using 'ipRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIpRanges :: Lens.Lens' IpPermission (Core.Maybe [Types.IpRange])
ipIpRanges = Lens.field @"ipRanges"
{-# INLINEABLE ipIpRanges #-}
{-# DEPRECATED ipRanges "Use generic-lens or generic-optics with 'ipRanges' instead"  #-}

-- | [VPC only] The IPv6 ranges.
--
-- /Note:/ Consider using 'ipv6Ranges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIpv6Ranges :: Lens.Lens' IpPermission (Core.Maybe [Types.Ipv6Range])
ipIpv6Ranges = Lens.field @"ipv6Ranges"
{-# INLINEABLE ipIpv6Ranges #-}
{-# DEPRECATED ipv6Ranges "Use generic-lens or generic-optics with 'ipv6Ranges' instead"  #-}

-- | [VPC only] The prefix list IDs.
--
-- /Note:/ Consider using 'prefixListIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipPrefixListIds :: Lens.Lens' IpPermission (Core.Maybe [Types.PrefixListId])
ipPrefixListIds = Lens.field @"prefixListIds"
{-# INLINEABLE ipPrefixListIds #-}
{-# DEPRECATED prefixListIds "Use generic-lens or generic-optics with 'prefixListIds' instead"  #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes. If you specify all ICMP/ICMPv6 types, you must specify all codes.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipToPort :: Lens.Lens' IpPermission (Core.Maybe Core.Int)
ipToPort = Lens.field @"toPort"
{-# INLINEABLE ipToPort #-}
{-# DEPRECATED toPort "Use generic-lens or generic-optics with 'toPort' instead"  #-}

-- | The security group and AWS account ID pairs.
--
-- /Note:/ Consider using 'userIdGroupPairs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipUserIdGroupPairs :: Lens.Lens' IpPermission (Core.Maybe [Types.UserIdGroupPair])
ipUserIdGroupPairs = Lens.field @"userIdGroupPairs"
{-# INLINEABLE ipUserIdGroupPairs #-}
{-# DEPRECATED userIdGroupPairs "Use generic-lens or generic-optics with 'userIdGroupPairs' instead"  #-}

instance Core.ToQuery IpPermission where
        toQuery IpPermission{..}
          = Core.maybe Core.mempty (Core.toQueryPair "FromPort") fromPort
              Core.<> Core.toQueryPair "IpProtocol" ipProtocol
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "IpRanges") ipRanges
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Ipv6Ranges") ipv6Ranges
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "PrefixListIds")
                prefixListIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "ToPort") toPort
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Groups") userIdGroupPairs

instance Core.FromXML IpPermission where
        parseXML x
          = IpPermission' Core.<$>
              (x Core..@? "fromPort") Core.<*> x Core..@ "ipProtocol" Core.<*>
                x Core..@? "ipRanges" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "ipv6Ranges" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "prefixListIds" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "toPort"
                Core.<*> x Core..@? "groups" Core..<@> Core.parseXMLList "item"
