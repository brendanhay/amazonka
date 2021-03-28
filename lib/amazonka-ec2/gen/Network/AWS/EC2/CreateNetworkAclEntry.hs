{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkAclEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an entry (a rule) in a network ACL with the specified rule number. Each network ACL has a set of numbered ingress rules and a separate set of numbered egress rules. When determining whether a packet should be allowed in or out of a subnet associated with the ACL, we process the entries in the ACL according to the rule numbers, in ascending order. Each network ACL has a set of ingress rules and a separate set of egress rules.
--
-- We recommend that you leave room between the rule numbers (for example, 100, 110, 120, ...), and not number them one right after the other (for example, 101, 102, 103, ...). This makes it easier to add a rule between existing ones without having to renumber the rules.
-- After you add an entry, you can't modify it; you must either replace it, or create an entry and delete the old one.
-- For more information about network ACLs, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateNetworkAclEntry
    (
    -- * Creating a request
      CreateNetworkAclEntry (..)
    , mkCreateNetworkAclEntry
    -- ** Request lenses
    , cnaeEgress
    , cnaeNetworkAclId
    , cnaeProtocol
    , cnaeRuleAction
    , cnaeRuleNumber
    , cnaeCidrBlock
    , cnaeDryRun
    , cnaeIcmpTypeCode
    , cnaeIpv6CidrBlock
    , cnaePortRange

    -- * Destructuring the response
    , CreateNetworkAclEntryResponse (..)
    , mkCreateNetworkAclEntryResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateNetworkAclEntry' smart constructor.
data CreateNetworkAclEntry = CreateNetworkAclEntry'
  { egress :: Core.Bool
    -- ^ Indicates whether this is an egress rule (rule is applied to traffic leaving the subnet).
  , networkAclId :: Types.NetworkAclId
    -- ^ The ID of the network ACL.
  , protocol :: Core.Text
    -- ^ The protocol number. A value of "-1" means all protocols. If you specify "-1" or a protocol number other than "6" (TCP), "17" (UDP), or "1" (ICMP), traffic on all ports is allowed, regardless of any ports or ICMP types or codes that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
  , ruleAction :: Types.RuleAction
    -- ^ Indicates whether to allow or deny the traffic that matches the rule.
  , ruleNumber :: Core.Int
    -- ^ The rule number for the entry (for example, 100). ACL entries are processed in ascending order by rule number.
--
-- Constraints: Positive integer from 1 to 32766. The range 32767 to 65535 is reserved for internal use.
  , cidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ). We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , icmpTypeCode :: Core.Maybe Types.IcmpTypeCode
    -- ^ ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
  , ipv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 network range to allow or deny, in CIDR notation (for example @2001:db8:1234:1a00::/64@ ).
  , portRange :: Core.Maybe Types.PortRange
    -- ^ TCP or UDP protocols: The range of ports the rule applies to. Required if specifying protocol 6 (TCP) or 17 (UDP).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkAclEntry' value with any optional fields omitted.
mkCreateNetworkAclEntry
    :: Core.Bool -- ^ 'egress'
    -> Types.NetworkAclId -- ^ 'networkAclId'
    -> Core.Text -- ^ 'protocol'
    -> Types.RuleAction -- ^ 'ruleAction'
    -> Core.Int -- ^ 'ruleNumber'
    -> CreateNetworkAclEntry
mkCreateNetworkAclEntry egress networkAclId protocol ruleAction
  ruleNumber
  = CreateNetworkAclEntry'{egress, networkAclId, protocol,
                           ruleAction, ruleNumber, cidrBlock = Core.Nothing,
                           dryRun = Core.Nothing, icmpTypeCode = Core.Nothing,
                           ipv6CidrBlock = Core.Nothing, portRange = Core.Nothing}

-- | Indicates whether this is an egress rule (rule is applied to traffic leaving the subnet).
--
-- /Note:/ Consider using 'egress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeEgress :: Lens.Lens' CreateNetworkAclEntry Core.Bool
cnaeEgress = Lens.field @"egress"
{-# INLINEABLE cnaeEgress #-}
{-# DEPRECATED egress "Use generic-lens or generic-optics with 'egress' instead"  #-}

-- | The ID of the network ACL.
--
-- /Note:/ Consider using 'networkAclId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeNetworkAclId :: Lens.Lens' CreateNetworkAclEntry Types.NetworkAclId
cnaeNetworkAclId = Lens.field @"networkAclId"
{-# INLINEABLE cnaeNetworkAclId #-}
{-# DEPRECATED networkAclId "Use generic-lens or generic-optics with 'networkAclId' instead"  #-}

-- | The protocol number. A value of "-1" means all protocols. If you specify "-1" or a protocol number other than "6" (TCP), "17" (UDP), or "1" (ICMP), traffic on all ports is allowed, regardless of any ports or ICMP types or codes that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeProtocol :: Lens.Lens' CreateNetworkAclEntry Core.Text
cnaeProtocol = Lens.field @"protocol"
{-# INLINEABLE cnaeProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeRuleAction :: Lens.Lens' CreateNetworkAclEntry Types.RuleAction
cnaeRuleAction = Lens.field @"ruleAction"
{-# INLINEABLE cnaeRuleAction #-}
{-# DEPRECATED ruleAction "Use generic-lens or generic-optics with 'ruleAction' instead"  #-}

-- | The rule number for the entry (for example, 100). ACL entries are processed in ascending order by rule number.
--
-- Constraints: Positive integer from 1 to 32766. The range 32767 to 65535 is reserved for internal use.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeRuleNumber :: Lens.Lens' CreateNetworkAclEntry Core.Int
cnaeRuleNumber = Lens.field @"ruleNumber"
{-# INLINEABLE cnaeRuleNumber #-}
{-# DEPRECATED ruleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead"  #-}

-- | The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ). We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeCidrBlock :: Lens.Lens' CreateNetworkAclEntry (Core.Maybe Core.Text)
cnaeCidrBlock = Lens.field @"cidrBlock"
{-# INLINEABLE cnaeCidrBlock #-}
{-# DEPRECATED cidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeDryRun :: Lens.Lens' CreateNetworkAclEntry (Core.Maybe Core.Bool)
cnaeDryRun = Lens.field @"dryRun"
{-# INLINEABLE cnaeDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
--
-- /Note:/ Consider using 'icmpTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeIcmpTypeCode :: Lens.Lens' CreateNetworkAclEntry (Core.Maybe Types.IcmpTypeCode)
cnaeIcmpTypeCode = Lens.field @"icmpTypeCode"
{-# INLINEABLE cnaeIcmpTypeCode #-}
{-# DEPRECATED icmpTypeCode "Use generic-lens or generic-optics with 'icmpTypeCode' instead"  #-}

-- | The IPv6 network range to allow or deny, in CIDR notation (for example @2001:db8:1234:1a00::/64@ ).
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeIpv6CidrBlock :: Lens.Lens' CreateNetworkAclEntry (Core.Maybe Core.Text)
cnaeIpv6CidrBlock = Lens.field @"ipv6CidrBlock"
{-# INLINEABLE cnaeIpv6CidrBlock #-}
{-# DEPRECATED ipv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead"  #-}

-- | TCP or UDP protocols: The range of ports the rule applies to. Required if specifying protocol 6 (TCP) or 17 (UDP).
--
-- /Note:/ Consider using 'portRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaePortRange :: Lens.Lens' CreateNetworkAclEntry (Core.Maybe Types.PortRange)
cnaePortRange = Lens.field @"portRange"
{-# INLINEABLE cnaePortRange #-}
{-# DEPRECATED portRange "Use generic-lens or generic-optics with 'portRange' instead"  #-}

instance Core.ToQuery CreateNetworkAclEntry where
        toQuery CreateNetworkAclEntry{..}
          = Core.toQueryPair "Action" ("CreateNetworkAclEntry" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Egress" egress
              Core.<> Core.toQueryPair "NetworkAclId" networkAclId
              Core.<> Core.toQueryPair "Protocol" protocol
              Core.<> Core.toQueryPair "RuleAction" ruleAction
              Core.<> Core.toQueryPair "RuleNumber" ruleNumber
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CidrBlock") cidrBlock
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Icmp") icmpTypeCode
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6CidrBlock")
                ipv6CidrBlock
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PortRange") portRange

instance Core.ToHeaders CreateNetworkAclEntry where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateNetworkAclEntry where
        type Rs CreateNetworkAclEntry = CreateNetworkAclEntryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull CreateNetworkAclEntryResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateNetworkAclEntryResponse' smart constructor.
data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkAclEntryResponse' value with any optional fields omitted.
mkCreateNetworkAclEntryResponse
    :: CreateNetworkAclEntryResponse
mkCreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse'
