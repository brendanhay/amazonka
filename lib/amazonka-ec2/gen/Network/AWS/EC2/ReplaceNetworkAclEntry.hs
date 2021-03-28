{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceNetworkAclEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an entry (rule) in a network ACL. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.ReplaceNetworkAclEntry
    (
    -- * Creating a request
      ReplaceNetworkAclEntry (..)
    , mkReplaceNetworkAclEntry
    -- ** Request lenses
    , rnaeEgress
    , rnaeNetworkAclId
    , rnaeProtocol
    , rnaeRuleAction
    , rnaeRuleNumber
    , rnaeCidrBlock
    , rnaeDryRun
    , rnaeIcmpTypeCode
    , rnaeIpv6CidrBlock
    , rnaePortRange

    -- * Destructuring the response
    , ReplaceNetworkAclEntryResponse (..)
    , mkReplaceNetworkAclEntryResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReplaceNetworkAclEntry' smart constructor.
data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry'
  { egress :: Core.Bool
    -- ^ Indicates whether to replace the egress rule.
--
-- Default: If no value is specified, we replace the ingress rule.
  , networkAclId :: Types.NetworkAclId
    -- ^ The ID of the ACL.
  , protocol :: Core.Text
    -- ^ The protocol number. A value of "-1" means all protocols. If you specify "-1" or a protocol number other than "6" (TCP), "17" (UDP), or "1" (ICMP), traffic on all ports is allowed, regardless of any ports or ICMP types or codes that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
  , ruleAction :: Types.RuleAction
    -- ^ Indicates whether to allow or deny the traffic that matches the rule.
  , ruleNumber :: Core.Int
    -- ^ The rule number of the entry to replace.
  , cidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ).
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , icmpTypeCode :: Core.Maybe Types.IcmpTypeCode
    -- ^ ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
  , ipv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 network range to allow or deny, in CIDR notation (for example @2001:bd8:1234:1a00::/64@ ).
  , portRange :: Core.Maybe Types.PortRange
    -- ^ TCP or UDP protocols: The range of ports the rule applies to. Required if specifying protocol 6 (TCP) or 17 (UDP).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceNetworkAclEntry' value with any optional fields omitted.
mkReplaceNetworkAclEntry
    :: Core.Bool -- ^ 'egress'
    -> Types.NetworkAclId -- ^ 'networkAclId'
    -> Core.Text -- ^ 'protocol'
    -> Types.RuleAction -- ^ 'ruleAction'
    -> Core.Int -- ^ 'ruleNumber'
    -> ReplaceNetworkAclEntry
mkReplaceNetworkAclEntry egress networkAclId protocol ruleAction
  ruleNumber
  = ReplaceNetworkAclEntry'{egress, networkAclId, protocol,
                            ruleAction, ruleNumber, cidrBlock = Core.Nothing,
                            dryRun = Core.Nothing, icmpTypeCode = Core.Nothing,
                            ipv6CidrBlock = Core.Nothing, portRange = Core.Nothing}

-- | Indicates whether to replace the egress rule.
--
-- Default: If no value is specified, we replace the ingress rule.
--
-- /Note:/ Consider using 'egress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeEgress :: Lens.Lens' ReplaceNetworkAclEntry Core.Bool
rnaeEgress = Lens.field @"egress"
{-# INLINEABLE rnaeEgress #-}
{-# DEPRECATED egress "Use generic-lens or generic-optics with 'egress' instead"  #-}

-- | The ID of the ACL.
--
-- /Note:/ Consider using 'networkAclId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeNetworkAclId :: Lens.Lens' ReplaceNetworkAclEntry Types.NetworkAclId
rnaeNetworkAclId = Lens.field @"networkAclId"
{-# INLINEABLE rnaeNetworkAclId #-}
{-# DEPRECATED networkAclId "Use generic-lens or generic-optics with 'networkAclId' instead"  #-}

-- | The protocol number. A value of "-1" means all protocols. If you specify "-1" or a protocol number other than "6" (TCP), "17" (UDP), or "1" (ICMP), traffic on all ports is allowed, regardless of any ports or ICMP types or codes that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeProtocol :: Lens.Lens' ReplaceNetworkAclEntry Core.Text
rnaeProtocol = Lens.field @"protocol"
{-# INLINEABLE rnaeProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeRuleAction :: Lens.Lens' ReplaceNetworkAclEntry Types.RuleAction
rnaeRuleAction = Lens.field @"ruleAction"
{-# INLINEABLE rnaeRuleAction #-}
{-# DEPRECATED ruleAction "Use generic-lens or generic-optics with 'ruleAction' instead"  #-}

-- | The rule number of the entry to replace.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeRuleNumber :: Lens.Lens' ReplaceNetworkAclEntry Core.Int
rnaeRuleNumber = Lens.field @"ruleNumber"
{-# INLINEABLE rnaeRuleNumber #-}
{-# DEPRECATED ruleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead"  #-}

-- | The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ).
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeCidrBlock :: Lens.Lens' ReplaceNetworkAclEntry (Core.Maybe Core.Text)
rnaeCidrBlock = Lens.field @"cidrBlock"
{-# INLINEABLE rnaeCidrBlock #-}
{-# DEPRECATED cidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeDryRun :: Lens.Lens' ReplaceNetworkAclEntry (Core.Maybe Core.Bool)
rnaeDryRun = Lens.field @"dryRun"
{-# INLINEABLE rnaeDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
--
-- /Note:/ Consider using 'icmpTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeIcmpTypeCode :: Lens.Lens' ReplaceNetworkAclEntry (Core.Maybe Types.IcmpTypeCode)
rnaeIcmpTypeCode = Lens.field @"icmpTypeCode"
{-# INLINEABLE rnaeIcmpTypeCode #-}
{-# DEPRECATED icmpTypeCode "Use generic-lens or generic-optics with 'icmpTypeCode' instead"  #-}

-- | The IPv6 network range to allow or deny, in CIDR notation (for example @2001:bd8:1234:1a00::/64@ ).
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeIpv6CidrBlock :: Lens.Lens' ReplaceNetworkAclEntry (Core.Maybe Core.Text)
rnaeIpv6CidrBlock = Lens.field @"ipv6CidrBlock"
{-# INLINEABLE rnaeIpv6CidrBlock #-}
{-# DEPRECATED ipv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead"  #-}

-- | TCP or UDP protocols: The range of ports the rule applies to. Required if specifying protocol 6 (TCP) or 17 (UDP).
--
-- /Note:/ Consider using 'portRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaePortRange :: Lens.Lens' ReplaceNetworkAclEntry (Core.Maybe Types.PortRange)
rnaePortRange = Lens.field @"portRange"
{-# INLINEABLE rnaePortRange #-}
{-# DEPRECATED portRange "Use generic-lens or generic-optics with 'portRange' instead"  #-}

instance Core.ToQuery ReplaceNetworkAclEntry where
        toQuery ReplaceNetworkAclEntry{..}
          = Core.toQueryPair "Action" ("ReplaceNetworkAclEntry" :: Core.Text)
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

instance Core.ToHeaders ReplaceNetworkAclEntry where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReplaceNetworkAclEntry where
        type Rs ReplaceNetworkAclEntry = ReplaceNetworkAclEntryResponse
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
        parseResponse
          = Response.receiveNull ReplaceNetworkAclEntryResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReplaceNetworkAclEntryResponse' smart constructor.
data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceNetworkAclEntryResponse' value with any optional fields omitted.
mkReplaceNetworkAclEntryResponse
    :: ReplaceNetworkAclEntryResponse
mkReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse'
