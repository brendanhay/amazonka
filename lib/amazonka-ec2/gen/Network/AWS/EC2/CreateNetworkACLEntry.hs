{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkACLEntry
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
module Network.AWS.EC2.CreateNetworkACLEntry
  ( -- * Creating a request
    CreateNetworkACLEntry (..),
    mkCreateNetworkACLEntry,

    -- ** Request lenses
    cnaeIPv6CidrBlock,
    cnaeICMPTypeCode,
    cnaePortRange,
    cnaeCidrBlock,
    cnaeDryRun,
    cnaeEgress,
    cnaeNetworkACLId,
    cnaeProtocol,
    cnaeRuleAction,
    cnaeRuleNumber,

    -- * Destructuring the response
    CreateNetworkACLEntryResponse (..),
    mkCreateNetworkACLEntryResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateNetworkACLEntry' smart constructor.
data CreateNetworkACLEntry = CreateNetworkACLEntry'
  { ipv6CidrBlock ::
      Lude.Maybe Lude.Text,
    icmpTypeCode :: Lude.Maybe ICMPTypeCode,
    portRange :: Lude.Maybe PortRange,
    cidrBlock :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    egress :: Lude.Bool,
    networkACLId :: Lude.Text,
    protocol :: Lude.Text,
    ruleAction :: RuleAction,
    ruleNumber :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkACLEntry' with the minimum fields required to make a request.
--
-- * 'cidrBlock' - The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ). We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'egress' - Indicates whether this is an egress rule (rule is applied to traffic leaving the subnet).
-- * 'icmpTypeCode' - ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
-- * 'ipv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation (for example @2001:db8:1234:1a00::/64@ ).
-- * 'networkACLId' - The ID of the network ACL.
-- * 'portRange' - TCP or UDP protocols: The range of ports the rule applies to. Required if specifying protocol 6 (TCP) or 17 (UDP).
-- * 'protocol' - The protocol number. A value of "-1" means all protocols. If you specify "-1" or a protocol number other than "6" (TCP), "17" (UDP), or "1" (ICMP), traffic on all ports is allowed, regardless of any ports or ICMP types or codes that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
-- * 'ruleAction' - Indicates whether to allow or deny the traffic that matches the rule.
-- * 'ruleNumber' - The rule number for the entry (for example, 100). ACL entries are processed in ascending order by rule number.
--
-- Constraints: Positive integer from 1 to 32766. The range 32767 to 65535 is reserved for internal use.
mkCreateNetworkACLEntry ::
  -- | 'egress'
  Lude.Bool ->
  -- | 'networkACLId'
  Lude.Text ->
  -- | 'protocol'
  Lude.Text ->
  -- | 'ruleAction'
  RuleAction ->
  -- | 'ruleNumber'
  Lude.Int ->
  CreateNetworkACLEntry
mkCreateNetworkACLEntry
  pEgress_
  pNetworkACLId_
  pProtocol_
  pRuleAction_
  pRuleNumber_ =
    CreateNetworkACLEntry'
      { ipv6CidrBlock = Lude.Nothing,
        icmpTypeCode = Lude.Nothing,
        portRange = Lude.Nothing,
        cidrBlock = Lude.Nothing,
        dryRun = Lude.Nothing,
        egress = pEgress_,
        networkACLId = pNetworkACLId_,
        protocol = pProtocol_,
        ruleAction = pRuleAction_,
        ruleNumber = pRuleNumber_
      }

-- | The IPv6 network range to allow or deny, in CIDR notation (for example @2001:db8:1234:1a00::/64@ ).
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeIPv6CidrBlock :: Lens.Lens' CreateNetworkACLEntry (Lude.Maybe Lude.Text)
cnaeIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: CreateNetworkACLEntry -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: CreateNetworkACLEntry)
{-# DEPRECATED cnaeIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

-- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
--
-- /Note:/ Consider using 'icmpTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeICMPTypeCode :: Lens.Lens' CreateNetworkACLEntry (Lude.Maybe ICMPTypeCode)
cnaeICMPTypeCode = Lens.lens (icmpTypeCode :: CreateNetworkACLEntry -> Lude.Maybe ICMPTypeCode) (\s a -> s {icmpTypeCode = a} :: CreateNetworkACLEntry)
{-# DEPRECATED cnaeICMPTypeCode "Use generic-lens or generic-optics with 'icmpTypeCode' instead." #-}

-- | TCP or UDP protocols: The range of ports the rule applies to. Required if specifying protocol 6 (TCP) or 17 (UDP).
--
-- /Note:/ Consider using 'portRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaePortRange :: Lens.Lens' CreateNetworkACLEntry (Lude.Maybe PortRange)
cnaePortRange = Lens.lens (portRange :: CreateNetworkACLEntry -> Lude.Maybe PortRange) (\s a -> s {portRange = a} :: CreateNetworkACLEntry)
{-# DEPRECATED cnaePortRange "Use generic-lens or generic-optics with 'portRange' instead." #-}

-- | The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ). We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeCidrBlock :: Lens.Lens' CreateNetworkACLEntry (Lude.Maybe Lude.Text)
cnaeCidrBlock = Lens.lens (cidrBlock :: CreateNetworkACLEntry -> Lude.Maybe Lude.Text) (\s a -> s {cidrBlock = a} :: CreateNetworkACLEntry)
{-# DEPRECATED cnaeCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeDryRun :: Lens.Lens' CreateNetworkACLEntry (Lude.Maybe Lude.Bool)
cnaeDryRun = Lens.lens (dryRun :: CreateNetworkACLEntry -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateNetworkACLEntry)
{-# DEPRECATED cnaeDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Indicates whether this is an egress rule (rule is applied to traffic leaving the subnet).
--
-- /Note:/ Consider using 'egress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeEgress :: Lens.Lens' CreateNetworkACLEntry Lude.Bool
cnaeEgress = Lens.lens (egress :: CreateNetworkACLEntry -> Lude.Bool) (\s a -> s {egress = a} :: CreateNetworkACLEntry)
{-# DEPRECATED cnaeEgress "Use generic-lens or generic-optics with 'egress' instead." #-}

-- | The ID of the network ACL.
--
-- /Note:/ Consider using 'networkACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeNetworkACLId :: Lens.Lens' CreateNetworkACLEntry Lude.Text
cnaeNetworkACLId = Lens.lens (networkACLId :: CreateNetworkACLEntry -> Lude.Text) (\s a -> s {networkACLId = a} :: CreateNetworkACLEntry)
{-# DEPRECATED cnaeNetworkACLId "Use generic-lens or generic-optics with 'networkACLId' instead." #-}

-- | The protocol number. A value of "-1" means all protocols. If you specify "-1" or a protocol number other than "6" (TCP), "17" (UDP), or "1" (ICMP), traffic on all ports is allowed, regardless of any ports or ICMP types or codes that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeProtocol :: Lens.Lens' CreateNetworkACLEntry Lude.Text
cnaeProtocol = Lens.lens (protocol :: CreateNetworkACLEntry -> Lude.Text) (\s a -> s {protocol = a} :: CreateNetworkACLEntry)
{-# DEPRECATED cnaeProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeRuleAction :: Lens.Lens' CreateNetworkACLEntry RuleAction
cnaeRuleAction = Lens.lens (ruleAction :: CreateNetworkACLEntry -> RuleAction) (\s a -> s {ruleAction = a} :: CreateNetworkACLEntry)
{-# DEPRECATED cnaeRuleAction "Use generic-lens or generic-optics with 'ruleAction' instead." #-}

-- | The rule number for the entry (for example, 100). ACL entries are processed in ascending order by rule number.
--
-- Constraints: Positive integer from 1 to 32766. The range 32767 to 65535 is reserved for internal use.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaeRuleNumber :: Lens.Lens' CreateNetworkACLEntry Lude.Int
cnaeRuleNumber = Lens.lens (ruleNumber :: CreateNetworkACLEntry -> Lude.Int) (\s a -> s {ruleNumber = a} :: CreateNetworkACLEntry)
{-# DEPRECATED cnaeRuleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead." #-}

instance Lude.AWSRequest CreateNetworkACLEntry where
  type Rs CreateNetworkACLEntry = CreateNetworkACLEntryResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull CreateNetworkACLEntryResponse'

instance Lude.ToHeaders CreateNetworkACLEntry where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateNetworkACLEntry where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNetworkACLEntry where
  toQuery CreateNetworkACLEntry' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateNetworkAclEntry" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Ipv6CidrBlock" Lude.=: ipv6CidrBlock,
        "Icmp" Lude.=: icmpTypeCode,
        "PortRange" Lude.=: portRange,
        "CidrBlock" Lude.=: cidrBlock,
        "DryRun" Lude.=: dryRun,
        "Egress" Lude.=: egress,
        "NetworkAclId" Lude.=: networkACLId,
        "Protocol" Lude.=: protocol,
        "RuleAction" Lude.=: ruleAction,
        "RuleNumber" Lude.=: ruleNumber
      ]

-- | /See:/ 'mkCreateNetworkACLEntryResponse' smart constructor.
data CreateNetworkACLEntryResponse = CreateNetworkACLEntryResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkACLEntryResponse' with the minimum fields required to make a request.
mkCreateNetworkACLEntryResponse ::
  CreateNetworkACLEntryResponse
mkCreateNetworkACLEntryResponse = CreateNetworkACLEntryResponse'
