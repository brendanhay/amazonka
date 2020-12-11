{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceNetworkACLEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an entry (rule) in a network ACL. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.ReplaceNetworkACLEntry
  ( -- * Creating a request
    ReplaceNetworkACLEntry (..),
    mkReplaceNetworkACLEntry,

    -- ** Request lenses
    rnaeIPv6CidrBlock,
    rnaeICMPTypeCode,
    rnaePortRange,
    rnaeCidrBlock,
    rnaeDryRun,
    rnaeEgress,
    rnaeNetworkACLId,
    rnaeProtocol,
    rnaeRuleAction,
    rnaeRuleNumber,

    -- * Destructuring the response
    ReplaceNetworkACLEntryResponse (..),
    mkReplaceNetworkACLEntryResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReplaceNetworkACLEntry' smart constructor.
data ReplaceNetworkACLEntry = ReplaceNetworkACLEntry'
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

-- | Creates a value of 'ReplaceNetworkACLEntry' with the minimum fields required to make a request.
--
-- * 'cidrBlock' - The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ).
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'egress' - Indicates whether to replace the egress rule.
--
-- Default: If no value is specified, we replace the ingress rule.
-- * 'icmpTypeCode' - ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
-- * 'ipv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation (for example @2001:bd8:1234:1a00::/64@ ).
-- * 'networkACLId' - The ID of the ACL.
-- * 'portRange' - TCP or UDP protocols: The range of ports the rule applies to. Required if specifying protocol 6 (TCP) or 17 (UDP).
-- * 'protocol' - The protocol number. A value of "-1" means all protocols. If you specify "-1" or a protocol number other than "6" (TCP), "17" (UDP), or "1" (ICMP), traffic on all ports is allowed, regardless of any ports or ICMP types or codes that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
-- * 'ruleAction' - Indicates whether to allow or deny the traffic that matches the rule.
-- * 'ruleNumber' - The rule number of the entry to replace.
mkReplaceNetworkACLEntry ::
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
  ReplaceNetworkACLEntry
mkReplaceNetworkACLEntry
  pEgress_
  pNetworkACLId_
  pProtocol_
  pRuleAction_
  pRuleNumber_ =
    ReplaceNetworkACLEntry'
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

-- | The IPv6 network range to allow or deny, in CIDR notation (for example @2001:bd8:1234:1a00::/64@ ).
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeIPv6CidrBlock :: Lens.Lens' ReplaceNetworkACLEntry (Lude.Maybe Lude.Text)
rnaeIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: ReplaceNetworkACLEntry -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: ReplaceNetworkACLEntry)
{-# DEPRECATED rnaeIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

-- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
--
-- /Note:/ Consider using 'icmpTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeICMPTypeCode :: Lens.Lens' ReplaceNetworkACLEntry (Lude.Maybe ICMPTypeCode)
rnaeICMPTypeCode = Lens.lens (icmpTypeCode :: ReplaceNetworkACLEntry -> Lude.Maybe ICMPTypeCode) (\s a -> s {icmpTypeCode = a} :: ReplaceNetworkACLEntry)
{-# DEPRECATED rnaeICMPTypeCode "Use generic-lens or generic-optics with 'icmpTypeCode' instead." #-}

-- | TCP or UDP protocols: The range of ports the rule applies to. Required if specifying protocol 6 (TCP) or 17 (UDP).
--
-- /Note:/ Consider using 'portRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaePortRange :: Lens.Lens' ReplaceNetworkACLEntry (Lude.Maybe PortRange)
rnaePortRange = Lens.lens (portRange :: ReplaceNetworkACLEntry -> Lude.Maybe PortRange) (\s a -> s {portRange = a} :: ReplaceNetworkACLEntry)
{-# DEPRECATED rnaePortRange "Use generic-lens or generic-optics with 'portRange' instead." #-}

-- | The IPv4 network range to allow or deny, in CIDR notation (for example @172.16.0.0/24@ ).
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeCidrBlock :: Lens.Lens' ReplaceNetworkACLEntry (Lude.Maybe Lude.Text)
rnaeCidrBlock = Lens.lens (cidrBlock :: ReplaceNetworkACLEntry -> Lude.Maybe Lude.Text) (\s a -> s {cidrBlock = a} :: ReplaceNetworkACLEntry)
{-# DEPRECATED rnaeCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeDryRun :: Lens.Lens' ReplaceNetworkACLEntry (Lude.Maybe Lude.Bool)
rnaeDryRun = Lens.lens (dryRun :: ReplaceNetworkACLEntry -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ReplaceNetworkACLEntry)
{-# DEPRECATED rnaeDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Indicates whether to replace the egress rule.
--
-- Default: If no value is specified, we replace the ingress rule.
--
-- /Note:/ Consider using 'egress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeEgress :: Lens.Lens' ReplaceNetworkACLEntry Lude.Bool
rnaeEgress = Lens.lens (egress :: ReplaceNetworkACLEntry -> Lude.Bool) (\s a -> s {egress = a} :: ReplaceNetworkACLEntry)
{-# DEPRECATED rnaeEgress "Use generic-lens or generic-optics with 'egress' instead." #-}

-- | The ID of the ACL.
--
-- /Note:/ Consider using 'networkACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeNetworkACLId :: Lens.Lens' ReplaceNetworkACLEntry Lude.Text
rnaeNetworkACLId = Lens.lens (networkACLId :: ReplaceNetworkACLEntry -> Lude.Text) (\s a -> s {networkACLId = a} :: ReplaceNetworkACLEntry)
{-# DEPRECATED rnaeNetworkACLId "Use generic-lens or generic-optics with 'networkACLId' instead." #-}

-- | The protocol number. A value of "-1" means all protocols. If you specify "-1" or a protocol number other than "6" (TCP), "17" (UDP), or "1" (ICMP), traffic on all ports is allowed, regardless of any ports or ICMP types or codes that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv4 CIDR block, traffic for all ICMP types and codes allowed, regardless of any that you specify. If you specify protocol "58" (ICMPv6) and specify an IPv6 CIDR block, you must specify an ICMP type and code.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeProtocol :: Lens.Lens' ReplaceNetworkACLEntry Lude.Text
rnaeProtocol = Lens.lens (protocol :: ReplaceNetworkACLEntry -> Lude.Text) (\s a -> s {protocol = a} :: ReplaceNetworkACLEntry)
{-# DEPRECATED rnaeProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeRuleAction :: Lens.Lens' ReplaceNetworkACLEntry RuleAction
rnaeRuleAction = Lens.lens (ruleAction :: ReplaceNetworkACLEntry -> RuleAction) (\s a -> s {ruleAction = a} :: ReplaceNetworkACLEntry)
{-# DEPRECATED rnaeRuleAction "Use generic-lens or generic-optics with 'ruleAction' instead." #-}

-- | The rule number of the entry to replace.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaeRuleNumber :: Lens.Lens' ReplaceNetworkACLEntry Lude.Int
rnaeRuleNumber = Lens.lens (ruleNumber :: ReplaceNetworkACLEntry -> Lude.Int) (\s a -> s {ruleNumber = a} :: ReplaceNetworkACLEntry)
{-# DEPRECATED rnaeRuleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead." #-}

instance Lude.AWSRequest ReplaceNetworkACLEntry where
  type Rs ReplaceNetworkACLEntry = ReplaceNetworkACLEntryResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ReplaceNetworkACLEntryResponse'

instance Lude.ToHeaders ReplaceNetworkACLEntry where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReplaceNetworkACLEntry where
  toPath = Lude.const "/"

instance Lude.ToQuery ReplaceNetworkACLEntry where
  toQuery ReplaceNetworkACLEntry' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ReplaceNetworkAclEntry" :: Lude.ByteString),
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

-- | /See:/ 'mkReplaceNetworkACLEntryResponse' smart constructor.
data ReplaceNetworkACLEntryResponse = ReplaceNetworkACLEntryResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceNetworkACLEntryResponse' with the minimum fields required to make a request.
mkReplaceNetworkACLEntryResponse ::
  ReplaceNetworkACLEntryResponse
mkReplaceNetworkACLEntryResponse = ReplaceNetworkACLEntryResponse'
