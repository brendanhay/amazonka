-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkACLEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkACLEntry
  ( NetworkACLEntry (..),

    -- * Smart constructor
    mkNetworkACLEntry,

    -- * Lenses
    naeIPv6CidrBlock,
    naeICMPTypeCode,
    naeRuleNumber,
    naeRuleAction,
    naeProtocol,
    naePortRange,
    naeCidrBlock,
    naeEgress,
  )
where

import Network.AWS.EC2.Types.ICMPTypeCode
import Network.AWS.EC2.Types.PortRange
import Network.AWS.EC2.Types.RuleAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an entry in a network ACL.
--
-- /See:/ 'mkNetworkACLEntry' smart constructor.
data NetworkACLEntry = NetworkACLEntry'
  { ipv6CidrBlock ::
      Lude.Maybe Lude.Text,
    icmpTypeCode :: Lude.Maybe ICMPTypeCode,
    ruleNumber :: Lude.Maybe Lude.Int,
    ruleAction :: Lude.Maybe RuleAction,
    protocol :: Lude.Maybe Lude.Text,
    portRange :: Lude.Maybe PortRange,
    cidrBlock :: Lude.Maybe Lude.Text,
    egress :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkACLEntry' with the minimum fields required to make a request.
--
-- * 'cidrBlock' - The IPv4 network range to allow or deny, in CIDR notation.
-- * 'egress' - Indicates whether the rule is an egress rule (applied to traffic leaving the subnet).
-- * 'icmpTypeCode' - ICMP protocol: The ICMP type and code.
-- * 'ipv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation.
-- * 'portRange' - TCP or UDP protocols: The range of ports the rule applies to.
-- * 'protocol' - The protocol number. A value of "-1" means all protocols.
-- * 'ruleAction' - Indicates whether to allow or deny the traffic that matches the rule.
-- * 'ruleNumber' - The rule number for the entry. ACL entries are processed in ascending order by rule number.
mkNetworkACLEntry ::
  NetworkACLEntry
mkNetworkACLEntry =
  NetworkACLEntry'
    { ipv6CidrBlock = Lude.Nothing,
      icmpTypeCode = Lude.Nothing,
      ruleNumber = Lude.Nothing,
      ruleAction = Lude.Nothing,
      protocol = Lude.Nothing,
      portRange = Lude.Nothing,
      cidrBlock = Lude.Nothing,
      egress = Lude.Nothing
    }

-- | The IPv6 network range to allow or deny, in CIDR notation.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeIPv6CidrBlock :: Lens.Lens' NetworkACLEntry (Lude.Maybe Lude.Text)
naeIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: NetworkACLEntry -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: NetworkACLEntry)
{-# DEPRECATED naeIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

-- | ICMP protocol: The ICMP type and code.
--
-- /Note:/ Consider using 'icmpTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeICMPTypeCode :: Lens.Lens' NetworkACLEntry (Lude.Maybe ICMPTypeCode)
naeICMPTypeCode = Lens.lens (icmpTypeCode :: NetworkACLEntry -> Lude.Maybe ICMPTypeCode) (\s a -> s {icmpTypeCode = a} :: NetworkACLEntry)
{-# DEPRECATED naeICMPTypeCode "Use generic-lens or generic-optics with 'icmpTypeCode' instead." #-}

-- | The rule number for the entry. ACL entries are processed in ascending order by rule number.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeRuleNumber :: Lens.Lens' NetworkACLEntry (Lude.Maybe Lude.Int)
naeRuleNumber = Lens.lens (ruleNumber :: NetworkACLEntry -> Lude.Maybe Lude.Int) (\s a -> s {ruleNumber = a} :: NetworkACLEntry)
{-# DEPRECATED naeRuleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead." #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeRuleAction :: Lens.Lens' NetworkACLEntry (Lude.Maybe RuleAction)
naeRuleAction = Lens.lens (ruleAction :: NetworkACLEntry -> Lude.Maybe RuleAction) (\s a -> s {ruleAction = a} :: NetworkACLEntry)
{-# DEPRECATED naeRuleAction "Use generic-lens or generic-optics with 'ruleAction' instead." #-}

-- | The protocol number. A value of "-1" means all protocols.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeProtocol :: Lens.Lens' NetworkACLEntry (Lude.Maybe Lude.Text)
naeProtocol = Lens.lens (protocol :: NetworkACLEntry -> Lude.Maybe Lude.Text) (\s a -> s {protocol = a} :: NetworkACLEntry)
{-# DEPRECATED naeProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | TCP or UDP protocols: The range of ports the rule applies to.
--
-- /Note:/ Consider using 'portRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naePortRange :: Lens.Lens' NetworkACLEntry (Lude.Maybe PortRange)
naePortRange = Lens.lens (portRange :: NetworkACLEntry -> Lude.Maybe PortRange) (\s a -> s {portRange = a} :: NetworkACLEntry)
{-# DEPRECATED naePortRange "Use generic-lens or generic-optics with 'portRange' instead." #-}

-- | The IPv4 network range to allow or deny, in CIDR notation.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeCidrBlock :: Lens.Lens' NetworkACLEntry (Lude.Maybe Lude.Text)
naeCidrBlock = Lens.lens (cidrBlock :: NetworkACLEntry -> Lude.Maybe Lude.Text) (\s a -> s {cidrBlock = a} :: NetworkACLEntry)
{-# DEPRECATED naeCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | Indicates whether the rule is an egress rule (applied to traffic leaving the subnet).
--
-- /Note:/ Consider using 'egress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeEgress :: Lens.Lens' NetworkACLEntry (Lude.Maybe Lude.Bool)
naeEgress = Lens.lens (egress :: NetworkACLEntry -> Lude.Maybe Lude.Bool) (\s a -> s {egress = a} :: NetworkACLEntry)
{-# DEPRECATED naeEgress "Use generic-lens or generic-optics with 'egress' instead." #-}

instance Lude.FromXML NetworkACLEntry where
  parseXML x =
    NetworkACLEntry'
      Lude.<$> (x Lude..@? "ipv6CidrBlock")
      Lude.<*> (x Lude..@? "icmpTypeCode")
      Lude.<*> (x Lude..@? "ruleNumber")
      Lude.<*> (x Lude..@? "ruleAction")
      Lude.<*> (x Lude..@? "protocol")
      Lude.<*> (x Lude..@? "portRange")
      Lude.<*> (x Lude..@? "cidrBlock")
      Lude.<*> (x Lude..@? "egress")
