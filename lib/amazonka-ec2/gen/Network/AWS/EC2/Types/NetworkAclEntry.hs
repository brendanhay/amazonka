{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkAclEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkAclEntry
  ( NetworkAclEntry (..)
  -- * Smart constructor
  , mkNetworkAclEntry
  -- * Lenses
  , naeCidrBlock
  , naeEgress
  , naeIcmpTypeCode
  , naeIpv6CidrBlock
  , naePortRange
  , naeProtocol
  , naeRuleAction
  , naeRuleNumber
  ) where

import qualified Network.AWS.EC2.Types.IcmpTypeCode as Types
import qualified Network.AWS.EC2.Types.PortRange as Types
import qualified Network.AWS.EC2.Types.RuleAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an entry in a network ACL.
--
-- /See:/ 'mkNetworkAclEntry' smart constructor.
data NetworkAclEntry = NetworkAclEntry'
  { cidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 network range to allow or deny, in CIDR notation.
  , egress :: Core.Maybe Core.Bool
    -- ^ Indicates whether the rule is an egress rule (applied to traffic leaving the subnet).
  , icmpTypeCode :: Core.Maybe Types.IcmpTypeCode
    -- ^ ICMP protocol: The ICMP type and code.
  , ipv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 network range to allow or deny, in CIDR notation.
  , portRange :: Core.Maybe Types.PortRange
    -- ^ TCP or UDP protocols: The range of ports the rule applies to.
  , protocol :: Core.Maybe Core.Text
    -- ^ The protocol number. A value of "-1" means all protocols.
  , ruleAction :: Core.Maybe Types.RuleAction
    -- ^ Indicates whether to allow or deny the traffic that matches the rule.
  , ruleNumber :: Core.Maybe Core.Int
    -- ^ The rule number for the entry. ACL entries are processed in ascending order by rule number.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkAclEntry' value with any optional fields omitted.
mkNetworkAclEntry
    :: NetworkAclEntry
mkNetworkAclEntry
  = NetworkAclEntry'{cidrBlock = Core.Nothing, egress = Core.Nothing,
                     icmpTypeCode = Core.Nothing, ipv6CidrBlock = Core.Nothing,
                     portRange = Core.Nothing, protocol = Core.Nothing,
                     ruleAction = Core.Nothing, ruleNumber = Core.Nothing}

-- | The IPv4 network range to allow or deny, in CIDR notation.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeCidrBlock :: Lens.Lens' NetworkAclEntry (Core.Maybe Core.Text)
naeCidrBlock = Lens.field @"cidrBlock"
{-# INLINEABLE naeCidrBlock #-}
{-# DEPRECATED cidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead"  #-}

-- | Indicates whether the rule is an egress rule (applied to traffic leaving the subnet).
--
-- /Note:/ Consider using 'egress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeEgress :: Lens.Lens' NetworkAclEntry (Core.Maybe Core.Bool)
naeEgress = Lens.field @"egress"
{-# INLINEABLE naeEgress #-}
{-# DEPRECATED egress "Use generic-lens or generic-optics with 'egress' instead"  #-}

-- | ICMP protocol: The ICMP type and code.
--
-- /Note:/ Consider using 'icmpTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeIcmpTypeCode :: Lens.Lens' NetworkAclEntry (Core.Maybe Types.IcmpTypeCode)
naeIcmpTypeCode = Lens.field @"icmpTypeCode"
{-# INLINEABLE naeIcmpTypeCode #-}
{-# DEPRECATED icmpTypeCode "Use generic-lens or generic-optics with 'icmpTypeCode' instead"  #-}

-- | The IPv6 network range to allow or deny, in CIDR notation.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeIpv6CidrBlock :: Lens.Lens' NetworkAclEntry (Core.Maybe Core.Text)
naeIpv6CidrBlock = Lens.field @"ipv6CidrBlock"
{-# INLINEABLE naeIpv6CidrBlock #-}
{-# DEPRECATED ipv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead"  #-}

-- | TCP or UDP protocols: The range of ports the rule applies to.
--
-- /Note:/ Consider using 'portRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naePortRange :: Lens.Lens' NetworkAclEntry (Core.Maybe Types.PortRange)
naePortRange = Lens.field @"portRange"
{-# INLINEABLE naePortRange #-}
{-# DEPRECATED portRange "Use generic-lens or generic-optics with 'portRange' instead"  #-}

-- | The protocol number. A value of "-1" means all protocols.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeProtocol :: Lens.Lens' NetworkAclEntry (Core.Maybe Core.Text)
naeProtocol = Lens.field @"protocol"
{-# INLINEABLE naeProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | Indicates whether to allow or deny the traffic that matches the rule.
--
-- /Note:/ Consider using 'ruleAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeRuleAction :: Lens.Lens' NetworkAclEntry (Core.Maybe Types.RuleAction)
naeRuleAction = Lens.field @"ruleAction"
{-# INLINEABLE naeRuleAction #-}
{-# DEPRECATED ruleAction "Use generic-lens or generic-optics with 'ruleAction' instead"  #-}

-- | The rule number for the entry. ACL entries are processed in ascending order by rule number.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naeRuleNumber :: Lens.Lens' NetworkAclEntry (Core.Maybe Core.Int)
naeRuleNumber = Lens.field @"ruleNumber"
{-# INLINEABLE naeRuleNumber #-}
{-# DEPRECATED ruleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead"  #-}

instance Core.FromXML NetworkAclEntry where
        parseXML x
          = NetworkAclEntry' Core.<$>
              (x Core..@? "cidrBlock") Core.<*> x Core..@? "egress" Core.<*>
                x Core..@? "icmpTypeCode"
                Core.<*> x Core..@? "ipv6CidrBlock"
                Core.<*> x Core..@? "portRange"
                Core.<*> x Core..@? "protocol"
                Core.<*> x Core..@? "ruleAction"
                Core.<*> x Core..@? "ruleNumber"
