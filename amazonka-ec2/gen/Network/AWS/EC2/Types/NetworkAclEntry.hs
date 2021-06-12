{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkAclEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkAclEntry where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IcmpTypeCode
import Network.AWS.EC2.Types.PortRange
import Network.AWS.EC2.Types.RuleAction
import qualified Network.AWS.Lens as Lens

-- | Describes an entry in a network ACL.
--
-- /See:/ 'newNetworkAclEntry' smart constructor.
data NetworkAclEntry = NetworkAclEntry'
  { -- | TCP or UDP protocols: The range of ports the rule applies to.
    portRange :: Core.Maybe PortRange,
    -- | Indicates whether to allow or deny the traffic that matches the rule.
    ruleAction :: Core.Maybe RuleAction,
    -- | Indicates whether the rule is an egress rule (applied to traffic leaving
    -- the subnet).
    egress :: Core.Maybe Core.Bool,
    -- | ICMP protocol: The ICMP type and code.
    icmpTypeCode :: Core.Maybe IcmpTypeCode,
    -- | The IPv6 network range to allow or deny, in CIDR notation.
    ipv6CidrBlock :: Core.Maybe Core.Text,
    -- | The protocol number. A value of \"-1\" means all protocols.
    protocol :: Core.Maybe Core.Text,
    -- | The rule number for the entry. ACL entries are processed in ascending
    -- order by rule number.
    ruleNumber :: Core.Maybe Core.Int,
    -- | The IPv4 network range to allow or deny, in CIDR notation.
    cidrBlock :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkAclEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portRange', 'networkAclEntry_portRange' - TCP or UDP protocols: The range of ports the rule applies to.
--
-- 'ruleAction', 'networkAclEntry_ruleAction' - Indicates whether to allow or deny the traffic that matches the rule.
--
-- 'egress', 'networkAclEntry_egress' - Indicates whether the rule is an egress rule (applied to traffic leaving
-- the subnet).
--
-- 'icmpTypeCode', 'networkAclEntry_icmpTypeCode' - ICMP protocol: The ICMP type and code.
--
-- 'ipv6CidrBlock', 'networkAclEntry_ipv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation.
--
-- 'protocol', 'networkAclEntry_protocol' - The protocol number. A value of \"-1\" means all protocols.
--
-- 'ruleNumber', 'networkAclEntry_ruleNumber' - The rule number for the entry. ACL entries are processed in ascending
-- order by rule number.
--
-- 'cidrBlock', 'networkAclEntry_cidrBlock' - The IPv4 network range to allow or deny, in CIDR notation.
newNetworkAclEntry ::
  NetworkAclEntry
newNetworkAclEntry =
  NetworkAclEntry'
    { portRange = Core.Nothing,
      ruleAction = Core.Nothing,
      egress = Core.Nothing,
      icmpTypeCode = Core.Nothing,
      ipv6CidrBlock = Core.Nothing,
      protocol = Core.Nothing,
      ruleNumber = Core.Nothing,
      cidrBlock = Core.Nothing
    }

-- | TCP or UDP protocols: The range of ports the rule applies to.
networkAclEntry_portRange :: Lens.Lens' NetworkAclEntry (Core.Maybe PortRange)
networkAclEntry_portRange = Lens.lens (\NetworkAclEntry' {portRange} -> portRange) (\s@NetworkAclEntry' {} a -> s {portRange = a} :: NetworkAclEntry)

-- | Indicates whether to allow or deny the traffic that matches the rule.
networkAclEntry_ruleAction :: Lens.Lens' NetworkAclEntry (Core.Maybe RuleAction)
networkAclEntry_ruleAction = Lens.lens (\NetworkAclEntry' {ruleAction} -> ruleAction) (\s@NetworkAclEntry' {} a -> s {ruleAction = a} :: NetworkAclEntry)

-- | Indicates whether the rule is an egress rule (applied to traffic leaving
-- the subnet).
networkAclEntry_egress :: Lens.Lens' NetworkAclEntry (Core.Maybe Core.Bool)
networkAclEntry_egress = Lens.lens (\NetworkAclEntry' {egress} -> egress) (\s@NetworkAclEntry' {} a -> s {egress = a} :: NetworkAclEntry)

-- | ICMP protocol: The ICMP type and code.
networkAclEntry_icmpTypeCode :: Lens.Lens' NetworkAclEntry (Core.Maybe IcmpTypeCode)
networkAclEntry_icmpTypeCode = Lens.lens (\NetworkAclEntry' {icmpTypeCode} -> icmpTypeCode) (\s@NetworkAclEntry' {} a -> s {icmpTypeCode = a} :: NetworkAclEntry)

-- | The IPv6 network range to allow or deny, in CIDR notation.
networkAclEntry_ipv6CidrBlock :: Lens.Lens' NetworkAclEntry (Core.Maybe Core.Text)
networkAclEntry_ipv6CidrBlock = Lens.lens (\NetworkAclEntry' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@NetworkAclEntry' {} a -> s {ipv6CidrBlock = a} :: NetworkAclEntry)

-- | The protocol number. A value of \"-1\" means all protocols.
networkAclEntry_protocol :: Lens.Lens' NetworkAclEntry (Core.Maybe Core.Text)
networkAclEntry_protocol = Lens.lens (\NetworkAclEntry' {protocol} -> protocol) (\s@NetworkAclEntry' {} a -> s {protocol = a} :: NetworkAclEntry)

-- | The rule number for the entry. ACL entries are processed in ascending
-- order by rule number.
networkAclEntry_ruleNumber :: Lens.Lens' NetworkAclEntry (Core.Maybe Core.Int)
networkAclEntry_ruleNumber = Lens.lens (\NetworkAclEntry' {ruleNumber} -> ruleNumber) (\s@NetworkAclEntry' {} a -> s {ruleNumber = a} :: NetworkAclEntry)

-- | The IPv4 network range to allow or deny, in CIDR notation.
networkAclEntry_cidrBlock :: Lens.Lens' NetworkAclEntry (Core.Maybe Core.Text)
networkAclEntry_cidrBlock = Lens.lens (\NetworkAclEntry' {cidrBlock} -> cidrBlock) (\s@NetworkAclEntry' {} a -> s {cidrBlock = a} :: NetworkAclEntry)

instance Core.FromXML NetworkAclEntry where
  parseXML x =
    NetworkAclEntry'
      Core.<$> (x Core..@? "portRange")
      Core.<*> (x Core..@? "ruleAction")
      Core.<*> (x Core..@? "egress")
      Core.<*> (x Core..@? "icmpTypeCode")
      Core.<*> (x Core..@? "ipv6CidrBlock")
      Core.<*> (x Core..@? "protocol")
      Core.<*> (x Core..@? "ruleNumber")
      Core.<*> (x Core..@? "cidrBlock")

instance Core.Hashable NetworkAclEntry

instance Core.NFData NetworkAclEntry
