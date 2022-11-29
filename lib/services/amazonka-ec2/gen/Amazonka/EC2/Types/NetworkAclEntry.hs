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
-- Module      : Amazonka.EC2.Types.NetworkAclEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkAclEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IcmpTypeCode
import Amazonka.EC2.Types.PortRange
import Amazonka.EC2.Types.RuleAction
import qualified Amazonka.Prelude as Prelude

-- | Describes an entry in a network ACL.
--
-- /See:/ 'newNetworkAclEntry' smart constructor.
data NetworkAclEntry = NetworkAclEntry'
  { -- | ICMP protocol: The ICMP type and code.
    icmpTypeCode :: Prelude.Maybe IcmpTypeCode,
    -- | Indicates whether the rule is an egress rule (applied to traffic leaving
    -- the subnet).
    egress :: Prelude.Maybe Prelude.Bool,
    -- | TCP or UDP protocols: The range of ports the rule applies to.
    portRange :: Prelude.Maybe PortRange,
    -- | The rule number for the entry. ACL entries are processed in ascending
    -- order by rule number.
    ruleNumber :: Prelude.Maybe Prelude.Int,
    -- | The IPv4 network range to allow or deny, in CIDR notation.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to allow or deny the traffic that matches the rule.
    ruleAction :: Prelude.Maybe RuleAction,
    -- | The protocol number. A value of \"-1\" means all protocols.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 network range to allow or deny, in CIDR notation.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkAclEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'icmpTypeCode', 'networkAclEntry_icmpTypeCode' - ICMP protocol: The ICMP type and code.
--
-- 'egress', 'networkAclEntry_egress' - Indicates whether the rule is an egress rule (applied to traffic leaving
-- the subnet).
--
-- 'portRange', 'networkAclEntry_portRange' - TCP or UDP protocols: The range of ports the rule applies to.
--
-- 'ruleNumber', 'networkAclEntry_ruleNumber' - The rule number for the entry. ACL entries are processed in ascending
-- order by rule number.
--
-- 'cidrBlock', 'networkAclEntry_cidrBlock' - The IPv4 network range to allow or deny, in CIDR notation.
--
-- 'ruleAction', 'networkAclEntry_ruleAction' - Indicates whether to allow or deny the traffic that matches the rule.
--
-- 'protocol', 'networkAclEntry_protocol' - The protocol number. A value of \"-1\" means all protocols.
--
-- 'ipv6CidrBlock', 'networkAclEntry_ipv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation.
newNetworkAclEntry ::
  NetworkAclEntry
newNetworkAclEntry =
  NetworkAclEntry'
    { icmpTypeCode = Prelude.Nothing,
      egress = Prelude.Nothing,
      portRange = Prelude.Nothing,
      ruleNumber = Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      ruleAction = Prelude.Nothing,
      protocol = Prelude.Nothing,
      ipv6CidrBlock = Prelude.Nothing
    }

-- | ICMP protocol: The ICMP type and code.
networkAclEntry_icmpTypeCode :: Lens.Lens' NetworkAclEntry (Prelude.Maybe IcmpTypeCode)
networkAclEntry_icmpTypeCode = Lens.lens (\NetworkAclEntry' {icmpTypeCode} -> icmpTypeCode) (\s@NetworkAclEntry' {} a -> s {icmpTypeCode = a} :: NetworkAclEntry)

-- | Indicates whether the rule is an egress rule (applied to traffic leaving
-- the subnet).
networkAclEntry_egress :: Lens.Lens' NetworkAclEntry (Prelude.Maybe Prelude.Bool)
networkAclEntry_egress = Lens.lens (\NetworkAclEntry' {egress} -> egress) (\s@NetworkAclEntry' {} a -> s {egress = a} :: NetworkAclEntry)

-- | TCP or UDP protocols: The range of ports the rule applies to.
networkAclEntry_portRange :: Lens.Lens' NetworkAclEntry (Prelude.Maybe PortRange)
networkAclEntry_portRange = Lens.lens (\NetworkAclEntry' {portRange} -> portRange) (\s@NetworkAclEntry' {} a -> s {portRange = a} :: NetworkAclEntry)

-- | The rule number for the entry. ACL entries are processed in ascending
-- order by rule number.
networkAclEntry_ruleNumber :: Lens.Lens' NetworkAclEntry (Prelude.Maybe Prelude.Int)
networkAclEntry_ruleNumber = Lens.lens (\NetworkAclEntry' {ruleNumber} -> ruleNumber) (\s@NetworkAclEntry' {} a -> s {ruleNumber = a} :: NetworkAclEntry)

-- | The IPv4 network range to allow or deny, in CIDR notation.
networkAclEntry_cidrBlock :: Lens.Lens' NetworkAclEntry (Prelude.Maybe Prelude.Text)
networkAclEntry_cidrBlock = Lens.lens (\NetworkAclEntry' {cidrBlock} -> cidrBlock) (\s@NetworkAclEntry' {} a -> s {cidrBlock = a} :: NetworkAclEntry)

-- | Indicates whether to allow or deny the traffic that matches the rule.
networkAclEntry_ruleAction :: Lens.Lens' NetworkAclEntry (Prelude.Maybe RuleAction)
networkAclEntry_ruleAction = Lens.lens (\NetworkAclEntry' {ruleAction} -> ruleAction) (\s@NetworkAclEntry' {} a -> s {ruleAction = a} :: NetworkAclEntry)

-- | The protocol number. A value of \"-1\" means all protocols.
networkAclEntry_protocol :: Lens.Lens' NetworkAclEntry (Prelude.Maybe Prelude.Text)
networkAclEntry_protocol = Lens.lens (\NetworkAclEntry' {protocol} -> protocol) (\s@NetworkAclEntry' {} a -> s {protocol = a} :: NetworkAclEntry)

-- | The IPv6 network range to allow or deny, in CIDR notation.
networkAclEntry_ipv6CidrBlock :: Lens.Lens' NetworkAclEntry (Prelude.Maybe Prelude.Text)
networkAclEntry_ipv6CidrBlock = Lens.lens (\NetworkAclEntry' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@NetworkAclEntry' {} a -> s {ipv6CidrBlock = a} :: NetworkAclEntry)

instance Core.FromXML NetworkAclEntry where
  parseXML x =
    NetworkAclEntry'
      Prelude.<$> (x Core..@? "icmpTypeCode")
      Prelude.<*> (x Core..@? "egress")
      Prelude.<*> (x Core..@? "portRange")
      Prelude.<*> (x Core..@? "ruleNumber")
      Prelude.<*> (x Core..@? "cidrBlock")
      Prelude.<*> (x Core..@? "ruleAction")
      Prelude.<*> (x Core..@? "protocol")
      Prelude.<*> (x Core..@? "ipv6CidrBlock")

instance Prelude.Hashable NetworkAclEntry where
  hashWithSalt _salt NetworkAclEntry' {..} =
    _salt `Prelude.hashWithSalt` icmpTypeCode
      `Prelude.hashWithSalt` egress
      `Prelude.hashWithSalt` portRange
      `Prelude.hashWithSalt` ruleNumber
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` ruleAction
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` ipv6CidrBlock

instance Prelude.NFData NetworkAclEntry where
  rnf NetworkAclEntry' {..} =
    Prelude.rnf icmpTypeCode
      `Prelude.seq` Prelude.rnf egress
      `Prelude.seq` Prelude.rnf portRange
      `Prelude.seq` Prelude.rnf ruleNumber
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf ruleAction
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf ipv6CidrBlock
