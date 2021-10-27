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
-- Module      : Network.AWS.SecurityHub.Types.AwsEc2NetworkAclEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsEc2NetworkAclEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.IcmpTypeCode
import Network.AWS.SecurityHub.Types.PortRangeFromTo

-- | A rule for the network ACL. Each rule allows or denies access based on
-- the IP address, traffic direction, port, and protocol.
--
-- /See:/ 'newAwsEc2NetworkAclEntry' smart constructor.
data AwsEc2NetworkAclEntry = AwsEc2NetworkAclEntry'
  { -- | The IPV6 network range for which to deny or allow access.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The Internet Control Message Protocol (ICMP) type and code for which to
    -- deny or allow access.
    icmpTypeCode :: Prelude.Maybe IcmpTypeCode,
    -- | The rule number. The rules are processed in order by their number.
    ruleNumber :: Prelude.Maybe Prelude.Int,
    -- | Whether the rule is used to allow access or deny access.
    ruleAction :: Prelude.Maybe Prelude.Text,
    -- | The protocol that the rule applies to. To deny or allow access to all
    -- protocols, use the value -1.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | For TCP or UDP protocols, the range of ports that the rule applies to.
    portRange :: Prelude.Maybe PortRangeFromTo,
    -- | The IPV4 network range for which to deny or allow access.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Whether the rule is an egress rule. An egress rule is a rule that
    -- applies to traffic that leaves the subnet.
    egress :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2NetworkAclEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6CidrBlock', 'awsEc2NetworkAclEntry_ipv6CidrBlock' - The IPV6 network range for which to deny or allow access.
--
-- 'icmpTypeCode', 'awsEc2NetworkAclEntry_icmpTypeCode' - The Internet Control Message Protocol (ICMP) type and code for which to
-- deny or allow access.
--
-- 'ruleNumber', 'awsEc2NetworkAclEntry_ruleNumber' - The rule number. The rules are processed in order by their number.
--
-- 'ruleAction', 'awsEc2NetworkAclEntry_ruleAction' - Whether the rule is used to allow access or deny access.
--
-- 'protocol', 'awsEc2NetworkAclEntry_protocol' - The protocol that the rule applies to. To deny or allow access to all
-- protocols, use the value -1.
--
-- 'portRange', 'awsEc2NetworkAclEntry_portRange' - For TCP or UDP protocols, the range of ports that the rule applies to.
--
-- 'cidrBlock', 'awsEc2NetworkAclEntry_cidrBlock' - The IPV4 network range for which to deny or allow access.
--
-- 'egress', 'awsEc2NetworkAclEntry_egress' - Whether the rule is an egress rule. An egress rule is a rule that
-- applies to traffic that leaves the subnet.
newAwsEc2NetworkAclEntry ::
  AwsEc2NetworkAclEntry
newAwsEc2NetworkAclEntry =
  AwsEc2NetworkAclEntry'
    { ipv6CidrBlock =
        Prelude.Nothing,
      icmpTypeCode = Prelude.Nothing,
      ruleNumber = Prelude.Nothing,
      ruleAction = Prelude.Nothing,
      protocol = Prelude.Nothing,
      portRange = Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      egress = Prelude.Nothing
    }

-- | The IPV6 network range for which to deny or allow access.
awsEc2NetworkAclEntry_ipv6CidrBlock :: Lens.Lens' AwsEc2NetworkAclEntry (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclEntry_ipv6CidrBlock = Lens.lens (\AwsEc2NetworkAclEntry' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@AwsEc2NetworkAclEntry' {} a -> s {ipv6CidrBlock = a} :: AwsEc2NetworkAclEntry)

-- | The Internet Control Message Protocol (ICMP) type and code for which to
-- deny or allow access.
awsEc2NetworkAclEntry_icmpTypeCode :: Lens.Lens' AwsEc2NetworkAclEntry (Prelude.Maybe IcmpTypeCode)
awsEc2NetworkAclEntry_icmpTypeCode = Lens.lens (\AwsEc2NetworkAclEntry' {icmpTypeCode} -> icmpTypeCode) (\s@AwsEc2NetworkAclEntry' {} a -> s {icmpTypeCode = a} :: AwsEc2NetworkAclEntry)

-- | The rule number. The rules are processed in order by their number.
awsEc2NetworkAclEntry_ruleNumber :: Lens.Lens' AwsEc2NetworkAclEntry (Prelude.Maybe Prelude.Int)
awsEc2NetworkAclEntry_ruleNumber = Lens.lens (\AwsEc2NetworkAclEntry' {ruleNumber} -> ruleNumber) (\s@AwsEc2NetworkAclEntry' {} a -> s {ruleNumber = a} :: AwsEc2NetworkAclEntry)

-- | Whether the rule is used to allow access or deny access.
awsEc2NetworkAclEntry_ruleAction :: Lens.Lens' AwsEc2NetworkAclEntry (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclEntry_ruleAction = Lens.lens (\AwsEc2NetworkAclEntry' {ruleAction} -> ruleAction) (\s@AwsEc2NetworkAclEntry' {} a -> s {ruleAction = a} :: AwsEc2NetworkAclEntry)

-- | The protocol that the rule applies to. To deny or allow access to all
-- protocols, use the value -1.
awsEc2NetworkAclEntry_protocol :: Lens.Lens' AwsEc2NetworkAclEntry (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclEntry_protocol = Lens.lens (\AwsEc2NetworkAclEntry' {protocol} -> protocol) (\s@AwsEc2NetworkAclEntry' {} a -> s {protocol = a} :: AwsEc2NetworkAclEntry)

-- | For TCP or UDP protocols, the range of ports that the rule applies to.
awsEc2NetworkAclEntry_portRange :: Lens.Lens' AwsEc2NetworkAclEntry (Prelude.Maybe PortRangeFromTo)
awsEc2NetworkAclEntry_portRange = Lens.lens (\AwsEc2NetworkAclEntry' {portRange} -> portRange) (\s@AwsEc2NetworkAclEntry' {} a -> s {portRange = a} :: AwsEc2NetworkAclEntry)

-- | The IPV4 network range for which to deny or allow access.
awsEc2NetworkAclEntry_cidrBlock :: Lens.Lens' AwsEc2NetworkAclEntry (Prelude.Maybe Prelude.Text)
awsEc2NetworkAclEntry_cidrBlock = Lens.lens (\AwsEc2NetworkAclEntry' {cidrBlock} -> cidrBlock) (\s@AwsEc2NetworkAclEntry' {} a -> s {cidrBlock = a} :: AwsEc2NetworkAclEntry)

-- | Whether the rule is an egress rule. An egress rule is a rule that
-- applies to traffic that leaves the subnet.
awsEc2NetworkAclEntry_egress :: Lens.Lens' AwsEc2NetworkAclEntry (Prelude.Maybe Prelude.Bool)
awsEc2NetworkAclEntry_egress = Lens.lens (\AwsEc2NetworkAclEntry' {egress} -> egress) (\s@AwsEc2NetworkAclEntry' {} a -> s {egress = a} :: AwsEc2NetworkAclEntry)

instance Core.FromJSON AwsEc2NetworkAclEntry where
  parseJSON =
    Core.withObject
      "AwsEc2NetworkAclEntry"
      ( \x ->
          AwsEc2NetworkAclEntry'
            Prelude.<$> (x Core..:? "Ipv6CidrBlock")
            Prelude.<*> (x Core..:? "IcmpTypeCode")
            Prelude.<*> (x Core..:? "RuleNumber")
            Prelude.<*> (x Core..:? "RuleAction")
            Prelude.<*> (x Core..:? "Protocol")
            Prelude.<*> (x Core..:? "PortRange")
            Prelude.<*> (x Core..:? "CidrBlock")
            Prelude.<*> (x Core..:? "Egress")
      )

instance Prelude.Hashable AwsEc2NetworkAclEntry

instance Prelude.NFData AwsEc2NetworkAclEntry

instance Core.ToJSON AwsEc2NetworkAclEntry where
  toJSON AwsEc2NetworkAclEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Ipv6CidrBlock" Core..=) Prelude.<$> ipv6CidrBlock,
            ("IcmpTypeCode" Core..=) Prelude.<$> icmpTypeCode,
            ("RuleNumber" Core..=) Prelude.<$> ruleNumber,
            ("RuleAction" Core..=) Prelude.<$> ruleAction,
            ("Protocol" Core..=) Prelude.<$> protocol,
            ("PortRange" Core..=) Prelude.<$> portRange,
            ("CidrBlock" Core..=) Prelude.<$> cidrBlock,
            ("Egress" Core..=) Prelude.<$> egress
          ]
      )
