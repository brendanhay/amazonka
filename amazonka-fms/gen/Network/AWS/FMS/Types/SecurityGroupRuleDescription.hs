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
-- Module      : Network.AWS.FMS.Types.SecurityGroupRuleDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.SecurityGroupRuleDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a set of permissions for a security group rule.
--
-- /See:/ 'newSecurityGroupRuleDescription' smart constructor.
data SecurityGroupRuleDescription = SecurityGroupRuleDescription'
  { -- | The start of the port range for the TCP and UDP protocols, or an
    -- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
    -- types.
    fromPort :: Core.Maybe Core.Natural,
    -- | The IPv4 ranges for the security group rule.
    iPV4Range :: Core.Maybe Core.Text,
    -- | The ID of the prefix list for the security group rule.
    prefixListId :: Core.Maybe Core.Text,
    -- | The IPv6 ranges for the security group rule.
    iPV6Range :: Core.Maybe Core.Text,
    -- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number.
    protocol :: Core.Maybe Core.Text,
    -- | The end of the port range for the TCP and UDP protocols, or an
    -- ICMP\/ICMPv6 code. A value of @-1@ indicates all ICMP\/ICMPv6 codes.
    toPort :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecurityGroupRuleDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'securityGroupRuleDescription_fromPort' - The start of the port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
-- types.
--
-- 'iPV4Range', 'securityGroupRuleDescription_iPV4Range' - The IPv4 ranges for the security group rule.
--
-- 'prefixListId', 'securityGroupRuleDescription_prefixListId' - The ID of the prefix list for the security group rule.
--
-- 'iPV6Range', 'securityGroupRuleDescription_iPV6Range' - The IPv6 ranges for the security group rule.
--
-- 'protocol', 'securityGroupRuleDescription_protocol' - The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number.
--
-- 'toPort', 'securityGroupRuleDescription_toPort' - The end of the port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 code. A value of @-1@ indicates all ICMP\/ICMPv6 codes.
newSecurityGroupRuleDescription ::
  SecurityGroupRuleDescription
newSecurityGroupRuleDescription =
  SecurityGroupRuleDescription'
    { fromPort =
        Core.Nothing,
      iPV4Range = Core.Nothing,
      prefixListId = Core.Nothing,
      iPV6Range = Core.Nothing,
      protocol = Core.Nothing,
      toPort = Core.Nothing
    }

-- | The start of the port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
-- types.
securityGroupRuleDescription_fromPort :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Core.Natural)
securityGroupRuleDescription_fromPort = Lens.lens (\SecurityGroupRuleDescription' {fromPort} -> fromPort) (\s@SecurityGroupRuleDescription' {} a -> s {fromPort = a} :: SecurityGroupRuleDescription)

-- | The IPv4 ranges for the security group rule.
securityGroupRuleDescription_iPV4Range :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Core.Text)
securityGroupRuleDescription_iPV4Range = Lens.lens (\SecurityGroupRuleDescription' {iPV4Range} -> iPV4Range) (\s@SecurityGroupRuleDescription' {} a -> s {iPV4Range = a} :: SecurityGroupRuleDescription)

-- | The ID of the prefix list for the security group rule.
securityGroupRuleDescription_prefixListId :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Core.Text)
securityGroupRuleDescription_prefixListId = Lens.lens (\SecurityGroupRuleDescription' {prefixListId} -> prefixListId) (\s@SecurityGroupRuleDescription' {} a -> s {prefixListId = a} :: SecurityGroupRuleDescription)

-- | The IPv6 ranges for the security group rule.
securityGroupRuleDescription_iPV6Range :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Core.Text)
securityGroupRuleDescription_iPV6Range = Lens.lens (\SecurityGroupRuleDescription' {iPV6Range} -> iPV6Range) (\s@SecurityGroupRuleDescription' {} a -> s {iPV6Range = a} :: SecurityGroupRuleDescription)

-- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number.
securityGroupRuleDescription_protocol :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Core.Text)
securityGroupRuleDescription_protocol = Lens.lens (\SecurityGroupRuleDescription' {protocol} -> protocol) (\s@SecurityGroupRuleDescription' {} a -> s {protocol = a} :: SecurityGroupRuleDescription)

-- | The end of the port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 code. A value of @-1@ indicates all ICMP\/ICMPv6 codes.
securityGroupRuleDescription_toPort :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Core.Natural)
securityGroupRuleDescription_toPort = Lens.lens (\SecurityGroupRuleDescription' {toPort} -> toPort) (\s@SecurityGroupRuleDescription' {} a -> s {toPort = a} :: SecurityGroupRuleDescription)

instance Core.FromJSON SecurityGroupRuleDescription where
  parseJSON =
    Core.withObject
      "SecurityGroupRuleDescription"
      ( \x ->
          SecurityGroupRuleDescription'
            Core.<$> (x Core..:? "FromPort")
            Core.<*> (x Core..:? "IPV4Range")
            Core.<*> (x Core..:? "PrefixListId")
            Core.<*> (x Core..:? "IPV6Range")
            Core.<*> (x Core..:? "Protocol")
            Core.<*> (x Core..:? "ToPort")
      )

instance Core.Hashable SecurityGroupRuleDescription

instance Core.NFData SecurityGroupRuleDescription
