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
-- Module      : Amazonka.FMS.Types.SecurityGroupRuleDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.SecurityGroupRuleDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a set of permissions for a security group rule.
--
-- /See:/ 'newSecurityGroupRuleDescription' smart constructor.
data SecurityGroupRuleDescription = SecurityGroupRuleDescription'
  { -- | The start of the port range for the TCP and UDP protocols, or an
    -- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
    -- types.
    fromPort :: Prelude.Maybe Prelude.Natural,
    -- | The IPv4 ranges for the security group rule.
    iPV4Range :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 ranges for the security group rule.
    iPV6Range :: Prelude.Maybe Prelude.Text,
    -- | The ID of the prefix list for the security group rule.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The end of the port range for the TCP and UDP protocols, or an
    -- ICMP\/ICMPv6 code. A value of @-1@ indicates all ICMP\/ICMPv6 codes.
    toPort :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'iPV6Range', 'securityGroupRuleDescription_iPV6Range' - The IPv6 ranges for the security group rule.
--
-- 'prefixListId', 'securityGroupRuleDescription_prefixListId' - The ID of the prefix list for the security group rule.
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
        Prelude.Nothing,
      iPV4Range = Prelude.Nothing,
      iPV6Range = Prelude.Nothing,
      prefixListId = Prelude.Nothing,
      protocol = Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The start of the port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
-- types.
securityGroupRuleDescription_fromPort :: Lens.Lens' SecurityGroupRuleDescription (Prelude.Maybe Prelude.Natural)
securityGroupRuleDescription_fromPort = Lens.lens (\SecurityGroupRuleDescription' {fromPort} -> fromPort) (\s@SecurityGroupRuleDescription' {} a -> s {fromPort = a} :: SecurityGroupRuleDescription)

-- | The IPv4 ranges for the security group rule.
securityGroupRuleDescription_iPV4Range :: Lens.Lens' SecurityGroupRuleDescription (Prelude.Maybe Prelude.Text)
securityGroupRuleDescription_iPV4Range = Lens.lens (\SecurityGroupRuleDescription' {iPV4Range} -> iPV4Range) (\s@SecurityGroupRuleDescription' {} a -> s {iPV4Range = a} :: SecurityGroupRuleDescription)

-- | The IPv6 ranges for the security group rule.
securityGroupRuleDescription_iPV6Range :: Lens.Lens' SecurityGroupRuleDescription (Prelude.Maybe Prelude.Text)
securityGroupRuleDescription_iPV6Range = Lens.lens (\SecurityGroupRuleDescription' {iPV6Range} -> iPV6Range) (\s@SecurityGroupRuleDescription' {} a -> s {iPV6Range = a} :: SecurityGroupRuleDescription)

-- | The ID of the prefix list for the security group rule.
securityGroupRuleDescription_prefixListId :: Lens.Lens' SecurityGroupRuleDescription (Prelude.Maybe Prelude.Text)
securityGroupRuleDescription_prefixListId = Lens.lens (\SecurityGroupRuleDescription' {prefixListId} -> prefixListId) (\s@SecurityGroupRuleDescription' {} a -> s {prefixListId = a} :: SecurityGroupRuleDescription)

-- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number.
securityGroupRuleDescription_protocol :: Lens.Lens' SecurityGroupRuleDescription (Prelude.Maybe Prelude.Text)
securityGroupRuleDescription_protocol = Lens.lens (\SecurityGroupRuleDescription' {protocol} -> protocol) (\s@SecurityGroupRuleDescription' {} a -> s {protocol = a} :: SecurityGroupRuleDescription)

-- | The end of the port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 code. A value of @-1@ indicates all ICMP\/ICMPv6 codes.
securityGroupRuleDescription_toPort :: Lens.Lens' SecurityGroupRuleDescription (Prelude.Maybe Prelude.Natural)
securityGroupRuleDescription_toPort = Lens.lens (\SecurityGroupRuleDescription' {toPort} -> toPort) (\s@SecurityGroupRuleDescription' {} a -> s {toPort = a} :: SecurityGroupRuleDescription)

instance Data.FromJSON SecurityGroupRuleDescription where
  parseJSON =
    Data.withObject
      "SecurityGroupRuleDescription"
      ( \x ->
          SecurityGroupRuleDescription'
            Prelude.<$> (x Data..:? "FromPort")
            Prelude.<*> (x Data..:? "IPV4Range")
            Prelude.<*> (x Data..:? "IPV6Range")
            Prelude.<*> (x Data..:? "PrefixListId")
            Prelude.<*> (x Data..:? "Protocol")
            Prelude.<*> (x Data..:? "ToPort")
      )

instance
  Prelude.Hashable
    SecurityGroupRuleDescription
  where
  hashWithSalt _salt SecurityGroupRuleDescription' {..} =
    _salt `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` iPV4Range
      `Prelude.hashWithSalt` iPV6Range
      `Prelude.hashWithSalt` prefixListId
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` toPort

instance Prelude.NFData SecurityGroupRuleDescription where
  rnf SecurityGroupRuleDescription' {..} =
    Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf iPV4Range
      `Prelude.seq` Prelude.rnf iPV6Range
      `Prelude.seq` Prelude.rnf prefixListId
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf toPort
