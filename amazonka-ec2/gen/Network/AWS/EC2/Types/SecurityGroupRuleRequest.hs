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
-- Module      : Network.AWS.EC2.Types.SecurityGroupRuleRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupRuleRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a security group rule.
--
-- You must specify exactly one of the following parameters, based on the
-- rule type:
--
-- -   CidrIpv4
--
-- -   CidrIpv6
--
-- -   PrefixListId
--
-- -   ReferencedGroupId
--
-- When you modify a rule, you cannot change the rule type. For example, if
-- the rule uses an IPv4 address range, you must use @CidrIpv4@ to specify
-- a new IPv4 address range.
--
-- /See:/ 'newSecurityGroupRuleRequest' smart constructor.
data SecurityGroupRuleRequest = SecurityGroupRuleRequest'
  { -- | The start of port range for the TCP and UDP protocols, or an
    -- ICMP\/ICMPv6 type. A value of -1 indicates all ICMP\/ICMPv6 types. If
    -- you specify all ICMP\/ICMPv6 types, you must specify all codes.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The IPv4 CIDR range. To specify a single IPv4 address, use the \/32
    -- prefix length.
    cidrIpv4 :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group that is referenced in the security group
    -- rule.
    referencedGroupId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR range. To specify a single IPv6 address, use the \/128
    -- prefix length.
    cidrIpv6 :: Prelude.Maybe Prelude.Text,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
    -- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
    --
    -- Use @-1@ to specify all protocols.
    ipProtocol :: Prelude.Maybe Prelude.Text,
    -- | The description of the security group rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
    -- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
    -- all ICMP\/ICMPv6 types, you must specify all codes.
    toPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroupRuleRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'securityGroupRuleRequest_fromPort' - The start of port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type. A value of -1 indicates all ICMP\/ICMPv6 types. If
-- you specify all ICMP\/ICMPv6 types, you must specify all codes.
--
-- 'cidrIpv4', 'securityGroupRuleRequest_cidrIpv4' - The IPv4 CIDR range. To specify a single IPv4 address, use the \/32
-- prefix length.
--
-- 'referencedGroupId', 'securityGroupRuleRequest_referencedGroupId' - The ID of the security group that is referenced in the security group
-- rule.
--
-- 'cidrIpv6', 'securityGroupRuleRequest_cidrIpv6' - The IPv6 CIDR range. To specify a single IPv6 address, use the \/128
-- prefix length.
--
-- 'prefixListId', 'securityGroupRuleRequest_prefixListId' - The ID of the prefix list.
--
-- 'ipProtocol', 'securityGroupRuleRequest_ipProtocol' - The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
--
-- Use @-1@ to specify all protocols.
--
-- 'description', 'securityGroupRuleRequest_description' - The description of the security group rule.
--
-- 'toPort', 'securityGroupRuleRequest_toPort' - The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
-- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
-- all ICMP\/ICMPv6 types, you must specify all codes.
newSecurityGroupRuleRequest ::
  SecurityGroupRuleRequest
newSecurityGroupRuleRequest =
  SecurityGroupRuleRequest'
    { fromPort =
        Prelude.Nothing,
      cidrIpv4 = Prelude.Nothing,
      referencedGroupId = Prelude.Nothing,
      cidrIpv6 = Prelude.Nothing,
      prefixListId = Prelude.Nothing,
      ipProtocol = Prelude.Nothing,
      description = Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type. A value of -1 indicates all ICMP\/ICMPv6 types. If
-- you specify all ICMP\/ICMPv6 types, you must specify all codes.
securityGroupRuleRequest_fromPort :: Lens.Lens' SecurityGroupRuleRequest (Prelude.Maybe Prelude.Int)
securityGroupRuleRequest_fromPort = Lens.lens (\SecurityGroupRuleRequest' {fromPort} -> fromPort) (\s@SecurityGroupRuleRequest' {} a -> s {fromPort = a} :: SecurityGroupRuleRequest)

-- | The IPv4 CIDR range. To specify a single IPv4 address, use the \/32
-- prefix length.
securityGroupRuleRequest_cidrIpv4 :: Lens.Lens' SecurityGroupRuleRequest (Prelude.Maybe Prelude.Text)
securityGroupRuleRequest_cidrIpv4 = Lens.lens (\SecurityGroupRuleRequest' {cidrIpv4} -> cidrIpv4) (\s@SecurityGroupRuleRequest' {} a -> s {cidrIpv4 = a} :: SecurityGroupRuleRequest)

-- | The ID of the security group that is referenced in the security group
-- rule.
securityGroupRuleRequest_referencedGroupId :: Lens.Lens' SecurityGroupRuleRequest (Prelude.Maybe Prelude.Text)
securityGroupRuleRequest_referencedGroupId = Lens.lens (\SecurityGroupRuleRequest' {referencedGroupId} -> referencedGroupId) (\s@SecurityGroupRuleRequest' {} a -> s {referencedGroupId = a} :: SecurityGroupRuleRequest)

-- | The IPv6 CIDR range. To specify a single IPv6 address, use the \/128
-- prefix length.
securityGroupRuleRequest_cidrIpv6 :: Lens.Lens' SecurityGroupRuleRequest (Prelude.Maybe Prelude.Text)
securityGroupRuleRequest_cidrIpv6 = Lens.lens (\SecurityGroupRuleRequest' {cidrIpv6} -> cidrIpv6) (\s@SecurityGroupRuleRequest' {} a -> s {cidrIpv6 = a} :: SecurityGroupRuleRequest)

-- | The ID of the prefix list.
securityGroupRuleRequest_prefixListId :: Lens.Lens' SecurityGroupRuleRequest (Prelude.Maybe Prelude.Text)
securityGroupRuleRequest_prefixListId = Lens.lens (\SecurityGroupRuleRequest' {prefixListId} -> prefixListId) (\s@SecurityGroupRuleRequest' {} a -> s {prefixListId = a} :: SecurityGroupRuleRequest)

-- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
--
-- Use @-1@ to specify all protocols.
securityGroupRuleRequest_ipProtocol :: Lens.Lens' SecurityGroupRuleRequest (Prelude.Maybe Prelude.Text)
securityGroupRuleRequest_ipProtocol = Lens.lens (\SecurityGroupRuleRequest' {ipProtocol} -> ipProtocol) (\s@SecurityGroupRuleRequest' {} a -> s {ipProtocol = a} :: SecurityGroupRuleRequest)

-- | The description of the security group rule.
securityGroupRuleRequest_description :: Lens.Lens' SecurityGroupRuleRequest (Prelude.Maybe Prelude.Text)
securityGroupRuleRequest_description = Lens.lens (\SecurityGroupRuleRequest' {description} -> description) (\s@SecurityGroupRuleRequest' {} a -> s {description = a} :: SecurityGroupRuleRequest)

-- | The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
-- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
-- all ICMP\/ICMPv6 types, you must specify all codes.
securityGroupRuleRequest_toPort :: Lens.Lens' SecurityGroupRuleRequest (Prelude.Maybe Prelude.Int)
securityGroupRuleRequest_toPort = Lens.lens (\SecurityGroupRuleRequest' {toPort} -> toPort) (\s@SecurityGroupRuleRequest' {} a -> s {toPort = a} :: SecurityGroupRuleRequest)

instance Prelude.Hashable SecurityGroupRuleRequest

instance Prelude.NFData SecurityGroupRuleRequest

instance Core.ToQuery SecurityGroupRuleRequest where
  toQuery SecurityGroupRuleRequest' {..} =
    Prelude.mconcat
      [ "FromPort" Core.=: fromPort,
        "CidrIpv4" Core.=: cidrIpv4,
        "ReferencedGroupId" Core.=: referencedGroupId,
        "CidrIpv6" Core.=: cidrIpv6,
        "PrefixListId" Core.=: prefixListId,
        "IpProtocol" Core.=: ipProtocol,
        "Description" Core.=: description,
        "ToPort" Core.=: toPort
      ]
