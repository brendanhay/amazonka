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
-- Module      : Network.AWS.EC2.Types.SecurityGroupRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupRule where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ReferencedSecurityGroup
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a security group rule.
--
-- /See:/ 'newSecurityGroupRule' smart constructor.
data SecurityGroupRule = SecurityGroupRule'
  { -- | The start of port range for the TCP and UDP protocols, or an
    -- ICMP\/ICMPv6 type. A value of -1 indicates all ICMP\/ICMPv6 types. If
    -- you specify all ICMP\/ICMPv6 types, you must specify all codes.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The IPv4 CIDR range.
    cidrIpv4 :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the security group rule is an outbound rule.
    isEgress :: Prelude.Maybe Prelude.Bool,
    -- | Describes the security group that is referenced in the rule.
    referencedGroupInfo :: Prelude.Maybe ReferencedSecurityGroup,
    -- | The ID of the Amazon Web Services account that owns the security group.
    groupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR range.
    cidrIpv6 :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
    -- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
    --
    -- Use @-1@ to specify all protocols.
    ipProtocol :: Prelude.Maybe Prelude.Text,
    -- | The tags applied to the security group rule.
    tags :: Prelude.Maybe [Tag],
    -- | The security group rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
    -- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
    -- all ICMP\/ICMPv6 types, you must specify all codes.
    toPort :: Prelude.Maybe Prelude.Int,
    -- | The ID of the security group rule.
    securityGroupRuleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroupRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'securityGroupRule_fromPort' - The start of port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type. A value of -1 indicates all ICMP\/ICMPv6 types. If
-- you specify all ICMP\/ICMPv6 types, you must specify all codes.
--
-- 'cidrIpv4', 'securityGroupRule_cidrIpv4' - The IPv4 CIDR range.
--
-- 'isEgress', 'securityGroupRule_isEgress' - Indicates whether the security group rule is an outbound rule.
--
-- 'referencedGroupInfo', 'securityGroupRule_referencedGroupInfo' - Describes the security group that is referenced in the rule.
--
-- 'groupOwnerId', 'securityGroupRule_groupOwnerId' - The ID of the Amazon Web Services account that owns the security group.
--
-- 'cidrIpv6', 'securityGroupRule_cidrIpv6' - The IPv6 CIDR range.
--
-- 'groupId', 'securityGroupRule_groupId' - The ID of the security group.
--
-- 'prefixListId', 'securityGroupRule_prefixListId' - The ID of the prefix list.
--
-- 'ipProtocol', 'securityGroupRule_ipProtocol' - The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
--
-- Use @-1@ to specify all protocols.
--
-- 'tags', 'securityGroupRule_tags' - The tags applied to the security group rule.
--
-- 'description', 'securityGroupRule_description' - The security group rule description.
--
-- 'toPort', 'securityGroupRule_toPort' - The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
-- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
-- all ICMP\/ICMPv6 types, you must specify all codes.
--
-- 'securityGroupRuleId', 'securityGroupRule_securityGroupRuleId' - The ID of the security group rule.
newSecurityGroupRule ::
  SecurityGroupRule
newSecurityGroupRule =
  SecurityGroupRule'
    { fromPort = Prelude.Nothing,
      cidrIpv4 = Prelude.Nothing,
      isEgress = Prelude.Nothing,
      referencedGroupInfo = Prelude.Nothing,
      groupOwnerId = Prelude.Nothing,
      cidrIpv6 = Prelude.Nothing,
      groupId = Prelude.Nothing,
      prefixListId = Prelude.Nothing,
      ipProtocol = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      toPort = Prelude.Nothing,
      securityGroupRuleId = Prelude.Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type. A value of -1 indicates all ICMP\/ICMPv6 types. If
-- you specify all ICMP\/ICMPv6 types, you must specify all codes.
securityGroupRule_fromPort :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Int)
securityGroupRule_fromPort = Lens.lens (\SecurityGroupRule' {fromPort} -> fromPort) (\s@SecurityGroupRule' {} a -> s {fromPort = a} :: SecurityGroupRule)

-- | The IPv4 CIDR range.
securityGroupRule_cidrIpv4 :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Text)
securityGroupRule_cidrIpv4 = Lens.lens (\SecurityGroupRule' {cidrIpv4} -> cidrIpv4) (\s@SecurityGroupRule' {} a -> s {cidrIpv4 = a} :: SecurityGroupRule)

-- | Indicates whether the security group rule is an outbound rule.
securityGroupRule_isEgress :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Bool)
securityGroupRule_isEgress = Lens.lens (\SecurityGroupRule' {isEgress} -> isEgress) (\s@SecurityGroupRule' {} a -> s {isEgress = a} :: SecurityGroupRule)

-- | Describes the security group that is referenced in the rule.
securityGroupRule_referencedGroupInfo :: Lens.Lens' SecurityGroupRule (Prelude.Maybe ReferencedSecurityGroup)
securityGroupRule_referencedGroupInfo = Lens.lens (\SecurityGroupRule' {referencedGroupInfo} -> referencedGroupInfo) (\s@SecurityGroupRule' {} a -> s {referencedGroupInfo = a} :: SecurityGroupRule)

-- | The ID of the Amazon Web Services account that owns the security group.
securityGroupRule_groupOwnerId :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Text)
securityGroupRule_groupOwnerId = Lens.lens (\SecurityGroupRule' {groupOwnerId} -> groupOwnerId) (\s@SecurityGroupRule' {} a -> s {groupOwnerId = a} :: SecurityGroupRule)

-- | The IPv6 CIDR range.
securityGroupRule_cidrIpv6 :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Text)
securityGroupRule_cidrIpv6 = Lens.lens (\SecurityGroupRule' {cidrIpv6} -> cidrIpv6) (\s@SecurityGroupRule' {} a -> s {cidrIpv6 = a} :: SecurityGroupRule)

-- | The ID of the security group.
securityGroupRule_groupId :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Text)
securityGroupRule_groupId = Lens.lens (\SecurityGroupRule' {groupId} -> groupId) (\s@SecurityGroupRule' {} a -> s {groupId = a} :: SecurityGroupRule)

-- | The ID of the prefix list.
securityGroupRule_prefixListId :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Text)
securityGroupRule_prefixListId = Lens.lens (\SecurityGroupRule' {prefixListId} -> prefixListId) (\s@SecurityGroupRule' {} a -> s {prefixListId = a} :: SecurityGroupRule)

-- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
--
-- Use @-1@ to specify all protocols.
securityGroupRule_ipProtocol :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Text)
securityGroupRule_ipProtocol = Lens.lens (\SecurityGroupRule' {ipProtocol} -> ipProtocol) (\s@SecurityGroupRule' {} a -> s {ipProtocol = a} :: SecurityGroupRule)

-- | The tags applied to the security group rule.
securityGroupRule_tags :: Lens.Lens' SecurityGroupRule (Prelude.Maybe [Tag])
securityGroupRule_tags = Lens.lens (\SecurityGroupRule' {tags} -> tags) (\s@SecurityGroupRule' {} a -> s {tags = a} :: SecurityGroupRule) Prelude.. Lens.mapping Lens._Coerce

-- | The security group rule description.
securityGroupRule_description :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Text)
securityGroupRule_description = Lens.lens (\SecurityGroupRule' {description} -> description) (\s@SecurityGroupRule' {} a -> s {description = a} :: SecurityGroupRule)

-- | The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
-- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
-- all ICMP\/ICMPv6 types, you must specify all codes.
securityGroupRule_toPort :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Int)
securityGroupRule_toPort = Lens.lens (\SecurityGroupRule' {toPort} -> toPort) (\s@SecurityGroupRule' {} a -> s {toPort = a} :: SecurityGroupRule)

-- | The ID of the security group rule.
securityGroupRule_securityGroupRuleId :: Lens.Lens' SecurityGroupRule (Prelude.Maybe Prelude.Text)
securityGroupRule_securityGroupRuleId = Lens.lens (\SecurityGroupRule' {securityGroupRuleId} -> securityGroupRuleId) (\s@SecurityGroupRule' {} a -> s {securityGroupRuleId = a} :: SecurityGroupRule)

instance Core.FromXML SecurityGroupRule where
  parseXML x =
    SecurityGroupRule'
      Prelude.<$> (x Core..@? "fromPort")
      Prelude.<*> (x Core..@? "cidrIpv4")
      Prelude.<*> (x Core..@? "isEgress")
      Prelude.<*> (x Core..@? "referencedGroupInfo")
      Prelude.<*> (x Core..@? "groupOwnerId")
      Prelude.<*> (x Core..@? "cidrIpv6")
      Prelude.<*> (x Core..@? "groupId")
      Prelude.<*> (x Core..@? "prefixListId")
      Prelude.<*> (x Core..@? "ipProtocol")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> (x Core..@? "toPort")
      Prelude.<*> (x Core..@? "securityGroupRuleId")

instance Prelude.Hashable SecurityGroupRule

instance Prelude.NFData SecurityGroupRule
