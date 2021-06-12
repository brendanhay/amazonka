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
-- Module      : Network.AWS.EC2.Types.IpPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IpPermission where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IpRange
import Network.AWS.EC2.Types.Ipv6Range
import Network.AWS.EC2.Types.PrefixListId
import Network.AWS.EC2.Types.UserIdGroupPair
import qualified Network.AWS.Lens as Lens

-- | Describes a set of permissions for a security group rule.
--
-- /See:/ 'newIpPermission' smart constructor.
data IpPermission = IpPermission'
  { -- | The start of port range for the TCP and UDP protocols, or an
    -- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
    -- types. If you specify all ICMP\/ICMPv6 types, you must specify all
    -- codes.
    fromPort :: Core.Maybe Core.Int,
    -- | [VPC only] The prefix list IDs.
    prefixListIds :: Core.Maybe [PrefixListId],
    -- | The IPv4 ranges.
    ipRanges :: Core.Maybe [IpRange],
    -- | [VPC only] The IPv6 ranges.
    ipv6Ranges :: Core.Maybe [Ipv6Range],
    -- | The security group and AWS account ID pairs.
    userIdGroupPairs :: Core.Maybe [UserIdGroupPair],
    -- | The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
    -- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
    -- all ICMP\/ICMPv6 types, you must specify all codes.
    toPort :: Core.Maybe Core.Int,
    -- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
    -- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
    --
    -- [VPC only] Use @-1@ to specify all protocols. When authorizing security
    -- group rules, specifying @-1@ or a protocol number other than @tcp@,
    -- @udp@, @icmp@, or @icmpv6@ allows traffic on all ports, regardless of
    -- any port range you specify. For @tcp@, @udp@, and @icmp@, you must
    -- specify a port range. For @icmpv6@, the port range is optional; if you
    -- omit the port range, traffic for all types and codes is allowed.
    ipProtocol :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IpPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'ipPermission_fromPort' - The start of port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
-- types. If you specify all ICMP\/ICMPv6 types, you must specify all
-- codes.
--
-- 'prefixListIds', 'ipPermission_prefixListIds' - [VPC only] The prefix list IDs.
--
-- 'ipRanges', 'ipPermission_ipRanges' - The IPv4 ranges.
--
-- 'ipv6Ranges', 'ipPermission_ipv6Ranges' - [VPC only] The IPv6 ranges.
--
-- 'userIdGroupPairs', 'ipPermission_userIdGroupPairs' - The security group and AWS account ID pairs.
--
-- 'toPort', 'ipPermission_toPort' - The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
-- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
-- all ICMP\/ICMPv6 types, you must specify all codes.
--
-- 'ipProtocol', 'ipPermission_ipProtocol' - The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
--
-- [VPC only] Use @-1@ to specify all protocols. When authorizing security
-- group rules, specifying @-1@ or a protocol number other than @tcp@,
-- @udp@, @icmp@, or @icmpv6@ allows traffic on all ports, regardless of
-- any port range you specify. For @tcp@, @udp@, and @icmp@, you must
-- specify a port range. For @icmpv6@, the port range is optional; if you
-- omit the port range, traffic for all types and codes is allowed.
newIpPermission ::
  -- | 'ipProtocol'
  Core.Text ->
  IpPermission
newIpPermission pIpProtocol_ =
  IpPermission'
    { fromPort = Core.Nothing,
      prefixListIds = Core.Nothing,
      ipRanges = Core.Nothing,
      ipv6Ranges = Core.Nothing,
      userIdGroupPairs = Core.Nothing,
      toPort = Core.Nothing,
      ipProtocol = pIpProtocol_
    }

-- | The start of port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
-- types. If you specify all ICMP\/ICMPv6 types, you must specify all
-- codes.
ipPermission_fromPort :: Lens.Lens' IpPermission (Core.Maybe Core.Int)
ipPermission_fromPort = Lens.lens (\IpPermission' {fromPort} -> fromPort) (\s@IpPermission' {} a -> s {fromPort = a} :: IpPermission)

-- | [VPC only] The prefix list IDs.
ipPermission_prefixListIds :: Lens.Lens' IpPermission (Core.Maybe [PrefixListId])
ipPermission_prefixListIds = Lens.lens (\IpPermission' {prefixListIds} -> prefixListIds) (\s@IpPermission' {} a -> s {prefixListIds = a} :: IpPermission) Core.. Lens.mapping Lens._Coerce

-- | The IPv4 ranges.
ipPermission_ipRanges :: Lens.Lens' IpPermission (Core.Maybe [IpRange])
ipPermission_ipRanges = Lens.lens (\IpPermission' {ipRanges} -> ipRanges) (\s@IpPermission' {} a -> s {ipRanges = a} :: IpPermission) Core.. Lens.mapping Lens._Coerce

-- | [VPC only] The IPv6 ranges.
ipPermission_ipv6Ranges :: Lens.Lens' IpPermission (Core.Maybe [Ipv6Range])
ipPermission_ipv6Ranges = Lens.lens (\IpPermission' {ipv6Ranges} -> ipv6Ranges) (\s@IpPermission' {} a -> s {ipv6Ranges = a} :: IpPermission) Core.. Lens.mapping Lens._Coerce

-- | The security group and AWS account ID pairs.
ipPermission_userIdGroupPairs :: Lens.Lens' IpPermission (Core.Maybe [UserIdGroupPair])
ipPermission_userIdGroupPairs = Lens.lens (\IpPermission' {userIdGroupPairs} -> userIdGroupPairs) (\s@IpPermission' {} a -> s {userIdGroupPairs = a} :: IpPermission) Core.. Lens.mapping Lens._Coerce

-- | The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
-- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
-- all ICMP\/ICMPv6 types, you must specify all codes.
ipPermission_toPort :: Lens.Lens' IpPermission (Core.Maybe Core.Int)
ipPermission_toPort = Lens.lens (\IpPermission' {toPort} -> toPort) (\s@IpPermission' {} a -> s {toPort = a} :: IpPermission)

-- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
--
-- [VPC only] Use @-1@ to specify all protocols. When authorizing security
-- group rules, specifying @-1@ or a protocol number other than @tcp@,
-- @udp@, @icmp@, or @icmpv6@ allows traffic on all ports, regardless of
-- any port range you specify. For @tcp@, @udp@, and @icmp@, you must
-- specify a port range. For @icmpv6@, the port range is optional; if you
-- omit the port range, traffic for all types and codes is allowed.
ipPermission_ipProtocol :: Lens.Lens' IpPermission Core.Text
ipPermission_ipProtocol = Lens.lens (\IpPermission' {ipProtocol} -> ipProtocol) (\s@IpPermission' {} a -> s {ipProtocol = a} :: IpPermission)

instance Core.FromXML IpPermission where
  parseXML x =
    IpPermission'
      Core.<$> (x Core..@? "fromPort")
      Core.<*> ( x Core..@? "prefixListIds" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "ipRanges" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "ipv6Ranges" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "groups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "toPort")
      Core.<*> (x Core..@ "ipProtocol")

instance Core.Hashable IpPermission

instance Core.NFData IpPermission

instance Core.ToQuery IpPermission where
  toQuery IpPermission' {..} =
    Core.mconcat
      [ "FromPort" Core.=: fromPort,
        Core.toQuery
          ( Core.toQueryList "PrefixListIds"
              Core.<$> prefixListIds
          ),
        Core.toQuery
          (Core.toQueryList "IpRanges" Core.<$> ipRanges),
        Core.toQuery
          (Core.toQueryList "Ipv6Ranges" Core.<$> ipv6Ranges),
        Core.toQuery
          ( Core.toQueryList "Groups"
              Core.<$> userIdGroupPairs
          ),
        "ToPort" Core.=: toPort,
        "IpProtocol" Core.=: ipProtocol
      ]
