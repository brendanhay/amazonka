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
-- Module      : Amazonka.EC2.Types.IpPermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpPermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpRange
import Amazonka.EC2.Types.Ipv6Range
import Amazonka.EC2.Types.PrefixListId
import Amazonka.EC2.Types.UserIdGroupPair
import qualified Amazonka.Prelude as Prelude

-- | Describes a set of permissions for a security group rule.
--
-- /See:/ 'newIpPermission' smart constructor.
data IpPermission = IpPermission'
  { -- | The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
    -- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
    -- all ICMP\/ICMPv6 types, you must specify all codes.
    toPort :: Prelude.Maybe Prelude.Int,
    -- | [VPC only] The IPv6 ranges.
    ipv6Ranges :: Prelude.Maybe [Ipv6Range],
    -- | [VPC only] The prefix list IDs.
    prefixListIds :: Prelude.Maybe [PrefixListId],
    -- | The IPv4 ranges.
    ipRanges :: Prelude.Maybe [IpRange],
    -- | The security group and Amazon Web Services account ID pairs.
    userIdGroupPairs :: Prelude.Maybe [UserIdGroupPair],
    -- | The start of port range for the TCP and UDP protocols, or an
    -- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
    -- types. If you specify all ICMP\/ICMPv6 types, you must specify all
    -- codes.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
    -- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
    --
    -- [VPC only] Use @-1@ to specify all protocols. When authorizing security
    -- group rules, specifying @-1@ or a protocol number other than @tcp@,
    -- @udp@, @icmp@, or @icmpv6@ allows traffic on all ports, regardless of
    -- any port range you specify. For @tcp@, @udp@, and @icmp@, you must
    -- specify a port range. For @icmpv6@, the port range is optional; if you
    -- omit the port range, traffic for all types and codes is allowed.
    ipProtocol :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'toPort', 'ipPermission_toPort' - The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
-- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
-- all ICMP\/ICMPv6 types, you must specify all codes.
--
-- 'ipv6Ranges', 'ipPermission_ipv6Ranges' - [VPC only] The IPv6 ranges.
--
-- 'prefixListIds', 'ipPermission_prefixListIds' - [VPC only] The prefix list IDs.
--
-- 'ipRanges', 'ipPermission_ipRanges' - The IPv4 ranges.
--
-- 'userIdGroupPairs', 'ipPermission_userIdGroupPairs' - The security group and Amazon Web Services account ID pairs.
--
-- 'fromPort', 'ipPermission_fromPort' - The start of port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
-- types. If you specify all ICMP\/ICMPv6 types, you must specify all
-- codes.
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
  Prelude.Text ->
  IpPermission
newIpPermission pIpProtocol_ =
  IpPermission'
    { toPort = Prelude.Nothing,
      ipv6Ranges = Prelude.Nothing,
      prefixListIds = Prelude.Nothing,
      ipRanges = Prelude.Nothing,
      userIdGroupPairs = Prelude.Nothing,
      fromPort = Prelude.Nothing,
      ipProtocol = pIpProtocol_
    }

-- | The end of port range for the TCP and UDP protocols, or an ICMP\/ICMPv6
-- code. A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify
-- all ICMP\/ICMPv6 types, you must specify all codes.
ipPermission_toPort :: Lens.Lens' IpPermission (Prelude.Maybe Prelude.Int)
ipPermission_toPort = Lens.lens (\IpPermission' {toPort} -> toPort) (\s@IpPermission' {} a -> s {toPort = a} :: IpPermission)

-- | [VPC only] The IPv6 ranges.
ipPermission_ipv6Ranges :: Lens.Lens' IpPermission (Prelude.Maybe [Ipv6Range])
ipPermission_ipv6Ranges = Lens.lens (\IpPermission' {ipv6Ranges} -> ipv6Ranges) (\s@IpPermission' {} a -> s {ipv6Ranges = a} :: IpPermission) Prelude.. Lens.mapping Lens.coerced

-- | [VPC only] The prefix list IDs.
ipPermission_prefixListIds :: Lens.Lens' IpPermission (Prelude.Maybe [PrefixListId])
ipPermission_prefixListIds = Lens.lens (\IpPermission' {prefixListIds} -> prefixListIds) (\s@IpPermission' {} a -> s {prefixListIds = a} :: IpPermission) Prelude.. Lens.mapping Lens.coerced

-- | The IPv4 ranges.
ipPermission_ipRanges :: Lens.Lens' IpPermission (Prelude.Maybe [IpRange])
ipPermission_ipRanges = Lens.lens (\IpPermission' {ipRanges} -> ipRanges) (\s@IpPermission' {} a -> s {ipRanges = a} :: IpPermission) Prelude.. Lens.mapping Lens.coerced

-- | The security group and Amazon Web Services account ID pairs.
ipPermission_userIdGroupPairs :: Lens.Lens' IpPermission (Prelude.Maybe [UserIdGroupPair])
ipPermission_userIdGroupPairs = Lens.lens (\IpPermission' {userIdGroupPairs} -> userIdGroupPairs) (\s@IpPermission' {} a -> s {userIdGroupPairs = a} :: IpPermission) Prelude.. Lens.mapping Lens.coerced

-- | The start of port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type number. A value of @-1@ indicates all ICMP\/ICMPv6
-- types. If you specify all ICMP\/ICMPv6 types, you must specify all
-- codes.
ipPermission_fromPort :: Lens.Lens' IpPermission (Prelude.Maybe Prelude.Int)
ipPermission_fromPort = Lens.lens (\IpPermission' {fromPort} -> fromPort) (\s@IpPermission' {} a -> s {fromPort = a} :: IpPermission)

-- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number (see
-- <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>).
--
-- [VPC only] Use @-1@ to specify all protocols. When authorizing security
-- group rules, specifying @-1@ or a protocol number other than @tcp@,
-- @udp@, @icmp@, or @icmpv6@ allows traffic on all ports, regardless of
-- any port range you specify. For @tcp@, @udp@, and @icmp@, you must
-- specify a port range. For @icmpv6@, the port range is optional; if you
-- omit the port range, traffic for all types and codes is allowed.
ipPermission_ipProtocol :: Lens.Lens' IpPermission Prelude.Text
ipPermission_ipProtocol = Lens.lens (\IpPermission' {ipProtocol} -> ipProtocol) (\s@IpPermission' {} a -> s {ipProtocol = a} :: IpPermission)

instance Data.FromXML IpPermission where
  parseXML x =
    IpPermission'
      Prelude.<$> (x Data..@? "toPort")
      Prelude.<*> ( x Data..@? "ipv6Ranges" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "prefixListIds" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "ipRanges" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "groups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "fromPort")
      Prelude.<*> (x Data..@ "ipProtocol")

instance Prelude.Hashable IpPermission where
  hashWithSalt _salt IpPermission' {..} =
    _salt `Prelude.hashWithSalt` toPort
      `Prelude.hashWithSalt` ipv6Ranges
      `Prelude.hashWithSalt` prefixListIds
      `Prelude.hashWithSalt` ipRanges
      `Prelude.hashWithSalt` userIdGroupPairs
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` ipProtocol

instance Prelude.NFData IpPermission where
  rnf IpPermission' {..} =
    Prelude.rnf toPort
      `Prelude.seq` Prelude.rnf ipv6Ranges
      `Prelude.seq` Prelude.rnf prefixListIds
      `Prelude.seq` Prelude.rnf ipRanges
      `Prelude.seq` Prelude.rnf userIdGroupPairs
      `Prelude.seq` Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf ipProtocol

instance Data.ToQuery IpPermission where
  toQuery IpPermission' {..} =
    Prelude.mconcat
      [ "ToPort" Data.=: toPort,
        Data.toQuery
          ( Data.toQueryList "Ipv6Ranges"
              Prelude.<$> ipv6Ranges
          ),
        Data.toQuery
          ( Data.toQueryList "PrefixListIds"
              Prelude.<$> prefixListIds
          ),
        Data.toQuery
          (Data.toQueryList "IpRanges" Prelude.<$> ipRanges),
        Data.toQuery
          ( Data.toQueryList "Groups"
              Prelude.<$> userIdGroupPairs
          ),
        "FromPort" Data.=: fromPort,
        "IpProtocol" Data.=: ipProtocol
      ]
