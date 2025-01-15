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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpPermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpRange
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpv6Range
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupPrefixListId
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupUserIdGroupPair

-- | An IP permission for an EC2 security group.
--
-- /See:/ 'newAwsEc2SecurityGroupIpPermission' smart constructor.
data AwsEc2SecurityGroupIpPermission = AwsEc2SecurityGroupIpPermission'
  { -- | The start of the port range for the TCP and UDP protocols, or an
    -- ICMP\/ICMPv6 type number.
    --
    -- A value of -1 indicates all ICMP\/ICMPv6 types. If you specify all
    -- ICMP\/ICMPv6 types, you must specify all codes.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number.
    --
    -- [VPC only] Use @-1@ to specify all protocols.
    --
    -- When authorizing security group rules, specifying @-1@ or a protocol
    -- number other than @tcp@, @udp@, @icmp@, or @icmpv6@ allows traffic on
    -- all ports, regardless of any port range you specify.
    --
    -- For @tcp@, @udp@, and @icmp@, you must specify a port range.
    --
    -- For @icmpv6@, the port range is optional. If you omit the port range,
    -- traffic for all types and codes is allowed.
    ipProtocol :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 ranges.
    ipRanges :: Prelude.Maybe [AwsEc2SecurityGroupIpRange],
    -- | The IPv6 ranges.
    ipv6Ranges :: Prelude.Maybe [AwsEc2SecurityGroupIpv6Range],
    -- | [VPC only] The prefix list IDs for an Amazon Web Services service. With
    -- outbound rules, this is the Amazon Web Services service to access
    -- through a VPC endpoint from instances associated with the security
    -- group.
    prefixListIds :: Prelude.Maybe [AwsEc2SecurityGroupPrefixListId],
    -- | The end of the port range for the TCP and UDP protocols, or an
    -- ICMP\/ICMPv6 code.
    --
    -- A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify all
    -- ICMP\/ICMPv6 types, you must specify all codes.
    toPort :: Prelude.Maybe Prelude.Int,
    -- | The security group and Amazon Web Services account ID pairs.
    userIdGroupPairs :: Prelude.Maybe [AwsEc2SecurityGroupUserIdGroupPair]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2SecurityGroupIpPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'awsEc2SecurityGroupIpPermission_fromPort' - The start of the port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type number.
--
-- A value of -1 indicates all ICMP\/ICMPv6 types. If you specify all
-- ICMP\/ICMPv6 types, you must specify all codes.
--
-- 'ipProtocol', 'awsEc2SecurityGroupIpPermission_ipProtocol' - The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number.
--
-- [VPC only] Use @-1@ to specify all protocols.
--
-- When authorizing security group rules, specifying @-1@ or a protocol
-- number other than @tcp@, @udp@, @icmp@, or @icmpv6@ allows traffic on
-- all ports, regardless of any port range you specify.
--
-- For @tcp@, @udp@, and @icmp@, you must specify a port range.
--
-- For @icmpv6@, the port range is optional. If you omit the port range,
-- traffic for all types and codes is allowed.
--
-- 'ipRanges', 'awsEc2SecurityGroupIpPermission_ipRanges' - The IPv4 ranges.
--
-- 'ipv6Ranges', 'awsEc2SecurityGroupIpPermission_ipv6Ranges' - The IPv6 ranges.
--
-- 'prefixListIds', 'awsEc2SecurityGroupIpPermission_prefixListIds' - [VPC only] The prefix list IDs for an Amazon Web Services service. With
-- outbound rules, this is the Amazon Web Services service to access
-- through a VPC endpoint from instances associated with the security
-- group.
--
-- 'toPort', 'awsEc2SecurityGroupIpPermission_toPort' - The end of the port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 code.
--
-- A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify all
-- ICMP\/ICMPv6 types, you must specify all codes.
--
-- 'userIdGroupPairs', 'awsEc2SecurityGroupIpPermission_userIdGroupPairs' - The security group and Amazon Web Services account ID pairs.
newAwsEc2SecurityGroupIpPermission ::
  AwsEc2SecurityGroupIpPermission
newAwsEc2SecurityGroupIpPermission =
  AwsEc2SecurityGroupIpPermission'
    { fromPort =
        Prelude.Nothing,
      ipProtocol = Prelude.Nothing,
      ipRanges = Prelude.Nothing,
      ipv6Ranges = Prelude.Nothing,
      prefixListIds = Prelude.Nothing,
      toPort = Prelude.Nothing,
      userIdGroupPairs = Prelude.Nothing
    }

-- | The start of the port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 type number.
--
-- A value of -1 indicates all ICMP\/ICMPv6 types. If you specify all
-- ICMP\/ICMPv6 types, you must specify all codes.
awsEc2SecurityGroupIpPermission_fromPort :: Lens.Lens' AwsEc2SecurityGroupIpPermission (Prelude.Maybe Prelude.Int)
awsEc2SecurityGroupIpPermission_fromPort = Lens.lens (\AwsEc2SecurityGroupIpPermission' {fromPort} -> fromPort) (\s@AwsEc2SecurityGroupIpPermission' {} a -> s {fromPort = a} :: AwsEc2SecurityGroupIpPermission)

-- | The IP protocol name (@tcp@, @udp@, @icmp@, @icmpv6@) or number.
--
-- [VPC only] Use @-1@ to specify all protocols.
--
-- When authorizing security group rules, specifying @-1@ or a protocol
-- number other than @tcp@, @udp@, @icmp@, or @icmpv6@ allows traffic on
-- all ports, regardless of any port range you specify.
--
-- For @tcp@, @udp@, and @icmp@, you must specify a port range.
--
-- For @icmpv6@, the port range is optional. If you omit the port range,
-- traffic for all types and codes is allowed.
awsEc2SecurityGroupIpPermission_ipProtocol :: Lens.Lens' AwsEc2SecurityGroupIpPermission (Prelude.Maybe Prelude.Text)
awsEc2SecurityGroupIpPermission_ipProtocol = Lens.lens (\AwsEc2SecurityGroupIpPermission' {ipProtocol} -> ipProtocol) (\s@AwsEc2SecurityGroupIpPermission' {} a -> s {ipProtocol = a} :: AwsEc2SecurityGroupIpPermission)

-- | The IPv4 ranges.
awsEc2SecurityGroupIpPermission_ipRanges :: Lens.Lens' AwsEc2SecurityGroupIpPermission (Prelude.Maybe [AwsEc2SecurityGroupIpRange])
awsEc2SecurityGroupIpPermission_ipRanges = Lens.lens (\AwsEc2SecurityGroupIpPermission' {ipRanges} -> ipRanges) (\s@AwsEc2SecurityGroupIpPermission' {} a -> s {ipRanges = a} :: AwsEc2SecurityGroupIpPermission) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 ranges.
awsEc2SecurityGroupIpPermission_ipv6Ranges :: Lens.Lens' AwsEc2SecurityGroupIpPermission (Prelude.Maybe [AwsEc2SecurityGroupIpv6Range])
awsEc2SecurityGroupIpPermission_ipv6Ranges = Lens.lens (\AwsEc2SecurityGroupIpPermission' {ipv6Ranges} -> ipv6Ranges) (\s@AwsEc2SecurityGroupIpPermission' {} a -> s {ipv6Ranges = a} :: AwsEc2SecurityGroupIpPermission) Prelude.. Lens.mapping Lens.coerced

-- | [VPC only] The prefix list IDs for an Amazon Web Services service. With
-- outbound rules, this is the Amazon Web Services service to access
-- through a VPC endpoint from instances associated with the security
-- group.
awsEc2SecurityGroupIpPermission_prefixListIds :: Lens.Lens' AwsEc2SecurityGroupIpPermission (Prelude.Maybe [AwsEc2SecurityGroupPrefixListId])
awsEc2SecurityGroupIpPermission_prefixListIds = Lens.lens (\AwsEc2SecurityGroupIpPermission' {prefixListIds} -> prefixListIds) (\s@AwsEc2SecurityGroupIpPermission' {} a -> s {prefixListIds = a} :: AwsEc2SecurityGroupIpPermission) Prelude.. Lens.mapping Lens.coerced

-- | The end of the port range for the TCP and UDP protocols, or an
-- ICMP\/ICMPv6 code.
--
-- A value of @-1@ indicates all ICMP\/ICMPv6 codes. If you specify all
-- ICMP\/ICMPv6 types, you must specify all codes.
awsEc2SecurityGroupIpPermission_toPort :: Lens.Lens' AwsEc2SecurityGroupIpPermission (Prelude.Maybe Prelude.Int)
awsEc2SecurityGroupIpPermission_toPort = Lens.lens (\AwsEc2SecurityGroupIpPermission' {toPort} -> toPort) (\s@AwsEc2SecurityGroupIpPermission' {} a -> s {toPort = a} :: AwsEc2SecurityGroupIpPermission)

-- | The security group and Amazon Web Services account ID pairs.
awsEc2SecurityGroupIpPermission_userIdGroupPairs :: Lens.Lens' AwsEc2SecurityGroupIpPermission (Prelude.Maybe [AwsEc2SecurityGroupUserIdGroupPair])
awsEc2SecurityGroupIpPermission_userIdGroupPairs = Lens.lens (\AwsEc2SecurityGroupIpPermission' {userIdGroupPairs} -> userIdGroupPairs) (\s@AwsEc2SecurityGroupIpPermission' {} a -> s {userIdGroupPairs = a} :: AwsEc2SecurityGroupIpPermission) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsEc2SecurityGroupIpPermission
  where
  parseJSON =
    Data.withObject
      "AwsEc2SecurityGroupIpPermission"
      ( \x ->
          AwsEc2SecurityGroupIpPermission'
            Prelude.<$> (x Data..:? "FromPort")
            Prelude.<*> (x Data..:? "IpProtocol")
            Prelude.<*> (x Data..:? "IpRanges" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Ipv6Ranges" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PrefixListIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ToPort")
            Prelude.<*> ( x
                            Data..:? "UserIdGroupPairs"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AwsEc2SecurityGroupIpPermission
  where
  hashWithSalt
    _salt
    AwsEc2SecurityGroupIpPermission' {..} =
      _salt
        `Prelude.hashWithSalt` fromPort
        `Prelude.hashWithSalt` ipProtocol
        `Prelude.hashWithSalt` ipRanges
        `Prelude.hashWithSalt` ipv6Ranges
        `Prelude.hashWithSalt` prefixListIds
        `Prelude.hashWithSalt` toPort
        `Prelude.hashWithSalt` userIdGroupPairs

instance
  Prelude.NFData
    AwsEc2SecurityGroupIpPermission
  where
  rnf AwsEc2SecurityGroupIpPermission' {..} =
    Prelude.rnf fromPort `Prelude.seq`
      Prelude.rnf ipProtocol `Prelude.seq`
        Prelude.rnf ipRanges `Prelude.seq`
          Prelude.rnf ipv6Ranges `Prelude.seq`
            Prelude.rnf prefixListIds `Prelude.seq`
              Prelude.rnf toPort `Prelude.seq`
                Prelude.rnf userIdGroupPairs

instance Data.ToJSON AwsEc2SecurityGroupIpPermission where
  toJSON AwsEc2SecurityGroupIpPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FromPort" Data..=) Prelude.<$> fromPort,
            ("IpProtocol" Data..=) Prelude.<$> ipProtocol,
            ("IpRanges" Data..=) Prelude.<$> ipRanges,
            ("Ipv6Ranges" Data..=) Prelude.<$> ipv6Ranges,
            ("PrefixListIds" Data..=) Prelude.<$> prefixListIds,
            ("ToPort" Data..=) Prelude.<$> toPort,
            ("UserIdGroupPairs" Data..=)
              Prelude.<$> userIdGroupPairs
          ]
      )
