{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.CreateNetworkAclEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an entry (a rule) in a network ACL with the specified rule
-- number. Each network ACL has a set of numbered ingress rules and a
-- separate set of numbered egress rules. When determining whether a packet
-- should be allowed in or out of a subnet associated with the ACL, we
-- process the entries in the ACL according to the rule numbers, in
-- ascending order. Each network ACL has a set of ingress rules and a
-- separate set of egress rules.
--
-- We recommend that you leave room between the rule numbers (for example,
-- 100, 110, 120, ...), and not number them one right after the other (for
-- example, 101, 102, 103, ...). This makes it easier to add a rule between
-- existing ones without having to renumber the rules.
--
-- After you add an entry, you can\'t modify it; you must either replace
-- it, or create an entry and delete the old one.
--
-- For more information about network ACLs, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.CreateNetworkAclEntry
  ( -- * Creating a Request
    CreateNetworkAclEntry (..),
    newCreateNetworkAclEntry,

    -- * Request Lenses
    createNetworkAclEntry_cidrBlock,
    createNetworkAclEntry_dryRun,
    createNetworkAclEntry_icmpTypeCode,
    createNetworkAclEntry_ipv6CidrBlock,
    createNetworkAclEntry_portRange,
    createNetworkAclEntry_egress,
    createNetworkAclEntry_networkAclId,
    createNetworkAclEntry_protocol,
    createNetworkAclEntry_ruleAction,
    createNetworkAclEntry_ruleNumber,

    -- * Destructuring the Response
    CreateNetworkAclEntryResponse (..),
    newCreateNetworkAclEntryResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNetworkAclEntry' smart constructor.
data CreateNetworkAclEntry = CreateNetworkAclEntry'
  { -- | The IPv4 network range to allow or deny, in CIDR notation (for example
    -- @172.16.0.0\/24@). We modify the specified CIDR block to its canonical
    -- form; for example, if you specify @100.68.0.18\/18@, we modify it to
    -- @100.68.0.0\/18@.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying
    -- protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
    icmpTypeCode :: Prelude.Maybe IcmpTypeCode,
    -- | The IPv6 network range to allow or deny, in CIDR notation (for example
    -- @2001:db8:1234:1a00::\/64@).
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | TCP or UDP protocols: The range of ports the rule applies to. Required
    -- if specifying protocol 6 (TCP) or 17 (UDP).
    portRange :: Prelude.Maybe PortRange,
    -- | Indicates whether this is an egress rule (rule is applied to traffic
    -- leaving the subnet).
    egress :: Prelude.Bool,
    -- | The ID of the network ACL.
    networkAclId :: Prelude.Text,
    -- | The protocol number. A value of \"-1\" means all protocols. If you
    -- specify \"-1\" or a protocol number other than \"6\" (TCP), \"17\"
    -- (UDP), or \"1\" (ICMP), traffic on all ports is allowed, regardless of
    -- any ports or ICMP types or codes that you specify. If you specify
    -- protocol \"58\" (ICMPv6) and specify an IPv4 CIDR block, traffic for all
    -- ICMP types and codes allowed, regardless of any that you specify. If you
    -- specify protocol \"58\" (ICMPv6) and specify an IPv6 CIDR block, you
    -- must specify an ICMP type and code.
    protocol :: Prelude.Text,
    -- | Indicates whether to allow or deny the traffic that matches the rule.
    ruleAction :: RuleAction,
    -- | The rule number for the entry (for example, 100). ACL entries are
    -- processed in ascending order by rule number.
    --
    -- Constraints: Positive integer from 1 to 32766. The range 32767 to 65535
    -- is reserved for internal use.
    ruleNumber :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkAclEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlock', 'createNetworkAclEntry_cidrBlock' - The IPv4 network range to allow or deny, in CIDR notation (for example
-- @172.16.0.0\/24@). We modify the specified CIDR block to its canonical
-- form; for example, if you specify @100.68.0.18\/18@, we modify it to
-- @100.68.0.0\/18@.
--
-- 'dryRun', 'createNetworkAclEntry_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'icmpTypeCode', 'createNetworkAclEntry_icmpTypeCode' - ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying
-- protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
--
-- 'ipv6CidrBlock', 'createNetworkAclEntry_ipv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation (for example
-- @2001:db8:1234:1a00::\/64@).
--
-- 'portRange', 'createNetworkAclEntry_portRange' - TCP or UDP protocols: The range of ports the rule applies to. Required
-- if specifying protocol 6 (TCP) or 17 (UDP).
--
-- 'egress', 'createNetworkAclEntry_egress' - Indicates whether this is an egress rule (rule is applied to traffic
-- leaving the subnet).
--
-- 'networkAclId', 'createNetworkAclEntry_networkAclId' - The ID of the network ACL.
--
-- 'protocol', 'createNetworkAclEntry_protocol' - The protocol number. A value of \"-1\" means all protocols. If you
-- specify \"-1\" or a protocol number other than \"6\" (TCP), \"17\"
-- (UDP), or \"1\" (ICMP), traffic on all ports is allowed, regardless of
-- any ports or ICMP types or codes that you specify. If you specify
-- protocol \"58\" (ICMPv6) and specify an IPv4 CIDR block, traffic for all
-- ICMP types and codes allowed, regardless of any that you specify. If you
-- specify protocol \"58\" (ICMPv6) and specify an IPv6 CIDR block, you
-- must specify an ICMP type and code.
--
-- 'ruleAction', 'createNetworkAclEntry_ruleAction' - Indicates whether to allow or deny the traffic that matches the rule.
--
-- 'ruleNumber', 'createNetworkAclEntry_ruleNumber' - The rule number for the entry (for example, 100). ACL entries are
-- processed in ascending order by rule number.
--
-- Constraints: Positive integer from 1 to 32766. The range 32767 to 65535
-- is reserved for internal use.
newCreateNetworkAclEntry ::
  -- | 'egress'
  Prelude.Bool ->
  -- | 'networkAclId'
  Prelude.Text ->
  -- | 'protocol'
  Prelude.Text ->
  -- | 'ruleAction'
  RuleAction ->
  -- | 'ruleNumber'
  Prelude.Int ->
  CreateNetworkAclEntry
newCreateNetworkAclEntry
  pEgress_
  pNetworkAclId_
  pProtocol_
  pRuleAction_
  pRuleNumber_ =
    CreateNetworkAclEntry'
      { cidrBlock = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        icmpTypeCode = Prelude.Nothing,
        ipv6CidrBlock = Prelude.Nothing,
        portRange = Prelude.Nothing,
        egress = pEgress_,
        networkAclId = pNetworkAclId_,
        protocol = pProtocol_,
        ruleAction = pRuleAction_,
        ruleNumber = pRuleNumber_
      }

-- | The IPv4 network range to allow or deny, in CIDR notation (for example
-- @172.16.0.0\/24@). We modify the specified CIDR block to its canonical
-- form; for example, if you specify @100.68.0.18\/18@, we modify it to
-- @100.68.0.0\/18@.
createNetworkAclEntry_cidrBlock :: Lens.Lens' CreateNetworkAclEntry (Prelude.Maybe Prelude.Text)
createNetworkAclEntry_cidrBlock = Lens.lens (\CreateNetworkAclEntry' {cidrBlock} -> cidrBlock) (\s@CreateNetworkAclEntry' {} a -> s {cidrBlock = a} :: CreateNetworkAclEntry)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNetworkAclEntry_dryRun :: Lens.Lens' CreateNetworkAclEntry (Prelude.Maybe Prelude.Bool)
createNetworkAclEntry_dryRun = Lens.lens (\CreateNetworkAclEntry' {dryRun} -> dryRun) (\s@CreateNetworkAclEntry' {} a -> s {dryRun = a} :: CreateNetworkAclEntry)

-- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying
-- protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
createNetworkAclEntry_icmpTypeCode :: Lens.Lens' CreateNetworkAclEntry (Prelude.Maybe IcmpTypeCode)
createNetworkAclEntry_icmpTypeCode = Lens.lens (\CreateNetworkAclEntry' {icmpTypeCode} -> icmpTypeCode) (\s@CreateNetworkAclEntry' {} a -> s {icmpTypeCode = a} :: CreateNetworkAclEntry)

-- | The IPv6 network range to allow or deny, in CIDR notation (for example
-- @2001:db8:1234:1a00::\/64@).
createNetworkAclEntry_ipv6CidrBlock :: Lens.Lens' CreateNetworkAclEntry (Prelude.Maybe Prelude.Text)
createNetworkAclEntry_ipv6CidrBlock = Lens.lens (\CreateNetworkAclEntry' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@CreateNetworkAclEntry' {} a -> s {ipv6CidrBlock = a} :: CreateNetworkAclEntry)

-- | TCP or UDP protocols: The range of ports the rule applies to. Required
-- if specifying protocol 6 (TCP) or 17 (UDP).
createNetworkAclEntry_portRange :: Lens.Lens' CreateNetworkAclEntry (Prelude.Maybe PortRange)
createNetworkAclEntry_portRange = Lens.lens (\CreateNetworkAclEntry' {portRange} -> portRange) (\s@CreateNetworkAclEntry' {} a -> s {portRange = a} :: CreateNetworkAclEntry)

-- | Indicates whether this is an egress rule (rule is applied to traffic
-- leaving the subnet).
createNetworkAclEntry_egress :: Lens.Lens' CreateNetworkAclEntry Prelude.Bool
createNetworkAclEntry_egress = Lens.lens (\CreateNetworkAclEntry' {egress} -> egress) (\s@CreateNetworkAclEntry' {} a -> s {egress = a} :: CreateNetworkAclEntry)

-- | The ID of the network ACL.
createNetworkAclEntry_networkAclId :: Lens.Lens' CreateNetworkAclEntry Prelude.Text
createNetworkAclEntry_networkAclId = Lens.lens (\CreateNetworkAclEntry' {networkAclId} -> networkAclId) (\s@CreateNetworkAclEntry' {} a -> s {networkAclId = a} :: CreateNetworkAclEntry)

-- | The protocol number. A value of \"-1\" means all protocols. If you
-- specify \"-1\" or a protocol number other than \"6\" (TCP), \"17\"
-- (UDP), or \"1\" (ICMP), traffic on all ports is allowed, regardless of
-- any ports or ICMP types or codes that you specify. If you specify
-- protocol \"58\" (ICMPv6) and specify an IPv4 CIDR block, traffic for all
-- ICMP types and codes allowed, regardless of any that you specify. If you
-- specify protocol \"58\" (ICMPv6) and specify an IPv6 CIDR block, you
-- must specify an ICMP type and code.
createNetworkAclEntry_protocol :: Lens.Lens' CreateNetworkAclEntry Prelude.Text
createNetworkAclEntry_protocol = Lens.lens (\CreateNetworkAclEntry' {protocol} -> protocol) (\s@CreateNetworkAclEntry' {} a -> s {protocol = a} :: CreateNetworkAclEntry)

-- | Indicates whether to allow or deny the traffic that matches the rule.
createNetworkAclEntry_ruleAction :: Lens.Lens' CreateNetworkAclEntry RuleAction
createNetworkAclEntry_ruleAction = Lens.lens (\CreateNetworkAclEntry' {ruleAction} -> ruleAction) (\s@CreateNetworkAclEntry' {} a -> s {ruleAction = a} :: CreateNetworkAclEntry)

-- | The rule number for the entry (for example, 100). ACL entries are
-- processed in ascending order by rule number.
--
-- Constraints: Positive integer from 1 to 32766. The range 32767 to 65535
-- is reserved for internal use.
createNetworkAclEntry_ruleNumber :: Lens.Lens' CreateNetworkAclEntry Prelude.Int
createNetworkAclEntry_ruleNumber = Lens.lens (\CreateNetworkAclEntry' {ruleNumber} -> ruleNumber) (\s@CreateNetworkAclEntry' {} a -> s {ruleNumber = a} :: CreateNetworkAclEntry)

instance Core.AWSRequest CreateNetworkAclEntry where
  type
    AWSResponse CreateNetworkAclEntry =
      CreateNetworkAclEntryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull CreateNetworkAclEntryResponse'

instance Prelude.Hashable CreateNetworkAclEntry where
  hashWithSalt _salt CreateNetworkAclEntry' {..} =
    _salt
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` icmpTypeCode
      `Prelude.hashWithSalt` ipv6CidrBlock
      `Prelude.hashWithSalt` portRange
      `Prelude.hashWithSalt` egress
      `Prelude.hashWithSalt` networkAclId
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` ruleAction
      `Prelude.hashWithSalt` ruleNumber

instance Prelude.NFData CreateNetworkAclEntry where
  rnf CreateNetworkAclEntry' {..} =
    Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf icmpTypeCode
      `Prelude.seq` Prelude.rnf ipv6CidrBlock
      `Prelude.seq` Prelude.rnf portRange
      `Prelude.seq` Prelude.rnf egress
      `Prelude.seq` Prelude.rnf networkAclId
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf ruleAction
      `Prelude.seq` Prelude.rnf ruleNumber

instance Data.ToHeaders CreateNetworkAclEntry where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateNetworkAclEntry where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateNetworkAclEntry where
  toQuery CreateNetworkAclEntry' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateNetworkAclEntry" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "CidrBlock" Data.=: cidrBlock,
        "DryRun" Data.=: dryRun,
        "Icmp" Data.=: icmpTypeCode,
        "Ipv6CidrBlock" Data.=: ipv6CidrBlock,
        "PortRange" Data.=: portRange,
        "Egress" Data.=: egress,
        "NetworkAclId" Data.=: networkAclId,
        "Protocol" Data.=: protocol,
        "RuleAction" Data.=: ruleAction,
        "RuleNumber" Data.=: ruleNumber
      ]

-- | /See:/ 'newCreateNetworkAclEntryResponse' smart constructor.
data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkAclEntryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateNetworkAclEntryResponse ::
  CreateNetworkAclEntryResponse
newCreateNetworkAclEntryResponse =
  CreateNetworkAclEntryResponse'

instance Prelude.NFData CreateNetworkAclEntryResponse where
  rnf _ = ()
