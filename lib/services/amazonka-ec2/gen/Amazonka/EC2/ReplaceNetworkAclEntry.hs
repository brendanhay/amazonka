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
-- Module      : Amazonka.EC2.ReplaceNetworkAclEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an entry (rule) in a network ACL. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.ReplaceNetworkAclEntry
  ( -- * Creating a Request
    ReplaceNetworkAclEntry (..),
    newReplaceNetworkAclEntry,

    -- * Request Lenses
    replaceNetworkAclEntry_cidrBlock,
    replaceNetworkAclEntry_dryRun,
    replaceNetworkAclEntry_icmpTypeCode,
    replaceNetworkAclEntry_ipv6CidrBlock,
    replaceNetworkAclEntry_portRange,
    replaceNetworkAclEntry_egress,
    replaceNetworkAclEntry_networkAclId,
    replaceNetworkAclEntry_protocol,
    replaceNetworkAclEntry_ruleAction,
    replaceNetworkAclEntry_ruleNumber,

    -- * Destructuring the Response
    ReplaceNetworkAclEntryResponse (..),
    newReplaceNetworkAclEntryResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReplaceNetworkAclEntry' smart constructor.
data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry'
  { -- | The IPv4 network range to allow or deny, in CIDR notation (for example
    -- @172.16.0.0\/24@).
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
    -- @2001:bd8:1234:1a00::\/64@).
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | TCP or UDP protocols: The range of ports the rule applies to. Required
    -- if specifying protocol 6 (TCP) or 17 (UDP).
    portRange :: Prelude.Maybe PortRange,
    -- | Indicates whether to replace the egress rule.
    --
    -- Default: If no value is specified, we replace the ingress rule.
    egress :: Prelude.Bool,
    -- | The ID of the ACL.
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
    -- | The rule number of the entry to replace.
    ruleNumber :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceNetworkAclEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlock', 'replaceNetworkAclEntry_cidrBlock' - The IPv4 network range to allow or deny, in CIDR notation (for example
-- @172.16.0.0\/24@).
--
-- 'dryRun', 'replaceNetworkAclEntry_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'icmpTypeCode', 'replaceNetworkAclEntry_icmpTypeCode' - ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying
-- protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
--
-- 'ipv6CidrBlock', 'replaceNetworkAclEntry_ipv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation (for example
-- @2001:bd8:1234:1a00::\/64@).
--
-- 'portRange', 'replaceNetworkAclEntry_portRange' - TCP or UDP protocols: The range of ports the rule applies to. Required
-- if specifying protocol 6 (TCP) or 17 (UDP).
--
-- 'egress', 'replaceNetworkAclEntry_egress' - Indicates whether to replace the egress rule.
--
-- Default: If no value is specified, we replace the ingress rule.
--
-- 'networkAclId', 'replaceNetworkAclEntry_networkAclId' - The ID of the ACL.
--
-- 'protocol', 'replaceNetworkAclEntry_protocol' - The protocol number. A value of \"-1\" means all protocols. If you
-- specify \"-1\" or a protocol number other than \"6\" (TCP), \"17\"
-- (UDP), or \"1\" (ICMP), traffic on all ports is allowed, regardless of
-- any ports or ICMP types or codes that you specify. If you specify
-- protocol \"58\" (ICMPv6) and specify an IPv4 CIDR block, traffic for all
-- ICMP types and codes allowed, regardless of any that you specify. If you
-- specify protocol \"58\" (ICMPv6) and specify an IPv6 CIDR block, you
-- must specify an ICMP type and code.
--
-- 'ruleAction', 'replaceNetworkAclEntry_ruleAction' - Indicates whether to allow or deny the traffic that matches the rule.
--
-- 'ruleNumber', 'replaceNetworkAclEntry_ruleNumber' - The rule number of the entry to replace.
newReplaceNetworkAclEntry ::
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
  ReplaceNetworkAclEntry
newReplaceNetworkAclEntry
  pEgress_
  pNetworkAclId_
  pProtocol_
  pRuleAction_
  pRuleNumber_ =
    ReplaceNetworkAclEntry'
      { cidrBlock =
          Prelude.Nothing,
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
-- @172.16.0.0\/24@).
replaceNetworkAclEntry_cidrBlock :: Lens.Lens' ReplaceNetworkAclEntry (Prelude.Maybe Prelude.Text)
replaceNetworkAclEntry_cidrBlock = Lens.lens (\ReplaceNetworkAclEntry' {cidrBlock} -> cidrBlock) (\s@ReplaceNetworkAclEntry' {} a -> s {cidrBlock = a} :: ReplaceNetworkAclEntry)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
replaceNetworkAclEntry_dryRun :: Lens.Lens' ReplaceNetworkAclEntry (Prelude.Maybe Prelude.Bool)
replaceNetworkAclEntry_dryRun = Lens.lens (\ReplaceNetworkAclEntry' {dryRun} -> dryRun) (\s@ReplaceNetworkAclEntry' {} a -> s {dryRun = a} :: ReplaceNetworkAclEntry)

-- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying
-- protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
replaceNetworkAclEntry_icmpTypeCode :: Lens.Lens' ReplaceNetworkAclEntry (Prelude.Maybe IcmpTypeCode)
replaceNetworkAclEntry_icmpTypeCode = Lens.lens (\ReplaceNetworkAclEntry' {icmpTypeCode} -> icmpTypeCode) (\s@ReplaceNetworkAclEntry' {} a -> s {icmpTypeCode = a} :: ReplaceNetworkAclEntry)

-- | The IPv6 network range to allow or deny, in CIDR notation (for example
-- @2001:bd8:1234:1a00::\/64@).
replaceNetworkAclEntry_ipv6CidrBlock :: Lens.Lens' ReplaceNetworkAclEntry (Prelude.Maybe Prelude.Text)
replaceNetworkAclEntry_ipv6CidrBlock = Lens.lens (\ReplaceNetworkAclEntry' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@ReplaceNetworkAclEntry' {} a -> s {ipv6CidrBlock = a} :: ReplaceNetworkAclEntry)

-- | TCP or UDP protocols: The range of ports the rule applies to. Required
-- if specifying protocol 6 (TCP) or 17 (UDP).
replaceNetworkAclEntry_portRange :: Lens.Lens' ReplaceNetworkAclEntry (Prelude.Maybe PortRange)
replaceNetworkAclEntry_portRange = Lens.lens (\ReplaceNetworkAclEntry' {portRange} -> portRange) (\s@ReplaceNetworkAclEntry' {} a -> s {portRange = a} :: ReplaceNetworkAclEntry)

-- | Indicates whether to replace the egress rule.
--
-- Default: If no value is specified, we replace the ingress rule.
replaceNetworkAclEntry_egress :: Lens.Lens' ReplaceNetworkAclEntry Prelude.Bool
replaceNetworkAclEntry_egress = Lens.lens (\ReplaceNetworkAclEntry' {egress} -> egress) (\s@ReplaceNetworkAclEntry' {} a -> s {egress = a} :: ReplaceNetworkAclEntry)

-- | The ID of the ACL.
replaceNetworkAclEntry_networkAclId :: Lens.Lens' ReplaceNetworkAclEntry Prelude.Text
replaceNetworkAclEntry_networkAclId = Lens.lens (\ReplaceNetworkAclEntry' {networkAclId} -> networkAclId) (\s@ReplaceNetworkAclEntry' {} a -> s {networkAclId = a} :: ReplaceNetworkAclEntry)

-- | The protocol number. A value of \"-1\" means all protocols. If you
-- specify \"-1\" or a protocol number other than \"6\" (TCP), \"17\"
-- (UDP), or \"1\" (ICMP), traffic on all ports is allowed, regardless of
-- any ports or ICMP types or codes that you specify. If you specify
-- protocol \"58\" (ICMPv6) and specify an IPv4 CIDR block, traffic for all
-- ICMP types and codes allowed, regardless of any that you specify. If you
-- specify protocol \"58\" (ICMPv6) and specify an IPv6 CIDR block, you
-- must specify an ICMP type and code.
replaceNetworkAclEntry_protocol :: Lens.Lens' ReplaceNetworkAclEntry Prelude.Text
replaceNetworkAclEntry_protocol = Lens.lens (\ReplaceNetworkAclEntry' {protocol} -> protocol) (\s@ReplaceNetworkAclEntry' {} a -> s {protocol = a} :: ReplaceNetworkAclEntry)

-- | Indicates whether to allow or deny the traffic that matches the rule.
replaceNetworkAclEntry_ruleAction :: Lens.Lens' ReplaceNetworkAclEntry RuleAction
replaceNetworkAclEntry_ruleAction = Lens.lens (\ReplaceNetworkAclEntry' {ruleAction} -> ruleAction) (\s@ReplaceNetworkAclEntry' {} a -> s {ruleAction = a} :: ReplaceNetworkAclEntry)

-- | The rule number of the entry to replace.
replaceNetworkAclEntry_ruleNumber :: Lens.Lens' ReplaceNetworkAclEntry Prelude.Int
replaceNetworkAclEntry_ruleNumber = Lens.lens (\ReplaceNetworkAclEntry' {ruleNumber} -> ruleNumber) (\s@ReplaceNetworkAclEntry' {} a -> s {ruleNumber = a} :: ReplaceNetworkAclEntry)

instance Core.AWSRequest ReplaceNetworkAclEntry where
  type
    AWSResponse ReplaceNetworkAclEntry =
      ReplaceNetworkAclEntryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      ReplaceNetworkAclEntryResponse'

instance Prelude.Hashable ReplaceNetworkAclEntry where
  hashWithSalt _salt ReplaceNetworkAclEntry' {..} =
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

instance Prelude.NFData ReplaceNetworkAclEntry where
  rnf ReplaceNetworkAclEntry' {..} =
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

instance Data.ToHeaders ReplaceNetworkAclEntry where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReplaceNetworkAclEntry where
  toPath = Prelude.const "/"

instance Data.ToQuery ReplaceNetworkAclEntry where
  toQuery ReplaceNetworkAclEntry' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ReplaceNetworkAclEntry" :: Prelude.ByteString),
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

-- | /See:/ 'newReplaceNetworkAclEntryResponse' smart constructor.
data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceNetworkAclEntryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newReplaceNetworkAclEntryResponse ::
  ReplaceNetworkAclEntryResponse
newReplaceNetworkAclEntryResponse =
  ReplaceNetworkAclEntryResponse'

instance
  Prelude.NFData
    ReplaceNetworkAclEntryResponse
  where
  rnf _ = ()
