{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.ReplaceNetworkAclEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an entry (rule) in a network ACL. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.ReplaceNetworkAclEntry
  ( -- * Creating a Request
    ReplaceNetworkAclEntry (..),
    newReplaceNetworkAclEntry,

    -- * Request Lenses
    replaceNetworkAclEntry_dryRun,
    replaceNetworkAclEntry_portRange,
    replaceNetworkAclEntry_icmpTypeCode,
    replaceNetworkAclEntry_ipv6CidrBlock,
    replaceNetworkAclEntry_cidrBlock,
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newReplaceNetworkAclEntry' smart constructor.
data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | TCP or UDP protocols: The range of ports the rule applies to. Required
    -- if specifying protocol 6 (TCP) or 17 (UDP).
    portRange :: Prelude.Maybe PortRange,
    -- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying
    -- protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
    icmpTypeCode :: Prelude.Maybe IcmpTypeCode,
    -- | The IPv6 network range to allow or deny, in CIDR notation (for example
    -- @2001:bd8:1234:1a00::\/64@).
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 network range to allow or deny, in CIDR notation (for example
    -- @172.16.0.0\/24@).
    cidrBlock :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplaceNetworkAclEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'replaceNetworkAclEntry_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'portRange', 'replaceNetworkAclEntry_portRange' - TCP or UDP protocols: The range of ports the rule applies to. Required
-- if specifying protocol 6 (TCP) or 17 (UDP).
--
-- 'icmpTypeCode', 'replaceNetworkAclEntry_icmpTypeCode' - ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying
-- protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
--
-- 'ipv6CidrBlock', 'replaceNetworkAclEntry_ipv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation (for example
-- @2001:bd8:1234:1a00::\/64@).
--
-- 'cidrBlock', 'replaceNetworkAclEntry_cidrBlock' - The IPv4 network range to allow or deny, in CIDR notation (for example
-- @172.16.0.0\/24@).
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
      { dryRun = Prelude.Nothing,
        portRange = Prelude.Nothing,
        icmpTypeCode = Prelude.Nothing,
        ipv6CidrBlock = Prelude.Nothing,
        cidrBlock = Prelude.Nothing,
        egress = pEgress_,
        networkAclId = pNetworkAclId_,
        protocol = pProtocol_,
        ruleAction = pRuleAction_,
        ruleNumber = pRuleNumber_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
replaceNetworkAclEntry_dryRun :: Lens.Lens' ReplaceNetworkAclEntry (Prelude.Maybe Prelude.Bool)
replaceNetworkAclEntry_dryRun = Lens.lens (\ReplaceNetworkAclEntry' {dryRun} -> dryRun) (\s@ReplaceNetworkAclEntry' {} a -> s {dryRun = a} :: ReplaceNetworkAclEntry)

-- | TCP or UDP protocols: The range of ports the rule applies to. Required
-- if specifying protocol 6 (TCP) or 17 (UDP).
replaceNetworkAclEntry_portRange :: Lens.Lens' ReplaceNetworkAclEntry (Prelude.Maybe PortRange)
replaceNetworkAclEntry_portRange = Lens.lens (\ReplaceNetworkAclEntry' {portRange} -> portRange) (\s@ReplaceNetworkAclEntry' {} a -> s {portRange = a} :: ReplaceNetworkAclEntry)

-- | ICMP protocol: The ICMP or ICMPv6 type and code. Required if specifying
-- protocol 1 (ICMP) or protocol 58 (ICMPv6) with an IPv6 CIDR block.
replaceNetworkAclEntry_icmpTypeCode :: Lens.Lens' ReplaceNetworkAclEntry (Prelude.Maybe IcmpTypeCode)
replaceNetworkAclEntry_icmpTypeCode = Lens.lens (\ReplaceNetworkAclEntry' {icmpTypeCode} -> icmpTypeCode) (\s@ReplaceNetworkAclEntry' {} a -> s {icmpTypeCode = a} :: ReplaceNetworkAclEntry)

-- | The IPv6 network range to allow or deny, in CIDR notation (for example
-- @2001:bd8:1234:1a00::\/64@).
replaceNetworkAclEntry_ipv6CidrBlock :: Lens.Lens' ReplaceNetworkAclEntry (Prelude.Maybe Prelude.Text)
replaceNetworkAclEntry_ipv6CidrBlock = Lens.lens (\ReplaceNetworkAclEntry' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@ReplaceNetworkAclEntry' {} a -> s {ipv6CidrBlock = a} :: ReplaceNetworkAclEntry)

-- | The IPv4 network range to allow or deny, in CIDR notation (for example
-- @172.16.0.0\/24@).
replaceNetworkAclEntry_cidrBlock :: Lens.Lens' ReplaceNetworkAclEntry (Prelude.Maybe Prelude.Text)
replaceNetworkAclEntry_cidrBlock = Lens.lens (\ReplaceNetworkAclEntry' {cidrBlock} -> cidrBlock) (\s@ReplaceNetworkAclEntry' {} a -> s {cidrBlock = a} :: ReplaceNetworkAclEntry)

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

instance Prelude.AWSRequest ReplaceNetworkAclEntry where
  type
    Rs ReplaceNetworkAclEntry =
      ReplaceNetworkAclEntryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      ReplaceNetworkAclEntryResponse'

instance Prelude.Hashable ReplaceNetworkAclEntry

instance Prelude.NFData ReplaceNetworkAclEntry

instance Prelude.ToHeaders ReplaceNetworkAclEntry where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ReplaceNetworkAclEntry where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ReplaceNetworkAclEntry where
  toQuery ReplaceNetworkAclEntry' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ReplaceNetworkAclEntry" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "PortRange" Prelude.=: portRange,
        "Icmp" Prelude.=: icmpTypeCode,
        "Ipv6CidrBlock" Prelude.=: ipv6CidrBlock,
        "CidrBlock" Prelude.=: cidrBlock,
        "Egress" Prelude.=: egress,
        "NetworkAclId" Prelude.=: networkAclId,
        "Protocol" Prelude.=: protocol,
        "RuleAction" Prelude.=: ruleAction,
        "RuleNumber" Prelude.=: ruleNumber
      ]

-- | /See:/ 'newReplaceNetworkAclEntryResponse' smart constructor.
data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
