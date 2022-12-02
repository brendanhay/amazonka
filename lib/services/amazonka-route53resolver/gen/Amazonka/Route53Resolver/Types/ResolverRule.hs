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
-- Module      : Amazonka.Route53Resolver.Types.ResolverRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.ResolverRuleStatus
import Amazonka.Route53Resolver.Types.RuleTypeOption
import Amazonka.Route53Resolver.Types.ShareStatus
import Amazonka.Route53Resolver.Types.TargetAddress

-- | For queries that originate in your VPC, detailed information about a
-- Resolver rule, which specifies how to route DNS queries out of the VPC.
-- The @ResolverRule@ parameter appears in the response to a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_CreateResolverRule.html CreateResolverRule>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_DeleteResolverRule.html DeleteResolverRule>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverRule.html GetResolverRule>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_ListResolverRules.html ListResolverRules>,
-- or
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_UpdateResolverRule.html UpdateResolverRule>
-- request.
--
-- /See:/ 'newResolverRule' smart constructor.
data ResolverRule = ResolverRule'
  { -- | The name for the Resolver rule, which you specified when you created the
    -- Resolver rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | When a rule is shared with another Amazon Web Services account, the
    -- account ID of the account that the rule is shared with.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Whether the rule is shared and, if so, whether the current account is
    -- sharing the rule with another account, or another account is sharing the
    -- rule with the current account.
    shareStatus :: Prelude.Maybe ShareStatus,
    -- | DNS queries for this domain name are forwarded to the IP addresses that
    -- are specified in @TargetIps@. If a query matches multiple Resolver rules
    -- (example.com and www.example.com), the query is routed using the
    -- Resolver rule that contains the most specific domain name
    -- (www.example.com).
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the Resolver rule was last updated, in Unix time
    -- format and Coordinated Universal Time (UTC).
    modificationTime :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) for the Resolver rule specified by @Id@.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An array that contains the IP addresses and ports that an outbound
    -- endpoint forwards DNS queries to. Typically, these are the IP addresses
    -- of DNS resolvers on your network. Specify IPv4 addresses. IPv6 is not
    -- supported.
    targetIps :: Prelude.Maybe (Prelude.NonEmpty TargetAddress),
    -- | A code that specifies the current status of the Resolver rule.
    status :: Prelude.Maybe ResolverRuleStatus,
    -- | The ID that Resolver assigned to the Resolver rule when you created it.
    id :: Prelude.Maybe Prelude.Text,
    -- | A unique string that you specified when you created the Resolver rule.
    -- @CreatorRequestId@ identifies the request and allows failed requests to
    -- be retried without the risk of running the operation twice.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | When you want to forward DNS queries for specified domain name to
    -- resolvers on your network, specify @FORWARD@.
    --
    -- When you have a forwarding rule to forward DNS queries for a domain to
    -- your network and you want Resolver to process queries for a subdomain of
    -- that domain, specify @SYSTEM@.
    --
    -- For example, to forward DNS queries for example.com to resolvers on your
    -- network, you create a rule and specify @FORWARD@ for @RuleType@. To then
    -- have Resolver process queries for apex.example.com, you create a rule
    -- and specify @SYSTEM@ for @RuleType@.
    --
    -- Currently, only Resolver can create rules that have a value of
    -- @RECURSIVE@ for @RuleType@.
    ruleType :: Prelude.Maybe RuleTypeOption,
    -- | The date and time that the Resolver rule was created, in Unix time
    -- format and Coordinated Universal Time (UTC).
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | A detailed description of the status of a Resolver rule.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the endpoint that the rule is associated with.
    resolverEndpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolverRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'resolverRule_name' - The name for the Resolver rule, which you specified when you created the
-- Resolver rule.
--
-- 'ownerId', 'resolverRule_ownerId' - When a rule is shared with another Amazon Web Services account, the
-- account ID of the account that the rule is shared with.
--
-- 'shareStatus', 'resolverRule_shareStatus' - Whether the rule is shared and, if so, whether the current account is
-- sharing the rule with another account, or another account is sharing the
-- rule with the current account.
--
-- 'domainName', 'resolverRule_domainName' - DNS queries for this domain name are forwarded to the IP addresses that
-- are specified in @TargetIps@. If a query matches multiple Resolver rules
-- (example.com and www.example.com), the query is routed using the
-- Resolver rule that contains the most specific domain name
-- (www.example.com).
--
-- 'modificationTime', 'resolverRule_modificationTime' - The date and time that the Resolver rule was last updated, in Unix time
-- format and Coordinated Universal Time (UTC).
--
-- 'arn', 'resolverRule_arn' - The ARN (Amazon Resource Name) for the Resolver rule specified by @Id@.
--
-- 'targetIps', 'resolverRule_targetIps' - An array that contains the IP addresses and ports that an outbound
-- endpoint forwards DNS queries to. Typically, these are the IP addresses
-- of DNS resolvers on your network. Specify IPv4 addresses. IPv6 is not
-- supported.
--
-- 'status', 'resolverRule_status' - A code that specifies the current status of the Resolver rule.
--
-- 'id', 'resolverRule_id' - The ID that Resolver assigned to the Resolver rule when you created it.
--
-- 'creatorRequestId', 'resolverRule_creatorRequestId' - A unique string that you specified when you created the Resolver rule.
-- @CreatorRequestId@ identifies the request and allows failed requests to
-- be retried without the risk of running the operation twice.
--
-- 'ruleType', 'resolverRule_ruleType' - When you want to forward DNS queries for specified domain name to
-- resolvers on your network, specify @FORWARD@.
--
-- When you have a forwarding rule to forward DNS queries for a domain to
-- your network and you want Resolver to process queries for a subdomain of
-- that domain, specify @SYSTEM@.
--
-- For example, to forward DNS queries for example.com to resolvers on your
-- network, you create a rule and specify @FORWARD@ for @RuleType@. To then
-- have Resolver process queries for apex.example.com, you create a rule
-- and specify @SYSTEM@ for @RuleType@.
--
-- Currently, only Resolver can create rules that have a value of
-- @RECURSIVE@ for @RuleType@.
--
-- 'creationTime', 'resolverRule_creationTime' - The date and time that the Resolver rule was created, in Unix time
-- format and Coordinated Universal Time (UTC).
--
-- 'statusMessage', 'resolverRule_statusMessage' - A detailed description of the status of a Resolver rule.
--
-- 'resolverEndpointId', 'resolverRule_resolverEndpointId' - The ID of the endpoint that the rule is associated with.
newResolverRule ::
  ResolverRule
newResolverRule =
  ResolverRule'
    { name = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      shareStatus = Prelude.Nothing,
      domainName = Prelude.Nothing,
      modificationTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      targetIps = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      ruleType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      resolverEndpointId = Prelude.Nothing
    }

-- | The name for the Resolver rule, which you specified when you created the
-- Resolver rule.
resolverRule_name :: Lens.Lens' ResolverRule (Prelude.Maybe Prelude.Text)
resolverRule_name = Lens.lens (\ResolverRule' {name} -> name) (\s@ResolverRule' {} a -> s {name = a} :: ResolverRule)

-- | When a rule is shared with another Amazon Web Services account, the
-- account ID of the account that the rule is shared with.
resolverRule_ownerId :: Lens.Lens' ResolverRule (Prelude.Maybe Prelude.Text)
resolverRule_ownerId = Lens.lens (\ResolverRule' {ownerId} -> ownerId) (\s@ResolverRule' {} a -> s {ownerId = a} :: ResolverRule)

-- | Whether the rule is shared and, if so, whether the current account is
-- sharing the rule with another account, or another account is sharing the
-- rule with the current account.
resolverRule_shareStatus :: Lens.Lens' ResolverRule (Prelude.Maybe ShareStatus)
resolverRule_shareStatus = Lens.lens (\ResolverRule' {shareStatus} -> shareStatus) (\s@ResolverRule' {} a -> s {shareStatus = a} :: ResolverRule)

-- | DNS queries for this domain name are forwarded to the IP addresses that
-- are specified in @TargetIps@. If a query matches multiple Resolver rules
-- (example.com and www.example.com), the query is routed using the
-- Resolver rule that contains the most specific domain name
-- (www.example.com).
resolverRule_domainName :: Lens.Lens' ResolverRule (Prelude.Maybe Prelude.Text)
resolverRule_domainName = Lens.lens (\ResolverRule' {domainName} -> domainName) (\s@ResolverRule' {} a -> s {domainName = a} :: ResolverRule)

-- | The date and time that the Resolver rule was last updated, in Unix time
-- format and Coordinated Universal Time (UTC).
resolverRule_modificationTime :: Lens.Lens' ResolverRule (Prelude.Maybe Prelude.Text)
resolverRule_modificationTime = Lens.lens (\ResolverRule' {modificationTime} -> modificationTime) (\s@ResolverRule' {} a -> s {modificationTime = a} :: ResolverRule)

-- | The ARN (Amazon Resource Name) for the Resolver rule specified by @Id@.
resolverRule_arn :: Lens.Lens' ResolverRule (Prelude.Maybe Prelude.Text)
resolverRule_arn = Lens.lens (\ResolverRule' {arn} -> arn) (\s@ResolverRule' {} a -> s {arn = a} :: ResolverRule)

-- | An array that contains the IP addresses and ports that an outbound
-- endpoint forwards DNS queries to. Typically, these are the IP addresses
-- of DNS resolvers on your network. Specify IPv4 addresses. IPv6 is not
-- supported.
resolverRule_targetIps :: Lens.Lens' ResolverRule (Prelude.Maybe (Prelude.NonEmpty TargetAddress))
resolverRule_targetIps = Lens.lens (\ResolverRule' {targetIps} -> targetIps) (\s@ResolverRule' {} a -> s {targetIps = a} :: ResolverRule) Prelude.. Lens.mapping Lens.coerced

-- | A code that specifies the current status of the Resolver rule.
resolverRule_status :: Lens.Lens' ResolverRule (Prelude.Maybe ResolverRuleStatus)
resolverRule_status = Lens.lens (\ResolverRule' {status} -> status) (\s@ResolverRule' {} a -> s {status = a} :: ResolverRule)

-- | The ID that Resolver assigned to the Resolver rule when you created it.
resolverRule_id :: Lens.Lens' ResolverRule (Prelude.Maybe Prelude.Text)
resolverRule_id = Lens.lens (\ResolverRule' {id} -> id) (\s@ResolverRule' {} a -> s {id = a} :: ResolverRule)

-- | A unique string that you specified when you created the Resolver rule.
-- @CreatorRequestId@ identifies the request and allows failed requests to
-- be retried without the risk of running the operation twice.
resolverRule_creatorRequestId :: Lens.Lens' ResolverRule (Prelude.Maybe Prelude.Text)
resolverRule_creatorRequestId = Lens.lens (\ResolverRule' {creatorRequestId} -> creatorRequestId) (\s@ResolverRule' {} a -> s {creatorRequestId = a} :: ResolverRule)

-- | When you want to forward DNS queries for specified domain name to
-- resolvers on your network, specify @FORWARD@.
--
-- When you have a forwarding rule to forward DNS queries for a domain to
-- your network and you want Resolver to process queries for a subdomain of
-- that domain, specify @SYSTEM@.
--
-- For example, to forward DNS queries for example.com to resolvers on your
-- network, you create a rule and specify @FORWARD@ for @RuleType@. To then
-- have Resolver process queries for apex.example.com, you create a rule
-- and specify @SYSTEM@ for @RuleType@.
--
-- Currently, only Resolver can create rules that have a value of
-- @RECURSIVE@ for @RuleType@.
resolverRule_ruleType :: Lens.Lens' ResolverRule (Prelude.Maybe RuleTypeOption)
resolverRule_ruleType = Lens.lens (\ResolverRule' {ruleType} -> ruleType) (\s@ResolverRule' {} a -> s {ruleType = a} :: ResolverRule)

-- | The date and time that the Resolver rule was created, in Unix time
-- format and Coordinated Universal Time (UTC).
resolverRule_creationTime :: Lens.Lens' ResolverRule (Prelude.Maybe Prelude.Text)
resolverRule_creationTime = Lens.lens (\ResolverRule' {creationTime} -> creationTime) (\s@ResolverRule' {} a -> s {creationTime = a} :: ResolverRule)

-- | A detailed description of the status of a Resolver rule.
resolverRule_statusMessage :: Lens.Lens' ResolverRule (Prelude.Maybe Prelude.Text)
resolverRule_statusMessage = Lens.lens (\ResolverRule' {statusMessage} -> statusMessage) (\s@ResolverRule' {} a -> s {statusMessage = a} :: ResolverRule)

-- | The ID of the endpoint that the rule is associated with.
resolverRule_resolverEndpointId :: Lens.Lens' ResolverRule (Prelude.Maybe Prelude.Text)
resolverRule_resolverEndpointId = Lens.lens (\ResolverRule' {resolverEndpointId} -> resolverEndpointId) (\s@ResolverRule' {} a -> s {resolverEndpointId = a} :: ResolverRule)

instance Data.FromJSON ResolverRule where
  parseJSON =
    Data.withObject
      "ResolverRule"
      ( \x ->
          ResolverRule'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "ShareStatus")
            Prelude.<*> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "ModificationTime")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "TargetIps")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "CreatorRequestId")
            Prelude.<*> (x Data..:? "RuleType")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "ResolverEndpointId")
      )

instance Prelude.Hashable ResolverRule where
  hashWithSalt _salt ResolverRule' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` shareStatus
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` modificationTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` targetIps
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` ruleType
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` resolverEndpointId

instance Prelude.NFData ResolverRule where
  rnf ResolverRule' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf shareStatus
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf modificationTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf targetIps
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf ruleType
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf resolverEndpointId
