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
-- Module      : Amazonka.Route53Resolver.CreateResolverRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For DNS queries that originate in your VPCs, specifies which Resolver
-- endpoint the queries pass through, one domain name that you want to
-- forward to your network, and the IP addresses of the DNS resolvers in
-- your network.
module Amazonka.Route53Resolver.CreateResolverRule
  ( -- * Creating a Request
    CreateResolverRule (..),
    newCreateResolverRule,

    -- * Request Lenses
    createResolverRule_name,
    createResolverRule_resolverEndpointId,
    createResolverRule_tags,
    createResolverRule_targetIps,
    createResolverRule_creatorRequestId,
    createResolverRule_ruleType,
    createResolverRule_domainName,

    -- * Destructuring the Response
    CreateResolverRuleResponse (..),
    newCreateResolverRuleResponse,

    -- * Response Lenses
    createResolverRuleResponse_resolverRule,
    createResolverRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newCreateResolverRule' smart constructor.
data CreateResolverRule = CreateResolverRule'
  { -- | A friendly name that lets you easily find a rule in the Resolver
    -- dashboard in the Route 53 console.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the outbound Resolver endpoint that you want to use to route
    -- DNS queries to the IP addresses that you specify in @TargetIps@.
    resolverEndpointId :: Prelude.Maybe Prelude.Text,
    -- | A list of the tag keys and values that you want to associate with the
    -- endpoint.
    tags :: Prelude.Maybe [Tag],
    -- | The IPs that you want Resolver to forward DNS queries to. You can
    -- specify only IPv4 addresses. Separate IP addresses with a space.
    --
    -- @TargetIps@ is available only when the value of @Rule type@ is
    -- @FORWARD@.
    targetIps :: Prelude.Maybe (Prelude.NonEmpty TargetAddress),
    -- | A unique string that identifies the request and that allows failed
    -- requests to be retried without the risk of running the operation twice.
    -- @CreatorRequestId@ can be any unique string, for example, a date\/time
    -- stamp.
    creatorRequestId :: Prelude.Text,
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
    ruleType :: RuleTypeOption,
    -- | DNS queries for this domain name are forwarded to the IP addresses that
    -- you specify in @TargetIps@. If a query matches multiple Resolver rules
    -- (example.com and www.example.com), outbound DNS queries are routed using
    -- the Resolver rule that contains the most specific domain name
    -- (www.example.com).
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResolverRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createResolverRule_name' - A friendly name that lets you easily find a rule in the Resolver
-- dashboard in the Route 53 console.
--
-- 'resolverEndpointId', 'createResolverRule_resolverEndpointId' - The ID of the outbound Resolver endpoint that you want to use to route
-- DNS queries to the IP addresses that you specify in @TargetIps@.
--
-- 'tags', 'createResolverRule_tags' - A list of the tag keys and values that you want to associate with the
-- endpoint.
--
-- 'targetIps', 'createResolverRule_targetIps' - The IPs that you want Resolver to forward DNS queries to. You can
-- specify only IPv4 addresses. Separate IP addresses with a space.
--
-- @TargetIps@ is available only when the value of @Rule type@ is
-- @FORWARD@.
--
-- 'creatorRequestId', 'createResolverRule_creatorRequestId' - A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of running the operation twice.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
--
-- 'ruleType', 'createResolverRule_ruleType' - When you want to forward DNS queries for specified domain name to
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
-- 'domainName', 'createResolverRule_domainName' - DNS queries for this domain name are forwarded to the IP addresses that
-- you specify in @TargetIps@. If a query matches multiple Resolver rules
-- (example.com and www.example.com), outbound DNS queries are routed using
-- the Resolver rule that contains the most specific domain name
-- (www.example.com).
newCreateResolverRule ::
  -- | 'creatorRequestId'
  Prelude.Text ->
  -- | 'ruleType'
  RuleTypeOption ->
  -- | 'domainName'
  Prelude.Text ->
  CreateResolverRule
newCreateResolverRule
  pCreatorRequestId_
  pRuleType_
  pDomainName_ =
    CreateResolverRule'
      { name = Prelude.Nothing,
        resolverEndpointId = Prelude.Nothing,
        tags = Prelude.Nothing,
        targetIps = Prelude.Nothing,
        creatorRequestId = pCreatorRequestId_,
        ruleType = pRuleType_,
        domainName = pDomainName_
      }

-- | A friendly name that lets you easily find a rule in the Resolver
-- dashboard in the Route 53 console.
createResolverRule_name :: Lens.Lens' CreateResolverRule (Prelude.Maybe Prelude.Text)
createResolverRule_name = Lens.lens (\CreateResolverRule' {name} -> name) (\s@CreateResolverRule' {} a -> s {name = a} :: CreateResolverRule)

-- | The ID of the outbound Resolver endpoint that you want to use to route
-- DNS queries to the IP addresses that you specify in @TargetIps@.
createResolverRule_resolverEndpointId :: Lens.Lens' CreateResolverRule (Prelude.Maybe Prelude.Text)
createResolverRule_resolverEndpointId = Lens.lens (\CreateResolverRule' {resolverEndpointId} -> resolverEndpointId) (\s@CreateResolverRule' {} a -> s {resolverEndpointId = a} :: CreateResolverRule)

-- | A list of the tag keys and values that you want to associate with the
-- endpoint.
createResolverRule_tags :: Lens.Lens' CreateResolverRule (Prelude.Maybe [Tag])
createResolverRule_tags = Lens.lens (\CreateResolverRule' {tags} -> tags) (\s@CreateResolverRule' {} a -> s {tags = a} :: CreateResolverRule) Prelude.. Lens.mapping Lens.coerced

-- | The IPs that you want Resolver to forward DNS queries to. You can
-- specify only IPv4 addresses. Separate IP addresses with a space.
--
-- @TargetIps@ is available only when the value of @Rule type@ is
-- @FORWARD@.
createResolverRule_targetIps :: Lens.Lens' CreateResolverRule (Prelude.Maybe (Prelude.NonEmpty TargetAddress))
createResolverRule_targetIps = Lens.lens (\CreateResolverRule' {targetIps} -> targetIps) (\s@CreateResolverRule' {} a -> s {targetIps = a} :: CreateResolverRule) Prelude.. Lens.mapping Lens.coerced

-- | A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of running the operation twice.
-- @CreatorRequestId@ can be any unique string, for example, a date\/time
-- stamp.
createResolverRule_creatorRequestId :: Lens.Lens' CreateResolverRule Prelude.Text
createResolverRule_creatorRequestId = Lens.lens (\CreateResolverRule' {creatorRequestId} -> creatorRequestId) (\s@CreateResolverRule' {} a -> s {creatorRequestId = a} :: CreateResolverRule)

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
createResolverRule_ruleType :: Lens.Lens' CreateResolverRule RuleTypeOption
createResolverRule_ruleType = Lens.lens (\CreateResolverRule' {ruleType} -> ruleType) (\s@CreateResolverRule' {} a -> s {ruleType = a} :: CreateResolverRule)

-- | DNS queries for this domain name are forwarded to the IP addresses that
-- you specify in @TargetIps@. If a query matches multiple Resolver rules
-- (example.com and www.example.com), outbound DNS queries are routed using
-- the Resolver rule that contains the most specific domain name
-- (www.example.com).
createResolverRule_domainName :: Lens.Lens' CreateResolverRule Prelude.Text
createResolverRule_domainName = Lens.lens (\CreateResolverRule' {domainName} -> domainName) (\s@CreateResolverRule' {} a -> s {domainName = a} :: CreateResolverRule)

instance Core.AWSRequest CreateResolverRule where
  type
    AWSResponse CreateResolverRule =
      CreateResolverRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResolverRuleResponse'
            Prelude.<$> (x Data..?> "ResolverRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateResolverRule where
  hashWithSalt _salt CreateResolverRule' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resolverEndpointId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetIps
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` ruleType
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData CreateResolverRule where
  rnf CreateResolverRule' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf resolverEndpointId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetIps
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf ruleType
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders CreateResolverRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.CreateResolverRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResolverRule where
  toJSON CreateResolverRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("ResolverEndpointId" Data..=)
              Prelude.<$> resolverEndpointId,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TargetIps" Data..=) Prelude.<$> targetIps,
            Prelude.Just
              ("CreatorRequestId" Data..= creatorRequestId),
            Prelude.Just ("RuleType" Data..= ruleType),
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath CreateResolverRule where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateResolverRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResolverRuleResponse' smart constructor.
data CreateResolverRuleResponse = CreateResolverRuleResponse'
  { -- | Information about the @CreateResolverRule@ request, including the status
    -- of the request.
    resolverRule :: Prelude.Maybe ResolverRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResolverRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverRule', 'createResolverRuleResponse_resolverRule' - Information about the @CreateResolverRule@ request, including the status
-- of the request.
--
-- 'httpStatus', 'createResolverRuleResponse_httpStatus' - The response's http status code.
newCreateResolverRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResolverRuleResponse
newCreateResolverRuleResponse pHttpStatus_ =
  CreateResolverRuleResponse'
    { resolverRule =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the @CreateResolverRule@ request, including the status
-- of the request.
createResolverRuleResponse_resolverRule :: Lens.Lens' CreateResolverRuleResponse (Prelude.Maybe ResolverRule)
createResolverRuleResponse_resolverRule = Lens.lens (\CreateResolverRuleResponse' {resolverRule} -> resolverRule) (\s@CreateResolverRuleResponse' {} a -> s {resolverRule = a} :: CreateResolverRuleResponse)

-- | The response's http status code.
createResolverRuleResponse_httpStatus :: Lens.Lens' CreateResolverRuleResponse Prelude.Int
createResolverRuleResponse_httpStatus = Lens.lens (\CreateResolverRuleResponse' {httpStatus} -> httpStatus) (\s@CreateResolverRuleResponse' {} a -> s {httpStatus = a} :: CreateResolverRuleResponse)

instance Prelude.NFData CreateResolverRuleResponse where
  rnf CreateResolverRuleResponse' {..} =
    Prelude.rnf resolverRule
      `Prelude.seq` Prelude.rnf httpStatus
