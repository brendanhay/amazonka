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
-- Module      : Amazonka.Route53Resolver.AssociateResolverRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a Resolver rule with a VPC. When you associate a rule with a
-- VPC, Resolver forwards all DNS queries for the domain name that is
-- specified in the rule and that originate in the VPC. The queries are
-- forwarded to the IP addresses for the DNS resolvers that are specified
-- in the rule. For more information about rules, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_CreateResolverRule.html CreateResolverRule>.
module Amazonka.Route53Resolver.AssociateResolverRule
  ( -- * Creating a Request
    AssociateResolverRule (..),
    newAssociateResolverRule,

    -- * Request Lenses
    associateResolverRule_name,
    associateResolverRule_resolverRuleId,
    associateResolverRule_vPCId,

    -- * Destructuring the Response
    AssociateResolverRuleResponse (..),
    newAssociateResolverRuleResponse,

    -- * Response Lenses
    associateResolverRuleResponse_resolverRuleAssociation,
    associateResolverRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newAssociateResolverRule' smart constructor.
data AssociateResolverRule = AssociateResolverRule'
  { -- | A name for the association that you\'re creating between a Resolver rule
    -- and a VPC.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Resolver rule that you want to associate with the VPC. To
    -- list the existing Resolver rules, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_ListResolverRules.html ListResolverRules>.
    resolverRuleId :: Prelude.Text,
    -- | The ID of the VPC that you want to associate the Resolver rule with.
    vPCId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResolverRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'associateResolverRule_name' - A name for the association that you\'re creating between a Resolver rule
-- and a VPC.
--
-- 'resolverRuleId', 'associateResolverRule_resolverRuleId' - The ID of the Resolver rule that you want to associate with the VPC. To
-- list the existing Resolver rules, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_ListResolverRules.html ListResolverRules>.
--
-- 'vPCId', 'associateResolverRule_vPCId' - The ID of the VPC that you want to associate the Resolver rule with.
newAssociateResolverRule ::
  -- | 'resolverRuleId'
  Prelude.Text ->
  -- | 'vPCId'
  Prelude.Text ->
  AssociateResolverRule
newAssociateResolverRule pResolverRuleId_ pVPCId_ =
  AssociateResolverRule'
    { name = Prelude.Nothing,
      resolverRuleId = pResolverRuleId_,
      vPCId = pVPCId_
    }

-- | A name for the association that you\'re creating between a Resolver rule
-- and a VPC.
associateResolverRule_name :: Lens.Lens' AssociateResolverRule (Prelude.Maybe Prelude.Text)
associateResolverRule_name = Lens.lens (\AssociateResolverRule' {name} -> name) (\s@AssociateResolverRule' {} a -> s {name = a} :: AssociateResolverRule)

-- | The ID of the Resolver rule that you want to associate with the VPC. To
-- list the existing Resolver rules, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_ListResolverRules.html ListResolverRules>.
associateResolverRule_resolverRuleId :: Lens.Lens' AssociateResolverRule Prelude.Text
associateResolverRule_resolverRuleId = Lens.lens (\AssociateResolverRule' {resolverRuleId} -> resolverRuleId) (\s@AssociateResolverRule' {} a -> s {resolverRuleId = a} :: AssociateResolverRule)

-- | The ID of the VPC that you want to associate the Resolver rule with.
associateResolverRule_vPCId :: Lens.Lens' AssociateResolverRule Prelude.Text
associateResolverRule_vPCId = Lens.lens (\AssociateResolverRule' {vPCId} -> vPCId) (\s@AssociateResolverRule' {} a -> s {vPCId = a} :: AssociateResolverRule)

instance Core.AWSRequest AssociateResolverRule where
  type
    AWSResponse AssociateResolverRule =
      AssociateResolverRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateResolverRuleResponse'
            Prelude.<$> (x Data..?> "ResolverRuleAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateResolverRule where
  hashWithSalt _salt AssociateResolverRule' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resolverRuleId
      `Prelude.hashWithSalt` vPCId

instance Prelude.NFData AssociateResolverRule where
  rnf AssociateResolverRule' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf resolverRuleId
      `Prelude.seq` Prelude.rnf vPCId

instance Data.ToHeaders AssociateResolverRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.AssociateResolverRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateResolverRule where
  toJSON AssociateResolverRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            Prelude.Just
              ("ResolverRuleId" Data..= resolverRuleId),
            Prelude.Just ("VPCId" Data..= vPCId)
          ]
      )

instance Data.ToPath AssociateResolverRule where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateResolverRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateResolverRuleResponse' smart constructor.
data AssociateResolverRuleResponse = AssociateResolverRuleResponse'
  { -- | Information about the @AssociateResolverRule@ request, including the
    -- status of the request.
    resolverRuleAssociation :: Prelude.Maybe ResolverRuleAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResolverRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverRuleAssociation', 'associateResolverRuleResponse_resolverRuleAssociation' - Information about the @AssociateResolverRule@ request, including the
-- status of the request.
--
-- 'httpStatus', 'associateResolverRuleResponse_httpStatus' - The response's http status code.
newAssociateResolverRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateResolverRuleResponse
newAssociateResolverRuleResponse pHttpStatus_ =
  AssociateResolverRuleResponse'
    { resolverRuleAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the @AssociateResolverRule@ request, including the
-- status of the request.
associateResolverRuleResponse_resolverRuleAssociation :: Lens.Lens' AssociateResolverRuleResponse (Prelude.Maybe ResolverRuleAssociation)
associateResolverRuleResponse_resolverRuleAssociation = Lens.lens (\AssociateResolverRuleResponse' {resolverRuleAssociation} -> resolverRuleAssociation) (\s@AssociateResolverRuleResponse' {} a -> s {resolverRuleAssociation = a} :: AssociateResolverRuleResponse)

-- | The response's http status code.
associateResolverRuleResponse_httpStatus :: Lens.Lens' AssociateResolverRuleResponse Prelude.Int
associateResolverRuleResponse_httpStatus = Lens.lens (\AssociateResolverRuleResponse' {httpStatus} -> httpStatus) (\s@AssociateResolverRuleResponse' {} a -> s {httpStatus = a} :: AssociateResolverRuleResponse)

instance Prelude.NFData AssociateResolverRuleResponse where
  rnf AssociateResolverRuleResponse' {..} =
    Prelude.rnf resolverRuleAssociation
      `Prelude.seq` Prelude.rnf httpStatus
