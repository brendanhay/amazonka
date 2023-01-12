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
-- Module      : Amazonka.Route53Resolver.DisassociateResolverRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between a specified Resolver rule and a
-- specified VPC.
--
-- If you disassociate a Resolver rule from a VPC, Resolver stops
-- forwarding DNS queries for the domain name that you specified in the
-- Resolver rule.
module Amazonka.Route53Resolver.DisassociateResolverRule
  ( -- * Creating a Request
    DisassociateResolverRule (..),
    newDisassociateResolverRule,

    -- * Request Lenses
    disassociateResolverRule_vPCId,
    disassociateResolverRule_resolverRuleId,

    -- * Destructuring the Response
    DisassociateResolverRuleResponse (..),
    newDisassociateResolverRuleResponse,

    -- * Response Lenses
    disassociateResolverRuleResponse_resolverRuleAssociation,
    disassociateResolverRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newDisassociateResolverRule' smart constructor.
data DisassociateResolverRule = DisassociateResolverRule'
  { -- | The ID of the VPC that you want to disassociate the Resolver rule from.
    vPCId :: Prelude.Text,
    -- | The ID of the Resolver rule that you want to disassociate from the
    -- specified VPC.
    resolverRuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResolverRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vPCId', 'disassociateResolverRule_vPCId' - The ID of the VPC that you want to disassociate the Resolver rule from.
--
-- 'resolverRuleId', 'disassociateResolverRule_resolverRuleId' - The ID of the Resolver rule that you want to disassociate from the
-- specified VPC.
newDisassociateResolverRule ::
  -- | 'vPCId'
  Prelude.Text ->
  -- | 'resolverRuleId'
  Prelude.Text ->
  DisassociateResolverRule
newDisassociateResolverRule pVPCId_ pResolverRuleId_ =
  DisassociateResolverRule'
    { vPCId = pVPCId_,
      resolverRuleId = pResolverRuleId_
    }

-- | The ID of the VPC that you want to disassociate the Resolver rule from.
disassociateResolverRule_vPCId :: Lens.Lens' DisassociateResolverRule Prelude.Text
disassociateResolverRule_vPCId = Lens.lens (\DisassociateResolverRule' {vPCId} -> vPCId) (\s@DisassociateResolverRule' {} a -> s {vPCId = a} :: DisassociateResolverRule)

-- | The ID of the Resolver rule that you want to disassociate from the
-- specified VPC.
disassociateResolverRule_resolverRuleId :: Lens.Lens' DisassociateResolverRule Prelude.Text
disassociateResolverRule_resolverRuleId = Lens.lens (\DisassociateResolverRule' {resolverRuleId} -> resolverRuleId) (\s@DisassociateResolverRule' {} a -> s {resolverRuleId = a} :: DisassociateResolverRule)

instance Core.AWSRequest DisassociateResolverRule where
  type
    AWSResponse DisassociateResolverRule =
      DisassociateResolverRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateResolverRuleResponse'
            Prelude.<$> (x Data..?> "ResolverRuleAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateResolverRule where
  hashWithSalt _salt DisassociateResolverRule' {..} =
    _salt `Prelude.hashWithSalt` vPCId
      `Prelude.hashWithSalt` resolverRuleId

instance Prelude.NFData DisassociateResolverRule where
  rnf DisassociateResolverRule' {..} =
    Prelude.rnf vPCId
      `Prelude.seq` Prelude.rnf resolverRuleId

instance Data.ToHeaders DisassociateResolverRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.DisassociateResolverRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateResolverRule where
  toJSON DisassociateResolverRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("VPCId" Data..= vPCId),
            Prelude.Just
              ("ResolverRuleId" Data..= resolverRuleId)
          ]
      )

instance Data.ToPath DisassociateResolverRule where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateResolverRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateResolverRuleResponse' smart constructor.
data DisassociateResolverRuleResponse = DisassociateResolverRuleResponse'
  { -- | Information about the @DisassociateResolverRule@ request, including the
    -- status of the request.
    resolverRuleAssociation :: Prelude.Maybe ResolverRuleAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResolverRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverRuleAssociation', 'disassociateResolverRuleResponse_resolverRuleAssociation' - Information about the @DisassociateResolverRule@ request, including the
-- status of the request.
--
-- 'httpStatus', 'disassociateResolverRuleResponse_httpStatus' - The response's http status code.
newDisassociateResolverRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateResolverRuleResponse
newDisassociateResolverRuleResponse pHttpStatus_ =
  DisassociateResolverRuleResponse'
    { resolverRuleAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the @DisassociateResolverRule@ request, including the
-- status of the request.
disassociateResolverRuleResponse_resolverRuleAssociation :: Lens.Lens' DisassociateResolverRuleResponse (Prelude.Maybe ResolverRuleAssociation)
disassociateResolverRuleResponse_resolverRuleAssociation = Lens.lens (\DisassociateResolverRuleResponse' {resolverRuleAssociation} -> resolverRuleAssociation) (\s@DisassociateResolverRuleResponse' {} a -> s {resolverRuleAssociation = a} :: DisassociateResolverRuleResponse)

-- | The response's http status code.
disassociateResolverRuleResponse_httpStatus :: Lens.Lens' DisassociateResolverRuleResponse Prelude.Int
disassociateResolverRuleResponse_httpStatus = Lens.lens (\DisassociateResolverRuleResponse' {httpStatus} -> httpStatus) (\s@DisassociateResolverRuleResponse' {} a -> s {httpStatus = a} :: DisassociateResolverRuleResponse)

instance
  Prelude.NFData
    DisassociateResolverRuleResponse
  where
  rnf DisassociateResolverRuleResponse' {..} =
    Prelude.rnf resolverRuleAssociation
      `Prelude.seq` Prelude.rnf httpStatus
