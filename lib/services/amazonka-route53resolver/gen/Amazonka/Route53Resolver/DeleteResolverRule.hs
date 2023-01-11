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
-- Module      : Amazonka.Route53Resolver.DeleteResolverRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Resolver rule. Before you can delete a Resolver rule, you must
-- disassociate it from all the VPCs that you associated the Resolver rule
-- with. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_DisassociateResolverRule.html DisassociateResolverRule>.
module Amazonka.Route53Resolver.DeleteResolverRule
  ( -- * Creating a Request
    DeleteResolverRule (..),
    newDeleteResolverRule,

    -- * Request Lenses
    deleteResolverRule_resolverRuleId,

    -- * Destructuring the Response
    DeleteResolverRuleResponse (..),
    newDeleteResolverRuleResponse,

    -- * Response Lenses
    deleteResolverRuleResponse_resolverRule,
    deleteResolverRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newDeleteResolverRule' smart constructor.
data DeleteResolverRule = DeleteResolverRule'
  { -- | The ID of the Resolver rule that you want to delete.
    resolverRuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResolverRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverRuleId', 'deleteResolverRule_resolverRuleId' - The ID of the Resolver rule that you want to delete.
newDeleteResolverRule ::
  -- | 'resolverRuleId'
  Prelude.Text ->
  DeleteResolverRule
newDeleteResolverRule pResolverRuleId_ =
  DeleteResolverRule'
    { resolverRuleId =
        pResolverRuleId_
    }

-- | The ID of the Resolver rule that you want to delete.
deleteResolverRule_resolverRuleId :: Lens.Lens' DeleteResolverRule Prelude.Text
deleteResolverRule_resolverRuleId = Lens.lens (\DeleteResolverRule' {resolverRuleId} -> resolverRuleId) (\s@DeleteResolverRule' {} a -> s {resolverRuleId = a} :: DeleteResolverRule)

instance Core.AWSRequest DeleteResolverRule where
  type
    AWSResponse DeleteResolverRule =
      DeleteResolverRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResolverRuleResponse'
            Prelude.<$> (x Data..?> "ResolverRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResolverRule where
  hashWithSalt _salt DeleteResolverRule' {..} =
    _salt `Prelude.hashWithSalt` resolverRuleId

instance Prelude.NFData DeleteResolverRule where
  rnf DeleteResolverRule' {..} =
    Prelude.rnf resolverRuleId

instance Data.ToHeaders DeleteResolverRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.DeleteResolverRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResolverRule where
  toJSON DeleteResolverRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResolverRuleId" Data..= resolverRuleId)
          ]
      )

instance Data.ToPath DeleteResolverRule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteResolverRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResolverRuleResponse' smart constructor.
data DeleteResolverRuleResponse = DeleteResolverRuleResponse'
  { -- | Information about the @DeleteResolverRule@ request, including the status
    -- of the request.
    resolverRule :: Prelude.Maybe ResolverRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResolverRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverRule', 'deleteResolverRuleResponse_resolverRule' - Information about the @DeleteResolverRule@ request, including the status
-- of the request.
--
-- 'httpStatus', 'deleteResolverRuleResponse_httpStatus' - The response's http status code.
newDeleteResolverRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResolverRuleResponse
newDeleteResolverRuleResponse pHttpStatus_ =
  DeleteResolverRuleResponse'
    { resolverRule =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the @DeleteResolverRule@ request, including the status
-- of the request.
deleteResolverRuleResponse_resolverRule :: Lens.Lens' DeleteResolverRuleResponse (Prelude.Maybe ResolverRule)
deleteResolverRuleResponse_resolverRule = Lens.lens (\DeleteResolverRuleResponse' {resolverRule} -> resolverRule) (\s@DeleteResolverRuleResponse' {} a -> s {resolverRule = a} :: DeleteResolverRuleResponse)

-- | The response's http status code.
deleteResolverRuleResponse_httpStatus :: Lens.Lens' DeleteResolverRuleResponse Prelude.Int
deleteResolverRuleResponse_httpStatus = Lens.lens (\DeleteResolverRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteResolverRuleResponse' {} a -> s {httpStatus = a} :: DeleteResolverRuleResponse)

instance Prelude.NFData DeleteResolverRuleResponse where
  rnf DeleteResolverRuleResponse' {..} =
    Prelude.rnf resolverRule
      `Prelude.seq` Prelude.rnf httpStatus
