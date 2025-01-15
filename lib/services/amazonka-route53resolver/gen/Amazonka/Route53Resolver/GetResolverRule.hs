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
-- Module      : Amazonka.Route53Resolver.GetResolverRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified Resolver rule, such as the domain
-- name that the rule forwards DNS queries for and the ID of the outbound
-- Resolver endpoint that the rule is associated with.
module Amazonka.Route53Resolver.GetResolverRule
  ( -- * Creating a Request
    GetResolverRule (..),
    newGetResolverRule,

    -- * Request Lenses
    getResolverRule_resolverRuleId,

    -- * Destructuring the Response
    GetResolverRuleResponse (..),
    newGetResolverRuleResponse,

    -- * Response Lenses
    getResolverRuleResponse_resolverRule,
    getResolverRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetResolverRule' smart constructor.
data GetResolverRule = GetResolverRule'
  { -- | The ID of the Resolver rule that you want to get information about.
    resolverRuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverRuleId', 'getResolverRule_resolverRuleId' - The ID of the Resolver rule that you want to get information about.
newGetResolverRule ::
  -- | 'resolverRuleId'
  Prelude.Text ->
  GetResolverRule
newGetResolverRule pResolverRuleId_ =
  GetResolverRule' {resolverRuleId = pResolverRuleId_}

-- | The ID of the Resolver rule that you want to get information about.
getResolverRule_resolverRuleId :: Lens.Lens' GetResolverRule Prelude.Text
getResolverRule_resolverRuleId = Lens.lens (\GetResolverRule' {resolverRuleId} -> resolverRuleId) (\s@GetResolverRule' {} a -> s {resolverRuleId = a} :: GetResolverRule)

instance Core.AWSRequest GetResolverRule where
  type
    AWSResponse GetResolverRule =
      GetResolverRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverRuleResponse'
            Prelude.<$> (x Data..?> "ResolverRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResolverRule where
  hashWithSalt _salt GetResolverRule' {..} =
    _salt `Prelude.hashWithSalt` resolverRuleId

instance Prelude.NFData GetResolverRule where
  rnf GetResolverRule' {..} = Prelude.rnf resolverRuleId

instance Data.ToHeaders GetResolverRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.GetResolverRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResolverRule where
  toJSON GetResolverRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResolverRuleId" Data..= resolverRuleId)
          ]
      )

instance Data.ToPath GetResolverRule where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResolverRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResolverRuleResponse' smart constructor.
data GetResolverRuleResponse = GetResolverRuleResponse'
  { -- | Information about the Resolver rule that you specified in a
    -- @GetResolverRule@ request.
    resolverRule :: Prelude.Maybe ResolverRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverRule', 'getResolverRuleResponse_resolverRule' - Information about the Resolver rule that you specified in a
-- @GetResolverRule@ request.
--
-- 'httpStatus', 'getResolverRuleResponse_httpStatus' - The response's http status code.
newGetResolverRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResolverRuleResponse
newGetResolverRuleResponse pHttpStatus_ =
  GetResolverRuleResponse'
    { resolverRule =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Resolver rule that you specified in a
-- @GetResolverRule@ request.
getResolverRuleResponse_resolverRule :: Lens.Lens' GetResolverRuleResponse (Prelude.Maybe ResolverRule)
getResolverRuleResponse_resolverRule = Lens.lens (\GetResolverRuleResponse' {resolverRule} -> resolverRule) (\s@GetResolverRuleResponse' {} a -> s {resolverRule = a} :: GetResolverRuleResponse)

-- | The response's http status code.
getResolverRuleResponse_httpStatus :: Lens.Lens' GetResolverRuleResponse Prelude.Int
getResolverRuleResponse_httpStatus = Lens.lens (\GetResolverRuleResponse' {httpStatus} -> httpStatus) (\s@GetResolverRuleResponse' {} a -> s {httpStatus = a} :: GetResolverRuleResponse)

instance Prelude.NFData GetResolverRuleResponse where
  rnf GetResolverRuleResponse' {..} =
    Prelude.rnf resolverRule `Prelude.seq`
      Prelude.rnf httpStatus
