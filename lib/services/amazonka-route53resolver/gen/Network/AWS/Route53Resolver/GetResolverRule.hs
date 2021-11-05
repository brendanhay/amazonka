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
-- Module      : Network.AWS.Route53Resolver.GetResolverRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified Resolver rule, such as the domain
-- name that the rule forwards DNS queries for and the ID of the outbound
-- Resolver endpoint that the rule is associated with.
module Network.AWS.Route53Resolver.GetResolverRule
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Resolver.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverRuleResponse'
            Prelude.<$> (x Core..?> "ResolverRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResolverRule

instance Prelude.NFData GetResolverRule

instance Core.ToHeaders GetResolverRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Resolver.GetResolverRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResolverRule where
  toJSON GetResolverRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResolverRuleId" Core..= resolverRuleId)
          ]
      )

instance Core.ToPath GetResolverRule where
  toPath = Prelude.const "/"

instance Core.ToQuery GetResolverRule where
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

instance Prelude.NFData GetResolverRuleResponse
