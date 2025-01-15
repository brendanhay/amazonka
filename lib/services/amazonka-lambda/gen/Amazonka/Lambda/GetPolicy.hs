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
-- Module      : Amazonka.Lambda.GetPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the
-- <https://docs.aws.amazon.com/lambda/latest/dg/access-control-resource-based.html resource-based IAM policy>
-- for a function, version, or alias.
module Amazonka.Lambda.GetPolicy
  ( -- * Creating a Request
    GetPolicy (..),
    newGetPolicy,

    -- * Request Lenses
    getPolicy_qualifier,
    getPolicy_functionName,

    -- * Destructuring the Response
    GetPolicyResponse (..),
    newGetPolicyResponse,

    -- * Response Lenses
    getPolicyResponse_policy,
    getPolicyResponse_revisionId,
    getPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { -- | Specify a version or alias to get the policy for that resource.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@ (name-only), @my-function:v1@
    --     (with alias).
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
    --
    -- You can append a version number or alias to any of the formats. The
    -- length constraint applies only to the full ARN. If you specify only the
    -- function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualifier', 'getPolicy_qualifier' - Specify a version or alias to get the policy for that resource.
--
-- 'functionName', 'getPolicy_functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
newGetPolicy ::
  -- | 'functionName'
  Prelude.Text ->
  GetPolicy
newGetPolicy pFunctionName_ =
  GetPolicy'
    { qualifier = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a version or alias to get the policy for that resource.
getPolicy_qualifier :: Lens.Lens' GetPolicy (Prelude.Maybe Prelude.Text)
getPolicy_qualifier = Lens.lens (\GetPolicy' {qualifier} -> qualifier) (\s@GetPolicy' {} a -> s {qualifier = a} :: GetPolicy)

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
getPolicy_functionName :: Lens.Lens' GetPolicy Prelude.Text
getPolicy_functionName = Lens.lens (\GetPolicy' {functionName} -> functionName) (\s@GetPolicy' {} a -> s {functionName = a} :: GetPolicy)

instance Core.AWSRequest GetPolicy where
  type AWSResponse GetPolicy = GetPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Prelude.<$> (x Data..?> "Policy")
            Prelude.<*> (x Data..?> "RevisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPolicy where
  hashWithSalt _salt GetPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData GetPolicy where
  rnf GetPolicy' {..} =
    Prelude.rnf qualifier `Prelude.seq`
      Prelude.rnf functionName

instance Data.ToHeaders GetPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPolicy where
  toPath GetPolicy' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/policy"
      ]

instance Data.ToQuery GetPolicy where
  toQuery GetPolicy' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]

-- | /See:/ 'newGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | The resource-based policy.
    policy :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the current revision of the policy.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getPolicyResponse_policy' - The resource-based policy.
--
-- 'revisionId', 'getPolicyResponse_revisionId' - A unique identifier for the current revision of the policy.
--
-- 'httpStatus', 'getPolicyResponse_httpStatus' - The response's http status code.
newGetPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPolicyResponse
newGetPolicyResponse pHttpStatus_ =
  GetPolicyResponse'
    { policy = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource-based policy.
getPolicyResponse_policy :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Prelude.Text)
getPolicyResponse_policy = Lens.lens (\GetPolicyResponse' {policy} -> policy) (\s@GetPolicyResponse' {} a -> s {policy = a} :: GetPolicyResponse)

-- | A unique identifier for the current revision of the policy.
getPolicyResponse_revisionId :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Prelude.Text)
getPolicyResponse_revisionId = Lens.lens (\GetPolicyResponse' {revisionId} -> revisionId) (\s@GetPolicyResponse' {} a -> s {revisionId = a} :: GetPolicyResponse)

-- | The response's http status code.
getPolicyResponse_httpStatus :: Lens.Lens' GetPolicyResponse Prelude.Int
getPolicyResponse_httpStatus = Lens.lens (\GetPolicyResponse' {httpStatus} -> httpStatus) (\s@GetPolicyResponse' {} a -> s {httpStatus = a} :: GetPolicyResponse)

instance Prelude.NFData GetPolicyResponse where
  rnf GetPolicyResponse' {..} =
    Prelude.rnf policy `Prelude.seq`
      Prelude.rnf revisionId `Prelude.seq`
        Prelude.rnf httpStatus
