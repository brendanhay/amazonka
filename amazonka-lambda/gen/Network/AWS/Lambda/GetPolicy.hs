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
-- Module      : Network.AWS.Lambda.GetPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the
-- <https://docs.aws.amazon.com/lambda/latest/dg/access-control-resource-based.html resource-based IAM policy>
-- for a function, version, or alias.
module Network.AWS.Lambda.GetPolicy
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
    getPolicyResponse_revisionId,
    getPolicyResponse_policy,
    getPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { -- | Specify a version or alias to get the policy for that resource.
    qualifier :: Core.Maybe Core.Text,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
    --     (with alias).
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- You can append a version number or alias to any of the formats. The
    -- length constraint applies only to the full ARN. If you specify only the
    -- function name, it is limited to 64 characters in length.
    functionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
newGetPolicy ::
  -- | 'functionName'
  Core.Text ->
  GetPolicy
newGetPolicy pFunctionName_ =
  GetPolicy'
    { qualifier = Core.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a version or alias to get the policy for that resource.
getPolicy_qualifier :: Lens.Lens' GetPolicy (Core.Maybe Core.Text)
getPolicy_qualifier = Lens.lens (\GetPolicy' {qualifier} -> qualifier) (\s@GetPolicy' {} a -> s {qualifier = a} :: GetPolicy)

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
getPolicy_functionName :: Lens.Lens' GetPolicy Core.Text
getPolicy_functionName = Lens.lens (\GetPolicy' {functionName} -> functionName) (\s@GetPolicy' {} a -> s {functionName = a} :: GetPolicy)

instance Core.AWSRequest GetPolicy where
  type AWSResponse GetPolicy = GetPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Core.<$> (x Core..?> "RevisionId")
            Core.<*> (x Core..?> "Policy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPolicy

instance Core.NFData GetPolicy

instance Core.ToHeaders GetPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetPolicy where
  toPath GetPolicy' {..} =
    Core.mconcat
      [ "/2015-03-31/functions/",
        Core.toBS functionName,
        "/policy"
      ]

instance Core.ToQuery GetPolicy where
  toQuery GetPolicy' {..} =
    Core.mconcat ["Qualifier" Core.=: qualifier]

-- | /See:/ 'newGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | A unique identifier for the current revision of the policy.
    revisionId :: Core.Maybe Core.Text,
    -- | The resource-based policy.
    policy :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'getPolicyResponse_revisionId' - A unique identifier for the current revision of the policy.
--
-- 'policy', 'getPolicyResponse_policy' - The resource-based policy.
--
-- 'httpStatus', 'getPolicyResponse_httpStatus' - The response's http status code.
newGetPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPolicyResponse
newGetPolicyResponse pHttpStatus_ =
  GetPolicyResponse'
    { revisionId = Core.Nothing,
      policy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the current revision of the policy.
getPolicyResponse_revisionId :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.Text)
getPolicyResponse_revisionId = Lens.lens (\GetPolicyResponse' {revisionId} -> revisionId) (\s@GetPolicyResponse' {} a -> s {revisionId = a} :: GetPolicyResponse)

-- | The resource-based policy.
getPolicyResponse_policy :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.Text)
getPolicyResponse_policy = Lens.lens (\GetPolicyResponse' {policy} -> policy) (\s@GetPolicyResponse' {} a -> s {policy = a} :: GetPolicyResponse)

-- | The response's http status code.
getPolicyResponse_httpStatus :: Lens.Lens' GetPolicyResponse Core.Int
getPolicyResponse_httpStatus = Lens.lens (\GetPolicyResponse' {httpStatus} -> httpStatus) (\s@GetPolicyResponse' {} a -> s {httpStatus = a} :: GetPolicyResponse)

instance Core.NFData GetPolicyResponse
