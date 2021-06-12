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
-- Module      : Network.AWS.WAF.GetPermissionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Returns the IAM policy attached to the RuleGroup.
module Network.AWS.WAF.GetPermissionPolicy
  ( -- * Creating a Request
    GetPermissionPolicy (..),
    newGetPermissionPolicy,

    -- * Request Lenses
    getPermissionPolicy_resourceArn,

    -- * Destructuring the Response
    GetPermissionPolicyResponse (..),
    newGetPermissionPolicyResponse,

    -- * Response Lenses
    getPermissionPolicyResponse_policy,
    getPermissionPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newGetPermissionPolicy' smart constructor.
data GetPermissionPolicy = GetPermissionPolicy'
  { -- | The Amazon Resource Name (ARN) of the RuleGroup for which you want to
    -- get the policy.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPermissionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getPermissionPolicy_resourceArn' - The Amazon Resource Name (ARN) of the RuleGroup for which you want to
-- get the policy.
newGetPermissionPolicy ::
  -- | 'resourceArn'
  Core.Text ->
  GetPermissionPolicy
newGetPermissionPolicy pResourceArn_ =
  GetPermissionPolicy' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the RuleGroup for which you want to
-- get the policy.
getPermissionPolicy_resourceArn :: Lens.Lens' GetPermissionPolicy Core.Text
getPermissionPolicy_resourceArn = Lens.lens (\GetPermissionPolicy' {resourceArn} -> resourceArn) (\s@GetPermissionPolicy' {} a -> s {resourceArn = a} :: GetPermissionPolicy)

instance Core.AWSRequest GetPermissionPolicy where
  type
    AWSResponse GetPermissionPolicy =
      GetPermissionPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPermissionPolicyResponse'
            Core.<$> (x Core..?> "Policy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPermissionPolicy

instance Core.NFData GetPermissionPolicy

instance Core.ToHeaders GetPermissionPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetPermissionPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetPermissionPolicy where
  toJSON GetPermissionPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ResourceArn" Core..= resourceArn)]
      )

instance Core.ToPath GetPermissionPolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetPermissionPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPermissionPolicyResponse' smart constructor.
data GetPermissionPolicyResponse = GetPermissionPolicyResponse'
  { -- | The IAM policy attached to the specified RuleGroup.
    policy :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPermissionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getPermissionPolicyResponse_policy' - The IAM policy attached to the specified RuleGroup.
--
-- 'httpStatus', 'getPermissionPolicyResponse_httpStatus' - The response's http status code.
newGetPermissionPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPermissionPolicyResponse
newGetPermissionPolicyResponse pHttpStatus_ =
  GetPermissionPolicyResponse'
    { policy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IAM policy attached to the specified RuleGroup.
getPermissionPolicyResponse_policy :: Lens.Lens' GetPermissionPolicyResponse (Core.Maybe Core.Text)
getPermissionPolicyResponse_policy = Lens.lens (\GetPermissionPolicyResponse' {policy} -> policy) (\s@GetPermissionPolicyResponse' {} a -> s {policy = a} :: GetPermissionPolicyResponse)

-- | The response's http status code.
getPermissionPolicyResponse_httpStatus :: Lens.Lens' GetPermissionPolicyResponse Core.Int
getPermissionPolicyResponse_httpStatus = Lens.lens (\GetPermissionPolicyResponse' {httpStatus} -> httpStatus) (\s@GetPermissionPolicyResponse' {} a -> s {httpStatus = a} :: GetPermissionPolicyResponse)

instance Core.NFData GetPermissionPolicyResponse
