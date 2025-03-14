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
-- Module      : Amazonka.WAFV2.PutPermissionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an IAM policy to the specified resource. Use this to share a
-- rule group across accounts.
--
-- You must be the owner of the rule group to perform this operation.
--
-- This action is subject to the following restrictions:
--
-- -   You can attach only one policy with each @PutPermissionPolicy@
--     request.
--
-- -   The ARN in the request must be a valid WAF RuleGroup ARN and the
--     rule group must exist in the same Region.
--
-- -   The user making the request must be the owner of the rule group.
module Amazonka.WAFV2.PutPermissionPolicy
  ( -- * Creating a Request
    PutPermissionPolicy (..),
    newPutPermissionPolicy,

    -- * Request Lenses
    putPermissionPolicy_resourceArn,
    putPermissionPolicy_policy,

    -- * Destructuring the Response
    PutPermissionPolicyResponse (..),
    newPutPermissionPolicyResponse,

    -- * Response Lenses
    putPermissionPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newPutPermissionPolicy' smart constructor.
data PutPermissionPolicy = PutPermissionPolicy'
  { -- | The Amazon Resource Name (ARN) of the RuleGroup to which you want to
    -- attach the policy.
    resourceArn :: Prelude.Text,
    -- | The policy to attach to the specified rule group.
    --
    -- The policy specifications must conform to the following:
    --
    -- -   The policy must be composed using IAM Policy version 2012-10-17 or
    --     version 2015-01-01.
    --
    -- -   The policy must include specifications for @Effect@, @Action@, and
    --     @Principal@.
    --
    -- -   @Effect@ must specify @Allow@.
    --
    -- -   @Action@ must specify @wafv2:CreateWebACL@, @wafv2:UpdateWebACL@,
    --     and @wafv2:PutFirewallManagerRuleGroups@ and may optionally specify
    --     @wafv2:GetRuleGroup@. WAF rejects any extra actions or wildcard
    --     actions in the policy.
    --
    -- -   The policy must not include a @Resource@ parameter.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies>.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPermissionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'putPermissionPolicy_resourceArn' - The Amazon Resource Name (ARN) of the RuleGroup to which you want to
-- attach the policy.
--
-- 'policy', 'putPermissionPolicy_policy' - The policy to attach to the specified rule group.
--
-- The policy specifications must conform to the following:
--
-- -   The policy must be composed using IAM Policy version 2012-10-17 or
--     version 2015-01-01.
--
-- -   The policy must include specifications for @Effect@, @Action@, and
--     @Principal@.
--
-- -   @Effect@ must specify @Allow@.
--
-- -   @Action@ must specify @wafv2:CreateWebACL@, @wafv2:UpdateWebACL@,
--     and @wafv2:PutFirewallManagerRuleGroups@ and may optionally specify
--     @wafv2:GetRuleGroup@. WAF rejects any extra actions or wildcard
--     actions in the policy.
--
-- -   The policy must not include a @Resource@ parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies>.
newPutPermissionPolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutPermissionPolicy
newPutPermissionPolicy pResourceArn_ pPolicy_ =
  PutPermissionPolicy'
    { resourceArn = pResourceArn_,
      policy = pPolicy_
    }

-- | The Amazon Resource Name (ARN) of the RuleGroup to which you want to
-- attach the policy.
putPermissionPolicy_resourceArn :: Lens.Lens' PutPermissionPolicy Prelude.Text
putPermissionPolicy_resourceArn = Lens.lens (\PutPermissionPolicy' {resourceArn} -> resourceArn) (\s@PutPermissionPolicy' {} a -> s {resourceArn = a} :: PutPermissionPolicy)

-- | The policy to attach to the specified rule group.
--
-- The policy specifications must conform to the following:
--
-- -   The policy must be composed using IAM Policy version 2012-10-17 or
--     version 2015-01-01.
--
-- -   The policy must include specifications for @Effect@, @Action@, and
--     @Principal@.
--
-- -   @Effect@ must specify @Allow@.
--
-- -   @Action@ must specify @wafv2:CreateWebACL@, @wafv2:UpdateWebACL@,
--     and @wafv2:PutFirewallManagerRuleGroups@ and may optionally specify
--     @wafv2:GetRuleGroup@. WAF rejects any extra actions or wildcard
--     actions in the policy.
--
-- -   The policy must not include a @Resource@ parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies>.
putPermissionPolicy_policy :: Lens.Lens' PutPermissionPolicy Prelude.Text
putPermissionPolicy_policy = Lens.lens (\PutPermissionPolicy' {policy} -> policy) (\s@PutPermissionPolicy' {} a -> s {policy = a} :: PutPermissionPolicy)

instance Core.AWSRequest PutPermissionPolicy where
  type
    AWSResponse PutPermissionPolicy =
      PutPermissionPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutPermissionPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutPermissionPolicy where
  hashWithSalt _salt PutPermissionPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutPermissionPolicy where
  rnf PutPermissionPolicy' {..} =
    Prelude.rnf resourceArn `Prelude.seq`
      Prelude.rnf policy

instance Data.ToHeaders PutPermissionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.PutPermissionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutPermissionPolicy where
  toJSON PutPermissionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("Policy" Data..= policy)
          ]
      )

instance Data.ToPath PutPermissionPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutPermissionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutPermissionPolicyResponse' smart constructor.
data PutPermissionPolicyResponse = PutPermissionPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPermissionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putPermissionPolicyResponse_httpStatus' - The response's http status code.
newPutPermissionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutPermissionPolicyResponse
newPutPermissionPolicyResponse pHttpStatus_ =
  PutPermissionPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putPermissionPolicyResponse_httpStatus :: Lens.Lens' PutPermissionPolicyResponse Prelude.Int
putPermissionPolicyResponse_httpStatus = Lens.lens (\PutPermissionPolicyResponse' {httpStatus} -> httpStatus) (\s@PutPermissionPolicyResponse' {} a -> s {httpStatus = a} :: PutPermissionPolicyResponse)

instance Prelude.NFData PutPermissionPolicyResponse where
  rnf PutPermissionPolicyResponse' {..} =
    Prelude.rnf httpStatus
