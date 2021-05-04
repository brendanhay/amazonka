{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WAFRegional.PutPermissionPolicy
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
-- Attaches an IAM policy to the specified resource. The only supported use
-- for this action is to share a RuleGroup across accounts.
--
-- The @PutPermissionPolicy@ is subject to the following restrictions:
--
-- -   You can attach only one policy with each @PutPermissionPolicy@
--     request.
--
-- -   The policy must include an @Effect@, @Action@ and @Principal@.
--
-- -   @Effect@ must specify @Allow@.
--
-- -   The @Action@ in the policy must be @waf:UpdateWebACL@,
--     @waf-regional:UpdateWebACL@, @waf:GetRuleGroup@ and
--     @waf-regional:GetRuleGroup@ . Any extra or wildcard actions in the
--     policy will be rejected.
--
-- -   The policy cannot include a @Resource@ parameter.
--
-- -   The ARN in the request must be a valid WAF RuleGroup ARN and the
--     RuleGroup must exist in the same region.
--
-- -   The user making the request must be the owner of the RuleGroup.
--
-- -   Your policy must be composed using IAM Policy version 2012-10-17.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies>.
--
-- An example of a valid policy parameter is shown in the Examples section
-- below.
module Network.AWS.WAFRegional.PutPermissionPolicy
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newPutPermissionPolicy' smart constructor.
data PutPermissionPolicy = PutPermissionPolicy'
  { -- | The Amazon Resource Name (ARN) of the RuleGroup to which you want to
    -- attach the policy.
    resourceArn :: Prelude.Text,
    -- | The policy to attach to the specified RuleGroup.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'policy', 'putPermissionPolicy_policy' - The policy to attach to the specified RuleGroup.
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

-- | The policy to attach to the specified RuleGroup.
putPermissionPolicy_policy :: Lens.Lens' PutPermissionPolicy Prelude.Text
putPermissionPolicy_policy = Lens.lens (\PutPermissionPolicy' {policy} -> policy) (\s@PutPermissionPolicy' {} a -> s {policy = a} :: PutPermissionPolicy)

instance Prelude.AWSRequest PutPermissionPolicy where
  type
    Rs PutPermissionPolicy =
      PutPermissionPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutPermissionPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutPermissionPolicy

instance Prelude.NFData PutPermissionPolicy

instance Prelude.ToHeaders PutPermissionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_Regional_20161128.PutPermissionPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutPermissionPolicy where
  toJSON PutPermissionPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Prelude..= resourceArn),
            Prelude.Just ("Policy" Prelude..= policy)
          ]
      )

instance Prelude.ToPath PutPermissionPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutPermissionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutPermissionPolicyResponse' smart constructor.
data PutPermissionPolicyResponse = PutPermissionPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData PutPermissionPolicyResponse
