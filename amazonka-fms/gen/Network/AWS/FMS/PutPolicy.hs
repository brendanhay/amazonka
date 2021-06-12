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
-- Module      : Network.AWS.FMS.PutPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager policy.
--
-- Firewall Manager provides the following types of policies:
--
-- -   An AWS WAF policy (type WAFV2), which defines rule groups to run
--     first in the corresponding AWS WAF web ACL and rule groups to run
--     last in the web ACL.
--
-- -   An AWS WAF Classic policy (type WAF), which defines a rule group.
--
-- -   A Shield Advanced policy, which applies Shield Advanced protection
--     to specified accounts and resources.
--
-- -   A security group policy, which manages VPC security groups across
--     your AWS organization.
--
-- -   An AWS Network Firewall policy, which provides firewall rules to
--     filter network traffic in specified Amazon VPCs.
--
-- Each policy is specific to one of the types. If you want to enforce more
-- than one policy type across accounts, create multiple policies. You can
-- create multiple policies for each type.
--
-- You must be subscribed to Shield Advanced to create a Shield Advanced
-- policy. For more information about subscribing to Shield Advanced, see
-- <https://docs.aws.amazon.com/waf/latest/DDOSAPIReference/API_CreateSubscription.html CreateSubscription>.
module Network.AWS.FMS.PutPolicy
  ( -- * Creating a Request
    PutPolicy (..),
    newPutPolicy,

    -- * Request Lenses
    putPolicy_tagList,
    putPolicy_policy,

    -- * Destructuring the Response
    PutPolicyResponse (..),
    newPutPolicyResponse,

    -- * Response Lenses
    putPolicyResponse_policy,
    putPolicyResponse_policyArn,
    putPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutPolicy' smart constructor.
data PutPolicy = PutPolicy'
  { -- | The tags to add to the AWS resource.
    tagList :: Core.Maybe [Tag],
    -- | The details of the AWS Firewall Manager policy to be created.
    policy :: Policy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'putPolicy_tagList' - The tags to add to the AWS resource.
--
-- 'policy', 'putPolicy_policy' - The details of the AWS Firewall Manager policy to be created.
newPutPolicy ::
  -- | 'policy'
  Policy ->
  PutPolicy
newPutPolicy pPolicy_ =
  PutPolicy'
    { tagList = Core.Nothing,
      policy = pPolicy_
    }

-- | The tags to add to the AWS resource.
putPolicy_tagList :: Lens.Lens' PutPolicy (Core.Maybe [Tag])
putPolicy_tagList = Lens.lens (\PutPolicy' {tagList} -> tagList) (\s@PutPolicy' {} a -> s {tagList = a} :: PutPolicy) Core.. Lens.mapping Lens._Coerce

-- | The details of the AWS Firewall Manager policy to be created.
putPolicy_policy :: Lens.Lens' PutPolicy Policy
putPolicy_policy = Lens.lens (\PutPolicy' {policy} -> policy) (\s@PutPolicy' {} a -> s {policy = a} :: PutPolicy)

instance Core.AWSRequest PutPolicy where
  type AWSResponse PutPolicy = PutPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutPolicyResponse'
            Core.<$> (x Core..?> "Policy")
            Core.<*> (x Core..?> "PolicyArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutPolicy

instance Core.NFData PutPolicy

instance Core.ToHeaders PutPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSFMS_20180101.PutPolicy" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutPolicy where
  toJSON PutPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TagList" Core..=) Core.<$> tagList,
            Core.Just ("Policy" Core..= policy)
          ]
      )

instance Core.ToPath PutPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutPolicyResponse' smart constructor.
data PutPolicyResponse = PutPolicyResponse'
  { -- | The details of the AWS Firewall Manager policy.
    policy :: Core.Maybe Policy,
    -- | The Amazon Resource Name (ARN) of the policy.
    policyArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putPolicyResponse_policy' - The details of the AWS Firewall Manager policy.
--
-- 'policyArn', 'putPolicyResponse_policyArn' - The Amazon Resource Name (ARN) of the policy.
--
-- 'httpStatus', 'putPolicyResponse_httpStatus' - The response's http status code.
newPutPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutPolicyResponse
newPutPolicyResponse pHttpStatus_ =
  PutPolicyResponse'
    { policy = Core.Nothing,
      policyArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the AWS Firewall Manager policy.
putPolicyResponse_policy :: Lens.Lens' PutPolicyResponse (Core.Maybe Policy)
putPolicyResponse_policy = Lens.lens (\PutPolicyResponse' {policy} -> policy) (\s@PutPolicyResponse' {} a -> s {policy = a} :: PutPolicyResponse)

-- | The Amazon Resource Name (ARN) of the policy.
putPolicyResponse_policyArn :: Lens.Lens' PutPolicyResponse (Core.Maybe Core.Text)
putPolicyResponse_policyArn = Lens.lens (\PutPolicyResponse' {policyArn} -> policyArn) (\s@PutPolicyResponse' {} a -> s {policyArn = a} :: PutPolicyResponse)

-- | The response's http status code.
putPolicyResponse_httpStatus :: Lens.Lens' PutPolicyResponse Core.Int
putPolicyResponse_httpStatus = Lens.lens (\PutPolicyResponse' {httpStatus} -> httpStatus) (\s@PutPolicyResponse' {} a -> s {httpStatus = a} :: PutPolicyResponse)

instance Core.NFData PutPolicyResponse
