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
-- Module      : Network.AWS.Organizations.DescribeEffectivePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the contents of the effective policy for specified policy type
-- and account. The effective policy is the aggregation of any policies of
-- the specified type that the account inherits, plus any policy of that
-- type that is directly attached to the account.
--
-- This operation applies only to policy types /other/ than service control
-- policies (SCPs).
--
-- For more information about policy inheritance, see
-- <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies-inheritance.html How Policy Inheritance Works>
-- in the /AWS Organizations User Guide/.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
module Network.AWS.Organizations.DescribeEffectivePolicy
  ( -- * Creating a Request
    DescribeEffectivePolicy (..),
    newDescribeEffectivePolicy,

    -- * Request Lenses
    describeEffectivePolicy_targetId,
    describeEffectivePolicy_policyType,

    -- * Destructuring the Response
    DescribeEffectivePolicyResponse (..),
    newDescribeEffectivePolicyResponse,

    -- * Response Lenses
    describeEffectivePolicyResponse_effectivePolicy,
    describeEffectivePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEffectivePolicy' smart constructor.
data DescribeEffectivePolicy = DescribeEffectivePolicy'
  { -- | When you\'re signed in as the management account, specify the ID of the
    -- account that you want details about. Specifying an organization root or
    -- organizational unit (OU) as the target is not supported.
    targetId :: Core.Maybe Core.Text,
    -- | The type of policy that you want information about. You can specify one
    -- of the following values:
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
    policyType :: EffectivePolicyType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEffectivePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetId', 'describeEffectivePolicy_targetId' - When you\'re signed in as the management account, specify the ID of the
-- account that you want details about. Specifying an organization root or
-- organizational unit (OU) as the target is not supported.
--
-- 'policyType', 'describeEffectivePolicy_policyType' - The type of policy that you want information about. You can specify one
-- of the following values:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
newDescribeEffectivePolicy ::
  -- | 'policyType'
  EffectivePolicyType ->
  DescribeEffectivePolicy
newDescribeEffectivePolicy pPolicyType_ =
  DescribeEffectivePolicy'
    { targetId = Core.Nothing,
      policyType = pPolicyType_
    }

-- | When you\'re signed in as the management account, specify the ID of the
-- account that you want details about. Specifying an organization root or
-- organizational unit (OU) as the target is not supported.
describeEffectivePolicy_targetId :: Lens.Lens' DescribeEffectivePolicy (Core.Maybe Core.Text)
describeEffectivePolicy_targetId = Lens.lens (\DescribeEffectivePolicy' {targetId} -> targetId) (\s@DescribeEffectivePolicy' {} a -> s {targetId = a} :: DescribeEffectivePolicy)

-- | The type of policy that you want information about. You can specify one
-- of the following values:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
describeEffectivePolicy_policyType :: Lens.Lens' DescribeEffectivePolicy EffectivePolicyType
describeEffectivePolicy_policyType = Lens.lens (\DescribeEffectivePolicy' {policyType} -> policyType) (\s@DescribeEffectivePolicy' {} a -> s {policyType = a} :: DescribeEffectivePolicy)

instance Core.AWSRequest DescribeEffectivePolicy where
  type
    AWSResponse DescribeEffectivePolicy =
      DescribeEffectivePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEffectivePolicyResponse'
            Core.<$> (x Core..?> "EffectivePolicy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEffectivePolicy

instance Core.NFData DescribeEffectivePolicy

instance Core.ToHeaders DescribeEffectivePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DescribeEffectivePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEffectivePolicy where
  toJSON DescribeEffectivePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TargetId" Core..=) Core.<$> targetId,
            Core.Just ("PolicyType" Core..= policyType)
          ]
      )

instance Core.ToPath DescribeEffectivePolicy where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEffectivePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEffectivePolicyResponse' smart constructor.
data DescribeEffectivePolicyResponse = DescribeEffectivePolicyResponse'
  { -- | The contents of the effective policy.
    effectivePolicy :: Core.Maybe EffectivePolicy,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEffectivePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effectivePolicy', 'describeEffectivePolicyResponse_effectivePolicy' - The contents of the effective policy.
--
-- 'httpStatus', 'describeEffectivePolicyResponse_httpStatus' - The response's http status code.
newDescribeEffectivePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEffectivePolicyResponse
newDescribeEffectivePolicyResponse pHttpStatus_ =
  DescribeEffectivePolicyResponse'
    { effectivePolicy =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The contents of the effective policy.
describeEffectivePolicyResponse_effectivePolicy :: Lens.Lens' DescribeEffectivePolicyResponse (Core.Maybe EffectivePolicy)
describeEffectivePolicyResponse_effectivePolicy = Lens.lens (\DescribeEffectivePolicyResponse' {effectivePolicy} -> effectivePolicy) (\s@DescribeEffectivePolicyResponse' {} a -> s {effectivePolicy = a} :: DescribeEffectivePolicyResponse)

-- | The response's http status code.
describeEffectivePolicyResponse_httpStatus :: Lens.Lens' DescribeEffectivePolicyResponse Core.Int
describeEffectivePolicyResponse_httpStatus = Lens.lens (\DescribeEffectivePolicyResponse' {httpStatus} -> httpStatus) (\s@DescribeEffectivePolicyResponse' {} a -> s {httpStatus = a} :: DescribeEffectivePolicyResponse)

instance Core.NFData DescribeEffectivePolicyResponse
