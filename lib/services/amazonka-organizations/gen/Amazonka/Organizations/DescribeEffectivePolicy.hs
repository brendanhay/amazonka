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
-- Module      : Amazonka.Organizations.DescribeEffectivePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies-inheritance.html How Policy Inheritance Works>
-- in the /Organizations User Guide/.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- Amazon Web Services service.
module Amazonka.Organizations.DescribeEffectivePolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEffectivePolicy' smart constructor.
data DescribeEffectivePolicy = DescribeEffectivePolicy'
  { -- | When you\'re signed in as the management account, specify the ID of the
    -- account that you want details about. Specifying an organization root or
    -- organizational unit (OU) as the target is not supported.
    targetId :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { targetId =
        Prelude.Nothing,
      policyType = pPolicyType_
    }

-- | When you\'re signed in as the management account, specify the ID of the
-- account that you want details about. Specifying an organization root or
-- organizational unit (OU) as the target is not supported.
describeEffectivePolicy_targetId :: Lens.Lens' DescribeEffectivePolicy (Prelude.Maybe Prelude.Text)
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEffectivePolicyResponse'
            Prelude.<$> (x Data..?> "EffectivePolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEffectivePolicy where
  hashWithSalt _salt DescribeEffectivePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` policyType

instance Prelude.NFData DescribeEffectivePolicy where
  rnf DescribeEffectivePolicy' {..} =
    Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf policyType

instance Data.ToHeaders DescribeEffectivePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.DescribeEffectivePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEffectivePolicy where
  toJSON DescribeEffectivePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetId" Data..=) Prelude.<$> targetId,
            Prelude.Just ("PolicyType" Data..= policyType)
          ]
      )

instance Data.ToPath DescribeEffectivePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEffectivePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEffectivePolicyResponse' smart constructor.
data DescribeEffectivePolicyResponse = DescribeEffectivePolicyResponse'
  { -- | The contents of the effective policy.
    effectivePolicy :: Prelude.Maybe EffectivePolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeEffectivePolicyResponse
newDescribeEffectivePolicyResponse pHttpStatus_ =
  DescribeEffectivePolicyResponse'
    { effectivePolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The contents of the effective policy.
describeEffectivePolicyResponse_effectivePolicy :: Lens.Lens' DescribeEffectivePolicyResponse (Prelude.Maybe EffectivePolicy)
describeEffectivePolicyResponse_effectivePolicy = Lens.lens (\DescribeEffectivePolicyResponse' {effectivePolicy} -> effectivePolicy) (\s@DescribeEffectivePolicyResponse' {} a -> s {effectivePolicy = a} :: DescribeEffectivePolicyResponse)

-- | The response's http status code.
describeEffectivePolicyResponse_httpStatus :: Lens.Lens' DescribeEffectivePolicyResponse Prelude.Int
describeEffectivePolicyResponse_httpStatus = Lens.lens (\DescribeEffectivePolicyResponse' {httpStatus} -> httpStatus) (\s@DescribeEffectivePolicyResponse' {} a -> s {httpStatus = a} :: DescribeEffectivePolicyResponse)

instance
  Prelude.NFData
    DescribeEffectivePolicyResponse
  where
  rnf DescribeEffectivePolicyResponse' {..} =
    Prelude.rnf effectivePolicy
      `Prelude.seq` Prelude.rnf httpStatus
