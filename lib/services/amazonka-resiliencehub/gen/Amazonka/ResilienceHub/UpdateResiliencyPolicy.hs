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
-- Module      : Amazonka.ResilienceHub.UpdateResiliencyPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a resiliency policy.
module Amazonka.ResilienceHub.UpdateResiliencyPolicy
  ( -- * Creating a Request
    UpdateResiliencyPolicy (..),
    newUpdateResiliencyPolicy,

    -- * Request Lenses
    updateResiliencyPolicy_policyName,
    updateResiliencyPolicy_policy,
    updateResiliencyPolicy_dataLocationConstraint,
    updateResiliencyPolicy_tier,
    updateResiliencyPolicy_policyDescription,
    updateResiliencyPolicy_policyArn,

    -- * Destructuring the Response
    UpdateResiliencyPolicyResponse (..),
    newUpdateResiliencyPolicyResponse,

    -- * Response Lenses
    updateResiliencyPolicyResponse_httpStatus,
    updateResiliencyPolicyResponse_policy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResiliencyPolicy' smart constructor.
data UpdateResiliencyPolicy = UpdateResiliencyPolicy'
  { -- | The name of the policy
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The type of resiliency policy to be created, including the recovery time
    -- objective (RTO) and recovery point objective (RPO) in seconds.
    policy :: Prelude.Maybe (Prelude.HashMap DisruptionType FailurePolicy),
    -- | Specifies a high-level geographical location constraint for where your
    -- resilience policy data can be stored.
    dataLocationConstraint :: Prelude.Maybe DataLocationConstraint,
    -- | The tier for this resiliency policy, ranging from the highest severity
    -- (@MissionCritical@) to lowest (@NonCritical@).
    tier :: Prelude.Maybe ResiliencyPolicyTier,
    -- | The description for the policy.
    policyDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
    -- this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResiliencyPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'updateResiliencyPolicy_policyName' - The name of the policy
--
-- 'policy', 'updateResiliencyPolicy_policy' - The type of resiliency policy to be created, including the recovery time
-- objective (RTO) and recovery point objective (RPO) in seconds.
--
-- 'dataLocationConstraint', 'updateResiliencyPolicy_dataLocationConstraint' - Specifies a high-level geographical location constraint for where your
-- resilience policy data can be stored.
--
-- 'tier', 'updateResiliencyPolicy_tier' - The tier for this resiliency policy, ranging from the highest severity
-- (@MissionCritical@) to lowest (@NonCritical@).
--
-- 'policyDescription', 'updateResiliencyPolicy_policyDescription' - The description for the policy.
--
-- 'policyArn', 'updateResiliencyPolicy_policyArn' - The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newUpdateResiliencyPolicy ::
  -- | 'policyArn'
  Prelude.Text ->
  UpdateResiliencyPolicy
newUpdateResiliencyPolicy pPolicyArn_ =
  UpdateResiliencyPolicy'
    { policyName =
        Prelude.Nothing,
      policy = Prelude.Nothing,
      dataLocationConstraint = Prelude.Nothing,
      tier = Prelude.Nothing,
      policyDescription = Prelude.Nothing,
      policyArn = pPolicyArn_
    }

-- | The name of the policy
updateResiliencyPolicy_policyName :: Lens.Lens' UpdateResiliencyPolicy (Prelude.Maybe Prelude.Text)
updateResiliencyPolicy_policyName = Lens.lens (\UpdateResiliencyPolicy' {policyName} -> policyName) (\s@UpdateResiliencyPolicy' {} a -> s {policyName = a} :: UpdateResiliencyPolicy)

-- | The type of resiliency policy to be created, including the recovery time
-- objective (RTO) and recovery point objective (RPO) in seconds.
updateResiliencyPolicy_policy :: Lens.Lens' UpdateResiliencyPolicy (Prelude.Maybe (Prelude.HashMap DisruptionType FailurePolicy))
updateResiliencyPolicy_policy = Lens.lens (\UpdateResiliencyPolicy' {policy} -> policy) (\s@UpdateResiliencyPolicy' {} a -> s {policy = a} :: UpdateResiliencyPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a high-level geographical location constraint for where your
-- resilience policy data can be stored.
updateResiliencyPolicy_dataLocationConstraint :: Lens.Lens' UpdateResiliencyPolicy (Prelude.Maybe DataLocationConstraint)
updateResiliencyPolicy_dataLocationConstraint = Lens.lens (\UpdateResiliencyPolicy' {dataLocationConstraint} -> dataLocationConstraint) (\s@UpdateResiliencyPolicy' {} a -> s {dataLocationConstraint = a} :: UpdateResiliencyPolicy)

-- | The tier for this resiliency policy, ranging from the highest severity
-- (@MissionCritical@) to lowest (@NonCritical@).
updateResiliencyPolicy_tier :: Lens.Lens' UpdateResiliencyPolicy (Prelude.Maybe ResiliencyPolicyTier)
updateResiliencyPolicy_tier = Lens.lens (\UpdateResiliencyPolicy' {tier} -> tier) (\s@UpdateResiliencyPolicy' {} a -> s {tier = a} :: UpdateResiliencyPolicy)

-- | The description for the policy.
updateResiliencyPolicy_policyDescription :: Lens.Lens' UpdateResiliencyPolicy (Prelude.Maybe Prelude.Text)
updateResiliencyPolicy_policyDescription = Lens.lens (\UpdateResiliencyPolicy' {policyDescription} -> policyDescription) (\s@UpdateResiliencyPolicy' {} a -> s {policyDescription = a} :: UpdateResiliencyPolicy)

-- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
updateResiliencyPolicy_policyArn :: Lens.Lens' UpdateResiliencyPolicy Prelude.Text
updateResiliencyPolicy_policyArn = Lens.lens (\UpdateResiliencyPolicy' {policyArn} -> policyArn) (\s@UpdateResiliencyPolicy' {} a -> s {policyArn = a} :: UpdateResiliencyPolicy)

instance Core.AWSRequest UpdateResiliencyPolicy where
  type
    AWSResponse UpdateResiliencyPolicy =
      UpdateResiliencyPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResiliencyPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "policy")
      )

instance Prelude.Hashable UpdateResiliencyPolicy where
  hashWithSalt _salt UpdateResiliencyPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` dataLocationConstraint
      `Prelude.hashWithSalt` tier
      `Prelude.hashWithSalt` policyDescription
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData UpdateResiliencyPolicy where
  rnf UpdateResiliencyPolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf dataLocationConstraint
      `Prelude.seq` Prelude.rnf tier
      `Prelude.seq` Prelude.rnf policyDescription
      `Prelude.seq` Prelude.rnf policyArn

instance Core.ToHeaders UpdateResiliencyPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateResiliencyPolicy where
  toJSON UpdateResiliencyPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("policyName" Core..=) Prelude.<$> policyName,
            ("policy" Core..=) Prelude.<$> policy,
            ("dataLocationConstraint" Core..=)
              Prelude.<$> dataLocationConstraint,
            ("tier" Core..=) Prelude.<$> tier,
            ("policyDescription" Core..=)
              Prelude.<$> policyDescription,
            Prelude.Just ("policyArn" Core..= policyArn)
          ]
      )

instance Core.ToPath UpdateResiliencyPolicy where
  toPath = Prelude.const "/update-resiliency-policy"

instance Core.ToQuery UpdateResiliencyPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResiliencyPolicyResponse' smart constructor.
data UpdateResiliencyPolicyResponse = UpdateResiliencyPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The type of resiliency policy that was updated, including the recovery
    -- time objective (RTO) and recovery point objective (RPO) in seconds.
    policy :: ResiliencyPolicy
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResiliencyPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResiliencyPolicyResponse_httpStatus' - The response's http status code.
--
-- 'policy', 'updateResiliencyPolicyResponse_policy' - The type of resiliency policy that was updated, including the recovery
-- time objective (RTO) and recovery point objective (RPO) in seconds.
newUpdateResiliencyPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policy'
  ResiliencyPolicy ->
  UpdateResiliencyPolicyResponse
newUpdateResiliencyPolicyResponse
  pHttpStatus_
  pPolicy_ =
    UpdateResiliencyPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        policy = pPolicy_
      }

-- | The response's http status code.
updateResiliencyPolicyResponse_httpStatus :: Lens.Lens' UpdateResiliencyPolicyResponse Prelude.Int
updateResiliencyPolicyResponse_httpStatus = Lens.lens (\UpdateResiliencyPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateResiliencyPolicyResponse' {} a -> s {httpStatus = a} :: UpdateResiliencyPolicyResponse)

-- | The type of resiliency policy that was updated, including the recovery
-- time objective (RTO) and recovery point objective (RPO) in seconds.
updateResiliencyPolicyResponse_policy :: Lens.Lens' UpdateResiliencyPolicyResponse ResiliencyPolicy
updateResiliencyPolicyResponse_policy = Lens.lens (\UpdateResiliencyPolicyResponse' {policy} -> policy) (\s@UpdateResiliencyPolicyResponse' {} a -> s {policy = a} :: UpdateResiliencyPolicyResponse)

instance
  Prelude.NFData
    UpdateResiliencyPolicyResponse
  where
  rnf UpdateResiliencyPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policy
