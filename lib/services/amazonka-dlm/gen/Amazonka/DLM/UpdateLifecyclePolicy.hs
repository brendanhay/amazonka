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
-- Module      : Amazonka.DLM.UpdateLifecyclePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified lifecycle policy.
--
-- For more information about updating a policy, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/view-modify-delete.html#modify Modify lifecycle policies>.
module Amazonka.DLM.UpdateLifecyclePolicy
  ( -- * Creating a Request
    UpdateLifecyclePolicy (..),
    newUpdateLifecyclePolicy,

    -- * Request Lenses
    updateLifecyclePolicy_policyDetails,
    updateLifecyclePolicy_state,
    updateLifecyclePolicy_description,
    updateLifecyclePolicy_executionRoleArn,
    updateLifecyclePolicy_policyId,

    -- * Destructuring the Response
    UpdateLifecyclePolicyResponse (..),
    newUpdateLifecyclePolicyResponse,

    -- * Response Lenses
    updateLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLifecyclePolicy' smart constructor.
data UpdateLifecyclePolicy = UpdateLifecyclePolicy'
  { -- | The configuration of the lifecycle policy. You cannot update the policy
    -- type or the resource type.
    policyDetails :: Prelude.Maybe PolicyDetails,
    -- | The desired activation state of the lifecycle policy after creation.
    state :: Prelude.Maybe SettablePolicyStateValues,
    -- | A description of the lifecycle policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used to run the
    -- operations specified by the lifecycle policy.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the lifecycle policy.
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyDetails', 'updateLifecyclePolicy_policyDetails' - The configuration of the lifecycle policy. You cannot update the policy
-- type or the resource type.
--
-- 'state', 'updateLifecyclePolicy_state' - The desired activation state of the lifecycle policy after creation.
--
-- 'description', 'updateLifecyclePolicy_description' - A description of the lifecycle policy.
--
-- 'executionRoleArn', 'updateLifecyclePolicy_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role used to run the
-- operations specified by the lifecycle policy.
--
-- 'policyId', 'updateLifecyclePolicy_policyId' - The identifier of the lifecycle policy.
newUpdateLifecyclePolicy ::
  -- | 'policyId'
  Prelude.Text ->
  UpdateLifecyclePolicy
newUpdateLifecyclePolicy pPolicyId_ =
  UpdateLifecyclePolicy'
    { policyDetails =
        Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      policyId = pPolicyId_
    }

-- | The configuration of the lifecycle policy. You cannot update the policy
-- type or the resource type.
updateLifecyclePolicy_policyDetails :: Lens.Lens' UpdateLifecyclePolicy (Prelude.Maybe PolicyDetails)
updateLifecyclePolicy_policyDetails = Lens.lens (\UpdateLifecyclePolicy' {policyDetails} -> policyDetails) (\s@UpdateLifecyclePolicy' {} a -> s {policyDetails = a} :: UpdateLifecyclePolicy)

-- | The desired activation state of the lifecycle policy after creation.
updateLifecyclePolicy_state :: Lens.Lens' UpdateLifecyclePolicy (Prelude.Maybe SettablePolicyStateValues)
updateLifecyclePolicy_state = Lens.lens (\UpdateLifecyclePolicy' {state} -> state) (\s@UpdateLifecyclePolicy' {} a -> s {state = a} :: UpdateLifecyclePolicy)

-- | A description of the lifecycle policy.
updateLifecyclePolicy_description :: Lens.Lens' UpdateLifecyclePolicy (Prelude.Maybe Prelude.Text)
updateLifecyclePolicy_description = Lens.lens (\UpdateLifecyclePolicy' {description} -> description) (\s@UpdateLifecyclePolicy' {} a -> s {description = a} :: UpdateLifecyclePolicy)

-- | The Amazon Resource Name (ARN) of the IAM role used to run the
-- operations specified by the lifecycle policy.
updateLifecyclePolicy_executionRoleArn :: Lens.Lens' UpdateLifecyclePolicy (Prelude.Maybe Prelude.Text)
updateLifecyclePolicy_executionRoleArn = Lens.lens (\UpdateLifecyclePolicy' {executionRoleArn} -> executionRoleArn) (\s@UpdateLifecyclePolicy' {} a -> s {executionRoleArn = a} :: UpdateLifecyclePolicy)

-- | The identifier of the lifecycle policy.
updateLifecyclePolicy_policyId :: Lens.Lens' UpdateLifecyclePolicy Prelude.Text
updateLifecyclePolicy_policyId = Lens.lens (\UpdateLifecyclePolicy' {policyId} -> policyId) (\s@UpdateLifecyclePolicy' {} a -> s {policyId = a} :: UpdateLifecyclePolicy)

instance Core.AWSRequest UpdateLifecyclePolicy where
  type
    AWSResponse UpdateLifecyclePolicy =
      UpdateLifecyclePolicyResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLifecyclePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLifecyclePolicy where
  hashWithSalt _salt UpdateLifecyclePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyDetails
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` policyId

instance Prelude.NFData UpdateLifecyclePolicy where
  rnf UpdateLifecyclePolicy' {..} =
    Prelude.rnf policyDetails
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf policyId

instance Data.ToHeaders UpdateLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLifecyclePolicy where
  toJSON UpdateLifecyclePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PolicyDetails" Data..=) Prelude.<$> policyDetails,
            ("State" Data..=) Prelude.<$> state,
            ("Description" Data..=) Prelude.<$> description,
            ("ExecutionRoleArn" Data..=)
              Prelude.<$> executionRoleArn
          ]
      )

instance Data.ToPath UpdateLifecyclePolicy where
  toPath UpdateLifecyclePolicy' {..} =
    Prelude.mconcat ["/policies/", Data.toBS policyId]

instance Data.ToQuery UpdateLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLifecyclePolicyResponse' smart constructor.
data UpdateLifecyclePolicyResponse = UpdateLifecyclePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLifecyclePolicyResponse_httpStatus' - The response's http status code.
newUpdateLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLifecyclePolicyResponse
newUpdateLifecyclePolicyResponse pHttpStatus_ =
  UpdateLifecyclePolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateLifecyclePolicyResponse_httpStatus :: Lens.Lens' UpdateLifecyclePolicyResponse Prelude.Int
updateLifecyclePolicyResponse_httpStatus = Lens.lens (\UpdateLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: UpdateLifecyclePolicyResponse)

instance Prelude.NFData UpdateLifecyclePolicyResponse where
  rnf UpdateLifecyclePolicyResponse' {..} =
    Prelude.rnf httpStatus
