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
-- Module      : Network.AWS.DLM.UpdateLifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified lifecycle policy.
module Network.AWS.DLM.UpdateLifecyclePolicy
  ( -- * Creating a Request
    UpdateLifecyclePolicy (..),
    newUpdateLifecyclePolicy,

    -- * Request Lenses
    updateLifecyclePolicy_state,
    updateLifecyclePolicy_policyDetails,
    updateLifecyclePolicy_executionRoleArn,
    updateLifecyclePolicy_description,
    updateLifecyclePolicy_policyId,

    -- * Destructuring the Response
    UpdateLifecyclePolicyResponse (..),
    newUpdateLifecyclePolicyResponse,

    -- * Response Lenses
    updateLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DLM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLifecyclePolicy' smart constructor.
data UpdateLifecyclePolicy = UpdateLifecyclePolicy'
  { -- | The desired activation state of the lifecycle policy after creation.
    state :: Prelude.Maybe SettablePolicyStateValues,
    -- | The configuration of the lifecycle policy. You cannot update the policy
    -- type or the resource type.
    policyDetails :: Prelude.Maybe PolicyDetails,
    -- | The Amazon Resource Name (ARN) of the IAM role used to run the
    -- operations specified by the lifecycle policy.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the lifecycle policy.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'state', 'updateLifecyclePolicy_state' - The desired activation state of the lifecycle policy after creation.
--
-- 'policyDetails', 'updateLifecyclePolicy_policyDetails' - The configuration of the lifecycle policy. You cannot update the policy
-- type or the resource type.
--
-- 'executionRoleArn', 'updateLifecyclePolicy_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role used to run the
-- operations specified by the lifecycle policy.
--
-- 'description', 'updateLifecyclePolicy_description' - A description of the lifecycle policy.
--
-- 'policyId', 'updateLifecyclePolicy_policyId' - The identifier of the lifecycle policy.
newUpdateLifecyclePolicy ::
  -- | 'policyId'
  Prelude.Text ->
  UpdateLifecyclePolicy
newUpdateLifecyclePolicy pPolicyId_ =
  UpdateLifecyclePolicy'
    { state = Prelude.Nothing,
      policyDetails = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      description = Prelude.Nothing,
      policyId = pPolicyId_
    }

-- | The desired activation state of the lifecycle policy after creation.
updateLifecyclePolicy_state :: Lens.Lens' UpdateLifecyclePolicy (Prelude.Maybe SettablePolicyStateValues)
updateLifecyclePolicy_state = Lens.lens (\UpdateLifecyclePolicy' {state} -> state) (\s@UpdateLifecyclePolicy' {} a -> s {state = a} :: UpdateLifecyclePolicy)

-- | The configuration of the lifecycle policy. You cannot update the policy
-- type or the resource type.
updateLifecyclePolicy_policyDetails :: Lens.Lens' UpdateLifecyclePolicy (Prelude.Maybe PolicyDetails)
updateLifecyclePolicy_policyDetails = Lens.lens (\UpdateLifecyclePolicy' {policyDetails} -> policyDetails) (\s@UpdateLifecyclePolicy' {} a -> s {policyDetails = a} :: UpdateLifecyclePolicy)

-- | The Amazon Resource Name (ARN) of the IAM role used to run the
-- operations specified by the lifecycle policy.
updateLifecyclePolicy_executionRoleArn :: Lens.Lens' UpdateLifecyclePolicy (Prelude.Maybe Prelude.Text)
updateLifecyclePolicy_executionRoleArn = Lens.lens (\UpdateLifecyclePolicy' {executionRoleArn} -> executionRoleArn) (\s@UpdateLifecyclePolicy' {} a -> s {executionRoleArn = a} :: UpdateLifecyclePolicy)

-- | A description of the lifecycle policy.
updateLifecyclePolicy_description :: Lens.Lens' UpdateLifecyclePolicy (Prelude.Maybe Prelude.Text)
updateLifecyclePolicy_description = Lens.lens (\UpdateLifecyclePolicy' {description} -> description) (\s@UpdateLifecyclePolicy' {} a -> s {description = a} :: UpdateLifecyclePolicy)

-- | The identifier of the lifecycle policy.
updateLifecyclePolicy_policyId :: Lens.Lens' UpdateLifecyclePolicy Prelude.Text
updateLifecyclePolicy_policyId = Lens.lens (\UpdateLifecyclePolicy' {policyId} -> policyId) (\s@UpdateLifecyclePolicy' {} a -> s {policyId = a} :: UpdateLifecyclePolicy)

instance Core.AWSRequest UpdateLifecyclePolicy where
  type
    AWSResponse UpdateLifecyclePolicy =
      UpdateLifecyclePolicyResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLifecyclePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLifecyclePolicy

instance Prelude.NFData UpdateLifecyclePolicy

instance Core.ToHeaders UpdateLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateLifecyclePolicy where
  toJSON UpdateLifecyclePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("State" Core..=) Prelude.<$> state,
            ("PolicyDetails" Core..=) Prelude.<$> policyDetails,
            ("ExecutionRoleArn" Core..=)
              Prelude.<$> executionRoleArn,
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateLifecyclePolicy where
  toPath UpdateLifecyclePolicy' {..} =
    Prelude.mconcat ["/policies/", Core.toBS policyId]

instance Core.ToQuery UpdateLifecyclePolicy where
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

instance Prelude.NFData UpdateLifecyclePolicyResponse
