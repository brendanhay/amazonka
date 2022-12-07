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
-- Module      : Amazonka.DLM.CreateLifecyclePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy to manage the lifecycle of the specified Amazon Web
-- Services resources. You can create up to 100 lifecycle policies.
module Amazonka.DLM.CreateLifecyclePolicy
  ( -- * Creating a Request
    CreateLifecyclePolicy (..),
    newCreateLifecyclePolicy,

    -- * Request Lenses
    createLifecyclePolicy_tags,
    createLifecyclePolicy_executionRoleArn,
    createLifecyclePolicy_description,
    createLifecyclePolicy_state,
    createLifecyclePolicy_policyDetails,

    -- * Destructuring the Response
    CreateLifecyclePolicyResponse (..),
    newCreateLifecyclePolicyResponse,

    -- * Response Lenses
    createLifecyclePolicyResponse_policyId,
    createLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLifecyclePolicy' smart constructor.
data CreateLifecyclePolicy = CreateLifecyclePolicy'
  { -- | The tags to apply to the lifecycle policy during creation.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the IAM role used to run the
    -- operations specified by the lifecycle policy.
    executionRoleArn :: Prelude.Text,
    -- | A description of the lifecycle policy. The characters ^[0-9A-Za-z _-]+$
    -- are supported.
    description :: Prelude.Text,
    -- | The desired activation state of the lifecycle policy after creation.
    state :: SettablePolicyStateValues,
    -- | The configuration details of the lifecycle policy.
    policyDetails :: PolicyDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLifecyclePolicy_tags' - The tags to apply to the lifecycle policy during creation.
--
-- 'executionRoleArn', 'createLifecyclePolicy_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role used to run the
-- operations specified by the lifecycle policy.
--
-- 'description', 'createLifecyclePolicy_description' - A description of the lifecycle policy. The characters ^[0-9A-Za-z _-]+$
-- are supported.
--
-- 'state', 'createLifecyclePolicy_state' - The desired activation state of the lifecycle policy after creation.
--
-- 'policyDetails', 'createLifecyclePolicy_policyDetails' - The configuration details of the lifecycle policy.
newCreateLifecyclePolicy ::
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'state'
  SettablePolicyStateValues ->
  -- | 'policyDetails'
  PolicyDetails ->
  CreateLifecyclePolicy
newCreateLifecyclePolicy
  pExecutionRoleArn_
  pDescription_
  pState_
  pPolicyDetails_ =
    CreateLifecyclePolicy'
      { tags = Prelude.Nothing,
        executionRoleArn = pExecutionRoleArn_,
        description = pDescription_,
        state = pState_,
        policyDetails = pPolicyDetails_
      }

-- | The tags to apply to the lifecycle policy during creation.
createLifecyclePolicy_tags :: Lens.Lens' CreateLifecyclePolicy (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLifecyclePolicy_tags = Lens.lens (\CreateLifecyclePolicy' {tags} -> tags) (\s@CreateLifecyclePolicy' {} a -> s {tags = a} :: CreateLifecyclePolicy) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the IAM role used to run the
-- operations specified by the lifecycle policy.
createLifecyclePolicy_executionRoleArn :: Lens.Lens' CreateLifecyclePolicy Prelude.Text
createLifecyclePolicy_executionRoleArn = Lens.lens (\CreateLifecyclePolicy' {executionRoleArn} -> executionRoleArn) (\s@CreateLifecyclePolicy' {} a -> s {executionRoleArn = a} :: CreateLifecyclePolicy)

-- | A description of the lifecycle policy. The characters ^[0-9A-Za-z _-]+$
-- are supported.
createLifecyclePolicy_description :: Lens.Lens' CreateLifecyclePolicy Prelude.Text
createLifecyclePolicy_description = Lens.lens (\CreateLifecyclePolicy' {description} -> description) (\s@CreateLifecyclePolicy' {} a -> s {description = a} :: CreateLifecyclePolicy)

-- | The desired activation state of the lifecycle policy after creation.
createLifecyclePolicy_state :: Lens.Lens' CreateLifecyclePolicy SettablePolicyStateValues
createLifecyclePolicy_state = Lens.lens (\CreateLifecyclePolicy' {state} -> state) (\s@CreateLifecyclePolicy' {} a -> s {state = a} :: CreateLifecyclePolicy)

-- | The configuration details of the lifecycle policy.
createLifecyclePolicy_policyDetails :: Lens.Lens' CreateLifecyclePolicy PolicyDetails
createLifecyclePolicy_policyDetails = Lens.lens (\CreateLifecyclePolicy' {policyDetails} -> policyDetails) (\s@CreateLifecyclePolicy' {} a -> s {policyDetails = a} :: CreateLifecyclePolicy)

instance Core.AWSRequest CreateLifecyclePolicy where
  type
    AWSResponse CreateLifecyclePolicy =
      CreateLifecyclePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLifecyclePolicyResponse'
            Prelude.<$> (x Data..?> "PolicyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLifecyclePolicy where
  hashWithSalt _salt CreateLifecyclePolicy' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` policyDetails

instance Prelude.NFData CreateLifecyclePolicy where
  rnf CreateLifecyclePolicy' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf policyDetails

instance Data.ToHeaders CreateLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLifecyclePolicy where
  toJSON CreateLifecyclePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ExecutionRoleArn" Data..= executionRoleArn),
            Prelude.Just ("Description" Data..= description),
            Prelude.Just ("State" Data..= state),
            Prelude.Just
              ("PolicyDetails" Data..= policyDetails)
          ]
      )

instance Data.ToPath CreateLifecyclePolicy where
  toPath = Prelude.const "/policies"

instance Data.ToQuery CreateLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLifecyclePolicyResponse' smart constructor.
data CreateLifecyclePolicyResponse = CreateLifecyclePolicyResponse'
  { -- | The identifier of the lifecycle policy.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'createLifecyclePolicyResponse_policyId' - The identifier of the lifecycle policy.
--
-- 'httpStatus', 'createLifecyclePolicyResponse_httpStatus' - The response's http status code.
newCreateLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLifecyclePolicyResponse
newCreateLifecyclePolicyResponse pHttpStatus_ =
  CreateLifecyclePolicyResponse'
    { policyId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the lifecycle policy.
createLifecyclePolicyResponse_policyId :: Lens.Lens' CreateLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
createLifecyclePolicyResponse_policyId = Lens.lens (\CreateLifecyclePolicyResponse' {policyId} -> policyId) (\s@CreateLifecyclePolicyResponse' {} a -> s {policyId = a} :: CreateLifecyclePolicyResponse)

-- | The response's http status code.
createLifecyclePolicyResponse_httpStatus :: Lens.Lens' CreateLifecyclePolicyResponse Prelude.Int
createLifecyclePolicyResponse_httpStatus = Lens.lens (\CreateLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@CreateLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: CreateLifecyclePolicyResponse)

instance Prelude.NFData CreateLifecyclePolicyResponse where
  rnf CreateLifecyclePolicyResponse' {..} =
    Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf httpStatus
