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
-- Module      : Amazonka.ResilienceHub.CreateResiliencyPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resiliency policy for an application.
module Amazonka.ResilienceHub.CreateResiliencyPolicy
  ( -- * Creating a Request
    CreateResiliencyPolicy (..),
    newCreateResiliencyPolicy,

    -- * Request Lenses
    createResiliencyPolicy_clientToken,
    createResiliencyPolicy_dataLocationConstraint,
    createResiliencyPolicy_policyDescription,
    createResiliencyPolicy_tags,
    createResiliencyPolicy_policy,
    createResiliencyPolicy_policyName,
    createResiliencyPolicy_tier,

    -- * Destructuring the Response
    CreateResiliencyPolicyResponse (..),
    newCreateResiliencyPolicyResponse,

    -- * Response Lenses
    createResiliencyPolicyResponse_httpStatus,
    createResiliencyPolicyResponse_policy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateResiliencyPolicy' smart constructor.
data CreateResiliencyPolicy = CreateResiliencyPolicy'
  { -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies a high-level geographical location constraint for where your
    -- resilience policy data can be stored.
    dataLocationConstraint :: Prelude.Maybe DataLocationConstraint,
    -- | The description for the policy.
    policyDescription :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the resource. A tag is a label that you assign to
    -- an Amazon Web Services resource. Each tag consists of a key\/value pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The type of resiliency policy to be created, including the recovery time
    -- objective (RTO) and recovery point objective (RPO) in seconds.
    policy :: Prelude.HashMap DisruptionType FailurePolicy,
    -- | The name of the policy
    policyName :: Prelude.Text,
    -- | The tier for this resiliency policy, ranging from the highest severity
    -- (@MissionCritical@) to lowest (@NonCritical@).
    tier :: ResiliencyPolicyTier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResiliencyPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createResiliencyPolicy_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'dataLocationConstraint', 'createResiliencyPolicy_dataLocationConstraint' - Specifies a high-level geographical location constraint for where your
-- resilience policy data can be stored.
--
-- 'policyDescription', 'createResiliencyPolicy_policyDescription' - The description for the policy.
--
-- 'tags', 'createResiliencyPolicy_tags' - The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
--
-- 'policy', 'createResiliencyPolicy_policy' - The type of resiliency policy to be created, including the recovery time
-- objective (RTO) and recovery point objective (RPO) in seconds.
--
-- 'policyName', 'createResiliencyPolicy_policyName' - The name of the policy
--
-- 'tier', 'createResiliencyPolicy_tier' - The tier for this resiliency policy, ranging from the highest severity
-- (@MissionCritical@) to lowest (@NonCritical@).
newCreateResiliencyPolicy ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'tier'
  ResiliencyPolicyTier ->
  CreateResiliencyPolicy
newCreateResiliencyPolicy pPolicyName_ pTier_ =
  CreateResiliencyPolicy'
    { clientToken =
        Prelude.Nothing,
      dataLocationConstraint = Prelude.Nothing,
      policyDescription = Prelude.Nothing,
      tags = Prelude.Nothing,
      policy = Prelude.mempty,
      policyName = pPolicyName_,
      tier = pTier_
    }

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
createResiliencyPolicy_clientToken :: Lens.Lens' CreateResiliencyPolicy (Prelude.Maybe Prelude.Text)
createResiliencyPolicy_clientToken = Lens.lens (\CreateResiliencyPolicy' {clientToken} -> clientToken) (\s@CreateResiliencyPolicy' {} a -> s {clientToken = a} :: CreateResiliencyPolicy)

-- | Specifies a high-level geographical location constraint for where your
-- resilience policy data can be stored.
createResiliencyPolicy_dataLocationConstraint :: Lens.Lens' CreateResiliencyPolicy (Prelude.Maybe DataLocationConstraint)
createResiliencyPolicy_dataLocationConstraint = Lens.lens (\CreateResiliencyPolicy' {dataLocationConstraint} -> dataLocationConstraint) (\s@CreateResiliencyPolicy' {} a -> s {dataLocationConstraint = a} :: CreateResiliencyPolicy)

-- | The description for the policy.
createResiliencyPolicy_policyDescription :: Lens.Lens' CreateResiliencyPolicy (Prelude.Maybe Prelude.Text)
createResiliencyPolicy_policyDescription = Lens.lens (\CreateResiliencyPolicy' {policyDescription} -> policyDescription) (\s@CreateResiliencyPolicy' {} a -> s {policyDescription = a} :: CreateResiliencyPolicy)

-- | The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
createResiliencyPolicy_tags :: Lens.Lens' CreateResiliencyPolicy (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createResiliencyPolicy_tags = Lens.lens (\CreateResiliencyPolicy' {tags} -> tags) (\s@CreateResiliencyPolicy' {} a -> s {tags = a} :: CreateResiliencyPolicy) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The type of resiliency policy to be created, including the recovery time
-- objective (RTO) and recovery point objective (RPO) in seconds.
createResiliencyPolicy_policy :: Lens.Lens' CreateResiliencyPolicy (Prelude.HashMap DisruptionType FailurePolicy)
createResiliencyPolicy_policy = Lens.lens (\CreateResiliencyPolicy' {policy} -> policy) (\s@CreateResiliencyPolicy' {} a -> s {policy = a} :: CreateResiliencyPolicy) Prelude.. Lens.coerced

-- | The name of the policy
createResiliencyPolicy_policyName :: Lens.Lens' CreateResiliencyPolicy Prelude.Text
createResiliencyPolicy_policyName = Lens.lens (\CreateResiliencyPolicy' {policyName} -> policyName) (\s@CreateResiliencyPolicy' {} a -> s {policyName = a} :: CreateResiliencyPolicy)

-- | The tier for this resiliency policy, ranging from the highest severity
-- (@MissionCritical@) to lowest (@NonCritical@).
createResiliencyPolicy_tier :: Lens.Lens' CreateResiliencyPolicy ResiliencyPolicyTier
createResiliencyPolicy_tier = Lens.lens (\CreateResiliencyPolicy' {tier} -> tier) (\s@CreateResiliencyPolicy' {} a -> s {tier = a} :: CreateResiliencyPolicy)

instance Core.AWSRequest CreateResiliencyPolicy where
  type
    AWSResponse CreateResiliencyPolicy =
      CreateResiliencyPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResiliencyPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policy")
      )

instance Prelude.Hashable CreateResiliencyPolicy where
  hashWithSalt _salt CreateResiliencyPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dataLocationConstraint
      `Prelude.hashWithSalt` policyDescription
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` tier

instance Prelude.NFData CreateResiliencyPolicy where
  rnf CreateResiliencyPolicy' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dataLocationConstraint
      `Prelude.seq` Prelude.rnf policyDescription
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf tier

instance Data.ToHeaders CreateResiliencyPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResiliencyPolicy where
  toJSON CreateResiliencyPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("dataLocationConstraint" Data..=)
              Prelude.<$> dataLocationConstraint,
            ("policyDescription" Data..=)
              Prelude.<$> policyDescription,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("policy" Data..= policy),
            Prelude.Just ("policyName" Data..= policyName),
            Prelude.Just ("tier" Data..= tier)
          ]
      )

instance Data.ToPath CreateResiliencyPolicy where
  toPath = Prelude.const "/create-resiliency-policy"

instance Data.ToQuery CreateResiliencyPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResiliencyPolicyResponse' smart constructor.
data CreateResiliencyPolicyResponse = CreateResiliencyPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The type of resiliency policy that was created, including the recovery
    -- time objective (RTO) and recovery point objective (RPO) in seconds.
    policy :: ResiliencyPolicy
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResiliencyPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createResiliencyPolicyResponse_httpStatus' - The response's http status code.
--
-- 'policy', 'createResiliencyPolicyResponse_policy' - The type of resiliency policy that was created, including the recovery
-- time objective (RTO) and recovery point objective (RPO) in seconds.
newCreateResiliencyPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policy'
  ResiliencyPolicy ->
  CreateResiliencyPolicyResponse
newCreateResiliencyPolicyResponse
  pHttpStatus_
  pPolicy_ =
    CreateResiliencyPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        policy = pPolicy_
      }

-- | The response's http status code.
createResiliencyPolicyResponse_httpStatus :: Lens.Lens' CreateResiliencyPolicyResponse Prelude.Int
createResiliencyPolicyResponse_httpStatus = Lens.lens (\CreateResiliencyPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateResiliencyPolicyResponse' {} a -> s {httpStatus = a} :: CreateResiliencyPolicyResponse)

-- | The type of resiliency policy that was created, including the recovery
-- time objective (RTO) and recovery point objective (RPO) in seconds.
createResiliencyPolicyResponse_policy :: Lens.Lens' CreateResiliencyPolicyResponse ResiliencyPolicy
createResiliencyPolicyResponse_policy = Lens.lens (\CreateResiliencyPolicyResponse' {policy} -> policy) (\s@CreateResiliencyPolicyResponse' {} a -> s {policy = a} :: CreateResiliencyPolicyResponse)

instance
  Prelude.NFData
    CreateResiliencyPolicyResponse
  where
  rnf CreateResiliencyPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policy
