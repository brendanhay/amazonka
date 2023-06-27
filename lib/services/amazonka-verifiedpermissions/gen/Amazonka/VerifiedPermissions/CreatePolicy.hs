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
-- Module      : Amazonka.VerifiedPermissions.CreatePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Cedar policy and saves it in the specified policy store. You
-- can create either a static policy or a policy linked to a policy
-- template.
--
-- -   To create a static policy, provide the Cedar policy text in the
--     @StaticPolicy@ section of the @PolicyDefinition@.
--
-- -   To create a policy that is dynamically linked to a policy template,
--     specify the policy template ID and the principal and resource to
--     associate with this policy in the @templateLinked@ section of the
--     @PolicyDefinition@. If the policy template is ever updated, any
--     policies linked to the policy template automatically use the updated
--     template.
--
-- Creating a policy causes it to be validated against the schema in the
-- policy store. If the policy doesn\'t pass validation, the operation
-- fails and the policy isn\'t stored.
module Amazonka.VerifiedPermissions.CreatePolicy
  ( -- * Creating a Request
    CreatePolicy (..),
    newCreatePolicy,

    -- * Request Lenses
    createPolicy_clientToken,
    createPolicy_policyStoreId,
    createPolicy_definition,

    -- * Destructuring the Response
    CreatePolicyResponse (..),
    newCreatePolicyResponse,

    -- * Response Lenses
    createPolicyResponse_principal,
    createPolicyResponse_resource,
    createPolicyResponse_httpStatus,
    createPolicyResponse_policyStoreId,
    createPolicyResponse_policyId,
    createPolicyResponse_policyType,
    createPolicyResponse_createdDate,
    createPolicyResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { -- | Specifies a unique, case-sensitive ID that you provide to ensure the
    -- idempotency of the request. This lets you safely retry the request
    -- without accidentally performing the same operation a second time.
    -- Passing the same value to a later call to an operation requires that you
    -- also pass the same value for all other parameters. We recommend that you
    -- use a
    -- <https://wikipedia.org/wiki/Universally_unique_Id UUID type of value.>.
    --
    -- If you don\'t provide this value, then Amazon Web Services generates a
    -- random one for you.
    --
    -- If you retry the operation with the same @ClientToken@, but with
    -- different parameters, the retry fails with an
    -- @IdempotentParameterMismatch@ error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @PolicyStoreId@ of the policy store you want to store the
    -- policy in.
    policyStoreId :: Prelude.Text,
    -- | A structure that specifies the policy type and content to use for the
    -- new policy. You must include either a static or a templateLinked
    -- element. The policy content must be written in the Cedar policy
    -- language.
    definition :: PolicyDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPolicy_clientToken' - Specifies a unique, case-sensitive ID that you provide to ensure the
-- idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_Id UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
--
-- 'policyStoreId', 'createPolicy_policyStoreId' - Specifies the @PolicyStoreId@ of the policy store you want to store the
-- policy in.
--
-- 'definition', 'createPolicy_definition' - A structure that specifies the policy type and content to use for the
-- new policy. You must include either a static or a templateLinked
-- element. The policy content must be written in the Cedar policy
-- language.
newCreatePolicy ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'definition'
  PolicyDefinition ->
  CreatePolicy
newCreatePolicy pPolicyStoreId_ pDefinition_ =
  CreatePolicy'
    { clientToken = Prelude.Nothing,
      policyStoreId = pPolicyStoreId_,
      definition = pDefinition_
    }

-- | Specifies a unique, case-sensitive ID that you provide to ensure the
-- idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_Id UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
createPolicy_clientToken :: Lens.Lens' CreatePolicy (Prelude.Maybe Prelude.Text)
createPolicy_clientToken = Lens.lens (\CreatePolicy' {clientToken} -> clientToken) (\s@CreatePolicy' {} a -> s {clientToken = a} :: CreatePolicy)

-- | Specifies the @PolicyStoreId@ of the policy store you want to store the
-- policy in.
createPolicy_policyStoreId :: Lens.Lens' CreatePolicy Prelude.Text
createPolicy_policyStoreId = Lens.lens (\CreatePolicy' {policyStoreId} -> policyStoreId) (\s@CreatePolicy' {} a -> s {policyStoreId = a} :: CreatePolicy)

-- | A structure that specifies the policy type and content to use for the
-- new policy. You must include either a static or a templateLinked
-- element. The policy content must be written in the Cedar policy
-- language.
createPolicy_definition :: Lens.Lens' CreatePolicy PolicyDefinition
createPolicy_definition = Lens.lens (\CreatePolicy' {definition} -> definition) (\s@CreatePolicy' {} a -> s {definition = a} :: CreatePolicy)

instance Core.AWSRequest CreatePolicy where
  type AWSResponse CreatePolicy = CreatePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePolicyResponse'
            Prelude.<$> (x Data..?> "principal")
            Prelude.<*> (x Data..?> "resource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "policyId")
            Prelude.<*> (x Data..:> "policyType")
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable CreatePolicy where
  hashWithSalt _salt CreatePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` definition

instance Prelude.NFData CreatePolicy where
  rnf CreatePolicy' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf definition

instance Data.ToHeaders CreatePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.CreatePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePolicy where
  toJSON CreatePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("policyStoreId" Data..= policyStoreId),
            Prelude.Just ("definition" Data..= definition)
          ]
      )

instance Data.ToPath CreatePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { -- | The principal specified in the new policy\'s scope. This response
    -- element isn\'t present when @principal@ isn\'t specified in the policy
    -- content.
    principal :: Prelude.Maybe EntityIdentifier,
    -- | The resource specified in the new policy\'s scope. This response element
    -- isn\'t present when the @resource@ isn\'t specified in the policy
    -- content.
    resource :: Prelude.Maybe EntityIdentifier,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the policy store that contains the new policy.
    policyStoreId :: Prelude.Text,
    -- | The unique ID of the new policy.
    policyId :: Prelude.Text,
    -- | The policy type of the new policy.
    policyType :: PolicyType,
    -- | The date and time the policy was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time the policy was last updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'createPolicyResponse_principal' - The principal specified in the new policy\'s scope. This response
-- element isn\'t present when @principal@ isn\'t specified in the policy
-- content.
--
-- 'resource', 'createPolicyResponse_resource' - The resource specified in the new policy\'s scope. This response element
-- isn\'t present when the @resource@ isn\'t specified in the policy
-- content.
--
-- 'httpStatus', 'createPolicyResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'createPolicyResponse_policyStoreId' - The ID of the policy store that contains the new policy.
--
-- 'policyId', 'createPolicyResponse_policyId' - The unique ID of the new policy.
--
-- 'policyType', 'createPolicyResponse_policyType' - The policy type of the new policy.
--
-- 'createdDate', 'createPolicyResponse_createdDate' - The date and time the policy was originally created.
--
-- 'lastUpdatedDate', 'createPolicyResponse_lastUpdatedDate' - The date and time the policy was last updated.
newCreatePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyId'
  Prelude.Text ->
  -- | 'policyType'
  PolicyType ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  CreatePolicyResponse
newCreatePolicyResponse
  pHttpStatus_
  pPolicyStoreId_
  pPolicyId_
  pPolicyType_
  pCreatedDate_
  pLastUpdatedDate_ =
    CreatePolicyResponse'
      { principal = Prelude.Nothing,
        resource = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        policyStoreId = pPolicyStoreId_,
        policyId = pPolicyId_,
        policyType = pPolicyType_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The principal specified in the new policy\'s scope. This response
-- element isn\'t present when @principal@ isn\'t specified in the policy
-- content.
createPolicyResponse_principal :: Lens.Lens' CreatePolicyResponse (Prelude.Maybe EntityIdentifier)
createPolicyResponse_principal = Lens.lens (\CreatePolicyResponse' {principal} -> principal) (\s@CreatePolicyResponse' {} a -> s {principal = a} :: CreatePolicyResponse)

-- | The resource specified in the new policy\'s scope. This response element
-- isn\'t present when the @resource@ isn\'t specified in the policy
-- content.
createPolicyResponse_resource :: Lens.Lens' CreatePolicyResponse (Prelude.Maybe EntityIdentifier)
createPolicyResponse_resource = Lens.lens (\CreatePolicyResponse' {resource} -> resource) (\s@CreatePolicyResponse' {} a -> s {resource = a} :: CreatePolicyResponse)

-- | The response's http status code.
createPolicyResponse_httpStatus :: Lens.Lens' CreatePolicyResponse Prelude.Int
createPolicyResponse_httpStatus = Lens.lens (\CreatePolicyResponse' {httpStatus} -> httpStatus) (\s@CreatePolicyResponse' {} a -> s {httpStatus = a} :: CreatePolicyResponse)

-- | The ID of the policy store that contains the new policy.
createPolicyResponse_policyStoreId :: Lens.Lens' CreatePolicyResponse Prelude.Text
createPolicyResponse_policyStoreId = Lens.lens (\CreatePolicyResponse' {policyStoreId} -> policyStoreId) (\s@CreatePolicyResponse' {} a -> s {policyStoreId = a} :: CreatePolicyResponse)

-- | The unique ID of the new policy.
createPolicyResponse_policyId :: Lens.Lens' CreatePolicyResponse Prelude.Text
createPolicyResponse_policyId = Lens.lens (\CreatePolicyResponse' {policyId} -> policyId) (\s@CreatePolicyResponse' {} a -> s {policyId = a} :: CreatePolicyResponse)

-- | The policy type of the new policy.
createPolicyResponse_policyType :: Lens.Lens' CreatePolicyResponse PolicyType
createPolicyResponse_policyType = Lens.lens (\CreatePolicyResponse' {policyType} -> policyType) (\s@CreatePolicyResponse' {} a -> s {policyType = a} :: CreatePolicyResponse)

-- | The date and time the policy was originally created.
createPolicyResponse_createdDate :: Lens.Lens' CreatePolicyResponse Prelude.UTCTime
createPolicyResponse_createdDate = Lens.lens (\CreatePolicyResponse' {createdDate} -> createdDate) (\s@CreatePolicyResponse' {} a -> s {createdDate = a} :: CreatePolicyResponse) Prelude.. Data._Time

-- | The date and time the policy was last updated.
createPolicyResponse_lastUpdatedDate :: Lens.Lens' CreatePolicyResponse Prelude.UTCTime
createPolicyResponse_lastUpdatedDate = Lens.lens (\CreatePolicyResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreatePolicyResponse' {} a -> s {lastUpdatedDate = a} :: CreatePolicyResponse) Prelude.. Data._Time

instance Prelude.NFData CreatePolicyResponse where
  rnf CreatePolicyResponse' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
