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
-- Module      : Amazonka.VerifiedPermissions.UpdatePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Cedar static policy in the specified policy store. You can
-- change only certain elements of the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicyInput.html#amazonverifiedpermissions-UpdatePolicy-request-UpdatePolicyDefinition UpdatePolicyDefinition>
-- parameter. You can directly update only static policies. To change a
-- template-linked policy, you must update the template instead, using
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicyTemplate.html UpdatePolicyTemplate>.
--
-- If policy validation is enabled in the policy store, then updating a
-- static policy causes Verified Permissions to validate the policy against
-- the schema in the policy store. If the updated static policy doesn\'t
-- pass validation, the operation fails and the update isn\'t stored.
module Amazonka.VerifiedPermissions.UpdatePolicy
  ( -- * Creating a Request
    UpdatePolicy (..),
    newUpdatePolicy,

    -- * Request Lenses
    updatePolicy_policyStoreId,
    updatePolicy_policyId,
    updatePolicy_definition,

    -- * Destructuring the Response
    UpdatePolicyResponse (..),
    newUpdatePolicyResponse,

    -- * Response Lenses
    updatePolicyResponse_principal,
    updatePolicyResponse_resource,
    updatePolicyResponse_httpStatus,
    updatePolicyResponse_policyStoreId,
    updatePolicyResponse_policyId,
    updatePolicyResponse_policyType,
    updatePolicyResponse_createdDate,
    updatePolicyResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newUpdatePolicy' smart constructor.
data UpdatePolicy = UpdatePolicy'
  { -- | Specifies the ID of the policy store that contains the policy that you
    -- want to update.
    policyStoreId :: Prelude.Text,
    -- | Specifies the ID of the policy that you want to update. To find this
    -- value, you can use
    -- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListPolicies.html ListPolicies>.
    policyId :: Prelude.Text,
    -- | Specifies the updated policy content that you want to replace on the
    -- specified policy. The content must be valid Cedar policy language text.
    --
    -- You can change only the following elements from the policy definition:
    --
    -- -   The @action@ referenced by the policy.
    --
    -- -   Any conditional clauses, such as @when@ or @unless@ clauses.
    --
    -- You __can\'t__ change the following elements:
    --
    -- -   Changing from @static@ to @templateLinked@.
    --
    -- -   Changing the effect of the policy from @permit@ or @forbid@.
    --
    -- -   The @principal@ referenced by the policy.
    --
    -- -   The @resource@ referenced by the policy.
    definition :: UpdatePolicyDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'updatePolicy_policyStoreId' - Specifies the ID of the policy store that contains the policy that you
-- want to update.
--
-- 'policyId', 'updatePolicy_policyId' - Specifies the ID of the policy that you want to update. To find this
-- value, you can use
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListPolicies.html ListPolicies>.
--
-- 'definition', 'updatePolicy_definition' - Specifies the updated policy content that you want to replace on the
-- specified policy. The content must be valid Cedar policy language text.
--
-- You can change only the following elements from the policy definition:
--
-- -   The @action@ referenced by the policy.
--
-- -   Any conditional clauses, such as @when@ or @unless@ clauses.
--
-- You __can\'t__ change the following elements:
--
-- -   Changing from @static@ to @templateLinked@.
--
-- -   Changing the effect of the policy from @permit@ or @forbid@.
--
-- -   The @principal@ referenced by the policy.
--
-- -   The @resource@ referenced by the policy.
newUpdatePolicy ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyId'
  Prelude.Text ->
  -- | 'definition'
  UpdatePolicyDefinition ->
  UpdatePolicy
newUpdatePolicy
  pPolicyStoreId_
  pPolicyId_
  pDefinition_ =
    UpdatePolicy'
      { policyStoreId = pPolicyStoreId_,
        policyId = pPolicyId_,
        definition = pDefinition_
      }

-- | Specifies the ID of the policy store that contains the policy that you
-- want to update.
updatePolicy_policyStoreId :: Lens.Lens' UpdatePolicy Prelude.Text
updatePolicy_policyStoreId = Lens.lens (\UpdatePolicy' {policyStoreId} -> policyStoreId) (\s@UpdatePolicy' {} a -> s {policyStoreId = a} :: UpdatePolicy)

-- | Specifies the ID of the policy that you want to update. To find this
-- value, you can use
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListPolicies.html ListPolicies>.
updatePolicy_policyId :: Lens.Lens' UpdatePolicy Prelude.Text
updatePolicy_policyId = Lens.lens (\UpdatePolicy' {policyId} -> policyId) (\s@UpdatePolicy' {} a -> s {policyId = a} :: UpdatePolicy)

-- | Specifies the updated policy content that you want to replace on the
-- specified policy. The content must be valid Cedar policy language text.
--
-- You can change only the following elements from the policy definition:
--
-- -   The @action@ referenced by the policy.
--
-- -   Any conditional clauses, such as @when@ or @unless@ clauses.
--
-- You __can\'t__ change the following elements:
--
-- -   Changing from @static@ to @templateLinked@.
--
-- -   Changing the effect of the policy from @permit@ or @forbid@.
--
-- -   The @principal@ referenced by the policy.
--
-- -   The @resource@ referenced by the policy.
updatePolicy_definition :: Lens.Lens' UpdatePolicy UpdatePolicyDefinition
updatePolicy_definition = Lens.lens (\UpdatePolicy' {definition} -> definition) (\s@UpdatePolicy' {} a -> s {definition = a} :: UpdatePolicy)

instance Core.AWSRequest UpdatePolicy where
  type AWSResponse UpdatePolicy = UpdatePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePolicyResponse'
            Prelude.<$> (x Data..?> "principal")
            Prelude.<*> (x Data..?> "resource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "policyId")
            Prelude.<*> (x Data..:> "policyType")
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable UpdatePolicy where
  hashWithSalt _salt UpdatePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` definition

instance Prelude.NFData UpdatePolicy where
  rnf UpdatePolicy' {..} =
    Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf definition

instance Data.ToHeaders UpdatePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.UpdatePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePolicy where
  toJSON UpdatePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId),
            Prelude.Just ("policyId" Data..= policyId),
            Prelude.Just ("definition" Data..= definition)
          ]
      )

instance Data.ToPath UpdatePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePolicyResponse' smart constructor.
data UpdatePolicyResponse = UpdatePolicyResponse'
  { -- | The principal specified in the policy\'s scope. This element isn\'t
    -- included in the response when @Principal@ isn\'t present in the policy
    -- content.
    principal :: Prelude.Maybe EntityIdentifier,
    -- | The resource specified in the policy\'s scope. This element isn\'t
    -- included in the response when @Resource@ isn\'t present in the policy
    -- content.
    resource :: Prelude.Maybe EntityIdentifier,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the policy store that contains the policy that was updated.
    policyStoreId :: Prelude.Text,
    -- | The ID of the policy that was updated.
    policyId :: Prelude.Text,
    -- | The type of the policy that was updated.
    policyType :: PolicyType,
    -- | The date and time that the policy was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time that the policy was most recently updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'updatePolicyResponse_principal' - The principal specified in the policy\'s scope. This element isn\'t
-- included in the response when @Principal@ isn\'t present in the policy
-- content.
--
-- 'resource', 'updatePolicyResponse_resource' - The resource specified in the policy\'s scope. This element isn\'t
-- included in the response when @Resource@ isn\'t present in the policy
-- content.
--
-- 'httpStatus', 'updatePolicyResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'updatePolicyResponse_policyStoreId' - The ID of the policy store that contains the policy that was updated.
--
-- 'policyId', 'updatePolicyResponse_policyId' - The ID of the policy that was updated.
--
-- 'policyType', 'updatePolicyResponse_policyType' - The type of the policy that was updated.
--
-- 'createdDate', 'updatePolicyResponse_createdDate' - The date and time that the policy was originally created.
--
-- 'lastUpdatedDate', 'updatePolicyResponse_lastUpdatedDate' - The date and time that the policy was most recently updated.
newUpdatePolicyResponse ::
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
  UpdatePolicyResponse
newUpdatePolicyResponse
  pHttpStatus_
  pPolicyStoreId_
  pPolicyId_
  pPolicyType_
  pCreatedDate_
  pLastUpdatedDate_ =
    UpdatePolicyResponse'
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

-- | The principal specified in the policy\'s scope. This element isn\'t
-- included in the response when @Principal@ isn\'t present in the policy
-- content.
updatePolicyResponse_principal :: Lens.Lens' UpdatePolicyResponse (Prelude.Maybe EntityIdentifier)
updatePolicyResponse_principal = Lens.lens (\UpdatePolicyResponse' {principal} -> principal) (\s@UpdatePolicyResponse' {} a -> s {principal = a} :: UpdatePolicyResponse)

-- | The resource specified in the policy\'s scope. This element isn\'t
-- included in the response when @Resource@ isn\'t present in the policy
-- content.
updatePolicyResponse_resource :: Lens.Lens' UpdatePolicyResponse (Prelude.Maybe EntityIdentifier)
updatePolicyResponse_resource = Lens.lens (\UpdatePolicyResponse' {resource} -> resource) (\s@UpdatePolicyResponse' {} a -> s {resource = a} :: UpdatePolicyResponse)

-- | The response's http status code.
updatePolicyResponse_httpStatus :: Lens.Lens' UpdatePolicyResponse Prelude.Int
updatePolicyResponse_httpStatus = Lens.lens (\UpdatePolicyResponse' {httpStatus} -> httpStatus) (\s@UpdatePolicyResponse' {} a -> s {httpStatus = a} :: UpdatePolicyResponse)

-- | The ID of the policy store that contains the policy that was updated.
updatePolicyResponse_policyStoreId :: Lens.Lens' UpdatePolicyResponse Prelude.Text
updatePolicyResponse_policyStoreId = Lens.lens (\UpdatePolicyResponse' {policyStoreId} -> policyStoreId) (\s@UpdatePolicyResponse' {} a -> s {policyStoreId = a} :: UpdatePolicyResponse)

-- | The ID of the policy that was updated.
updatePolicyResponse_policyId :: Lens.Lens' UpdatePolicyResponse Prelude.Text
updatePolicyResponse_policyId = Lens.lens (\UpdatePolicyResponse' {policyId} -> policyId) (\s@UpdatePolicyResponse' {} a -> s {policyId = a} :: UpdatePolicyResponse)

-- | The type of the policy that was updated.
updatePolicyResponse_policyType :: Lens.Lens' UpdatePolicyResponse PolicyType
updatePolicyResponse_policyType = Lens.lens (\UpdatePolicyResponse' {policyType} -> policyType) (\s@UpdatePolicyResponse' {} a -> s {policyType = a} :: UpdatePolicyResponse)

-- | The date and time that the policy was originally created.
updatePolicyResponse_createdDate :: Lens.Lens' UpdatePolicyResponse Prelude.UTCTime
updatePolicyResponse_createdDate = Lens.lens (\UpdatePolicyResponse' {createdDate} -> createdDate) (\s@UpdatePolicyResponse' {} a -> s {createdDate = a} :: UpdatePolicyResponse) Prelude.. Data._Time

-- | The date and time that the policy was most recently updated.
updatePolicyResponse_lastUpdatedDate :: Lens.Lens' UpdatePolicyResponse Prelude.UTCTime
updatePolicyResponse_lastUpdatedDate = Lens.lens (\UpdatePolicyResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@UpdatePolicyResponse' {} a -> s {lastUpdatedDate = a} :: UpdatePolicyResponse) Prelude.. Data._Time

instance Prelude.NFData UpdatePolicyResponse where
  rnf UpdatePolicyResponse' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
