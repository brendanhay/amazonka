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
-- Module      : Amazonka.VerifiedPermissions.GetPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified policy.
module Amazonka.VerifiedPermissions.GetPolicy
  ( -- * Creating a Request
    GetPolicy (..),
    newGetPolicy,

    -- * Request Lenses
    getPolicy_policyStoreId,
    getPolicy_policyId,

    -- * Destructuring the Response
    GetPolicyResponse (..),
    newGetPolicyResponse,

    -- * Response Lenses
    getPolicyResponse_principal,
    getPolicyResponse_resource,
    getPolicyResponse_httpStatus,
    getPolicyResponse_policyStoreId,
    getPolicyResponse_policyId,
    getPolicyResponse_policyType,
    getPolicyResponse_definition,
    getPolicyResponse_createdDate,
    getPolicyResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newGetPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { -- | Specifies the ID of the policy store that contains the policy that you
    -- want information about.
    policyStoreId :: Prelude.Text,
    -- | Specifies the ID of the policy you want information about.
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'getPolicy_policyStoreId' - Specifies the ID of the policy store that contains the policy that you
-- want information about.
--
-- 'policyId', 'getPolicy_policyId' - Specifies the ID of the policy you want information about.
newGetPolicy ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyId'
  Prelude.Text ->
  GetPolicy
newGetPolicy pPolicyStoreId_ pPolicyId_ =
  GetPolicy'
    { policyStoreId = pPolicyStoreId_,
      policyId = pPolicyId_
    }

-- | Specifies the ID of the policy store that contains the policy that you
-- want information about.
getPolicy_policyStoreId :: Lens.Lens' GetPolicy Prelude.Text
getPolicy_policyStoreId = Lens.lens (\GetPolicy' {policyStoreId} -> policyStoreId) (\s@GetPolicy' {} a -> s {policyStoreId = a} :: GetPolicy)

-- | Specifies the ID of the policy you want information about.
getPolicy_policyId :: Lens.Lens' GetPolicy Prelude.Text
getPolicy_policyId = Lens.lens (\GetPolicy' {policyId} -> policyId) (\s@GetPolicy' {} a -> s {policyId = a} :: GetPolicy)

instance Core.AWSRequest GetPolicy where
  type AWSResponse GetPolicy = GetPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Prelude.<$> (x Data..?> "principal")
            Prelude.<*> (x Data..?> "resource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "policyId")
            Prelude.<*> (x Data..:> "policyType")
            Prelude.<*> (x Data..:> "definition")
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable GetPolicy where
  hashWithSalt _salt GetPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` policyId

instance Prelude.NFData GetPolicy where
  rnf GetPolicy' {..} =
    Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyId

instance Data.ToHeaders GetPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.GetPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPolicy where
  toJSON GetPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId),
            Prelude.Just ("policyId" Data..= policyId)
          ]
      )

instance Data.ToPath GetPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
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
    -- | The ID of the policy store that contains the policy that you want
    -- information about.
    policyStoreId :: Prelude.Text,
    -- | The unique ID of the policy that you want information about.
    policyId :: Prelude.Text,
    -- | The type of the policy.
    policyType :: PolicyType,
    -- | The definition of the requested policy.
    definition :: PolicyDefinitionDetail,
    -- | The date and time that the policy was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time that the policy was last updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'getPolicyResponse_principal' - The principal specified in the policy\'s scope. This element isn\'t
-- included in the response when @Principal@ isn\'t present in the policy
-- content.
--
-- 'resource', 'getPolicyResponse_resource' - The resource specified in the policy\'s scope. This element isn\'t
-- included in the response when @Resource@ isn\'t present in the policy
-- content.
--
-- 'httpStatus', 'getPolicyResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'getPolicyResponse_policyStoreId' - The ID of the policy store that contains the policy that you want
-- information about.
--
-- 'policyId', 'getPolicyResponse_policyId' - The unique ID of the policy that you want information about.
--
-- 'policyType', 'getPolicyResponse_policyType' - The type of the policy.
--
-- 'definition', 'getPolicyResponse_definition' - The definition of the requested policy.
--
-- 'createdDate', 'getPolicyResponse_createdDate' - The date and time that the policy was originally created.
--
-- 'lastUpdatedDate', 'getPolicyResponse_lastUpdatedDate' - The date and time that the policy was last updated.
newGetPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyId'
  Prelude.Text ->
  -- | 'policyType'
  PolicyType ->
  -- | 'definition'
  PolicyDefinitionDetail ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  GetPolicyResponse
newGetPolicyResponse
  pHttpStatus_
  pPolicyStoreId_
  pPolicyId_
  pPolicyType_
  pDefinition_
  pCreatedDate_
  pLastUpdatedDate_ =
    GetPolicyResponse'
      { principal = Prelude.Nothing,
        resource = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        policyStoreId = pPolicyStoreId_,
        policyId = pPolicyId_,
        policyType = pPolicyType_,
        definition = pDefinition_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The principal specified in the policy\'s scope. This element isn\'t
-- included in the response when @Principal@ isn\'t present in the policy
-- content.
getPolicyResponse_principal :: Lens.Lens' GetPolicyResponse (Prelude.Maybe EntityIdentifier)
getPolicyResponse_principal = Lens.lens (\GetPolicyResponse' {principal} -> principal) (\s@GetPolicyResponse' {} a -> s {principal = a} :: GetPolicyResponse)

-- | The resource specified in the policy\'s scope. This element isn\'t
-- included in the response when @Resource@ isn\'t present in the policy
-- content.
getPolicyResponse_resource :: Lens.Lens' GetPolicyResponse (Prelude.Maybe EntityIdentifier)
getPolicyResponse_resource = Lens.lens (\GetPolicyResponse' {resource} -> resource) (\s@GetPolicyResponse' {} a -> s {resource = a} :: GetPolicyResponse)

-- | The response's http status code.
getPolicyResponse_httpStatus :: Lens.Lens' GetPolicyResponse Prelude.Int
getPolicyResponse_httpStatus = Lens.lens (\GetPolicyResponse' {httpStatus} -> httpStatus) (\s@GetPolicyResponse' {} a -> s {httpStatus = a} :: GetPolicyResponse)

-- | The ID of the policy store that contains the policy that you want
-- information about.
getPolicyResponse_policyStoreId :: Lens.Lens' GetPolicyResponse Prelude.Text
getPolicyResponse_policyStoreId = Lens.lens (\GetPolicyResponse' {policyStoreId} -> policyStoreId) (\s@GetPolicyResponse' {} a -> s {policyStoreId = a} :: GetPolicyResponse)

-- | The unique ID of the policy that you want information about.
getPolicyResponse_policyId :: Lens.Lens' GetPolicyResponse Prelude.Text
getPolicyResponse_policyId = Lens.lens (\GetPolicyResponse' {policyId} -> policyId) (\s@GetPolicyResponse' {} a -> s {policyId = a} :: GetPolicyResponse)

-- | The type of the policy.
getPolicyResponse_policyType :: Lens.Lens' GetPolicyResponse PolicyType
getPolicyResponse_policyType = Lens.lens (\GetPolicyResponse' {policyType} -> policyType) (\s@GetPolicyResponse' {} a -> s {policyType = a} :: GetPolicyResponse)

-- | The definition of the requested policy.
getPolicyResponse_definition :: Lens.Lens' GetPolicyResponse PolicyDefinitionDetail
getPolicyResponse_definition = Lens.lens (\GetPolicyResponse' {definition} -> definition) (\s@GetPolicyResponse' {} a -> s {definition = a} :: GetPolicyResponse)

-- | The date and time that the policy was originally created.
getPolicyResponse_createdDate :: Lens.Lens' GetPolicyResponse Prelude.UTCTime
getPolicyResponse_createdDate = Lens.lens (\GetPolicyResponse' {createdDate} -> createdDate) (\s@GetPolicyResponse' {} a -> s {createdDate = a} :: GetPolicyResponse) Prelude.. Data._Time

-- | The date and time that the policy was last updated.
getPolicyResponse_lastUpdatedDate :: Lens.Lens' GetPolicyResponse Prelude.UTCTime
getPolicyResponse_lastUpdatedDate = Lens.lens (\GetPolicyResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetPolicyResponse' {} a -> s {lastUpdatedDate = a} :: GetPolicyResponse) Prelude.. Data._Time

instance Prelude.NFData GetPolicyResponse where
  rnf GetPolicyResponse' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
