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
-- Module      : Amazonka.VerifiedPermissions.GetPolicyStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about a policy store.
module Amazonka.VerifiedPermissions.GetPolicyStore
  ( -- * Creating a Request
    GetPolicyStore (..),
    newGetPolicyStore,

    -- * Request Lenses
    getPolicyStore_policyStoreId,

    -- * Destructuring the Response
    GetPolicyStoreResponse (..),
    newGetPolicyStoreResponse,

    -- * Response Lenses
    getPolicyStoreResponse_httpStatus,
    getPolicyStoreResponse_policyStoreId,
    getPolicyStoreResponse_arn,
    getPolicyStoreResponse_validationSettings,
    getPolicyStoreResponse_createdDate,
    getPolicyStoreResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newGetPolicyStore' smart constructor.
data GetPolicyStore = GetPolicyStore'
  { -- | Specifies the ID of the policy store that you want information about.
    policyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'getPolicyStore_policyStoreId' - Specifies the ID of the policy store that you want information about.
newGetPolicyStore ::
  -- | 'policyStoreId'
  Prelude.Text ->
  GetPolicyStore
newGetPolicyStore pPolicyStoreId_ =
  GetPolicyStore' {policyStoreId = pPolicyStoreId_}

-- | Specifies the ID of the policy store that you want information about.
getPolicyStore_policyStoreId :: Lens.Lens' GetPolicyStore Prelude.Text
getPolicyStore_policyStoreId = Lens.lens (\GetPolicyStore' {policyStoreId} -> policyStoreId) (\s@GetPolicyStore' {} a -> s {policyStoreId = a} :: GetPolicyStore)

instance Core.AWSRequest GetPolicyStore where
  type
    AWSResponse GetPolicyStore =
      GetPolicyStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "validationSettings")
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable GetPolicyStore where
  hashWithSalt _salt GetPolicyStore' {..} =
    _salt `Prelude.hashWithSalt` policyStoreId

instance Prelude.NFData GetPolicyStore where
  rnf GetPolicyStore' {..} = Prelude.rnf policyStoreId

instance Data.ToHeaders GetPolicyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.GetPolicyStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPolicyStore where
  toJSON GetPolicyStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId)
          ]
      )

instance Data.ToPath GetPolicyStore where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPolicyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPolicyStoreResponse' smart constructor.
data GetPolicyStoreResponse = GetPolicyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the policy store;
    policyStoreId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the policy store.
    arn :: Prelude.Text,
    -- | The current validation settings for the policy store.
    validationSettings :: ValidationSettings,
    -- | The date and time that the policy store was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time that the policy store was last updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getPolicyStoreResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'getPolicyStoreResponse_policyStoreId' - The ID of the policy store;
--
-- 'arn', 'getPolicyStoreResponse_arn' - The Amazon Resource Name (ARN) of the policy store.
--
-- 'validationSettings', 'getPolicyStoreResponse_validationSettings' - The current validation settings for the policy store.
--
-- 'createdDate', 'getPolicyStoreResponse_createdDate' - The date and time that the policy store was originally created.
--
-- 'lastUpdatedDate', 'getPolicyStoreResponse_lastUpdatedDate' - The date and time that the policy store was last updated.
newGetPolicyStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'validationSettings'
  ValidationSettings ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  GetPolicyStoreResponse
newGetPolicyStoreResponse
  pHttpStatus_
  pPolicyStoreId_
  pArn_
  pValidationSettings_
  pCreatedDate_
  pLastUpdatedDate_ =
    GetPolicyStoreResponse'
      { httpStatus = pHttpStatus_,
        policyStoreId = pPolicyStoreId_,
        arn = pArn_,
        validationSettings = pValidationSettings_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The response's http status code.
getPolicyStoreResponse_httpStatus :: Lens.Lens' GetPolicyStoreResponse Prelude.Int
getPolicyStoreResponse_httpStatus = Lens.lens (\GetPolicyStoreResponse' {httpStatus} -> httpStatus) (\s@GetPolicyStoreResponse' {} a -> s {httpStatus = a} :: GetPolicyStoreResponse)

-- | The ID of the policy store;
getPolicyStoreResponse_policyStoreId :: Lens.Lens' GetPolicyStoreResponse Prelude.Text
getPolicyStoreResponse_policyStoreId = Lens.lens (\GetPolicyStoreResponse' {policyStoreId} -> policyStoreId) (\s@GetPolicyStoreResponse' {} a -> s {policyStoreId = a} :: GetPolicyStoreResponse)

-- | The Amazon Resource Name (ARN) of the policy store.
getPolicyStoreResponse_arn :: Lens.Lens' GetPolicyStoreResponse Prelude.Text
getPolicyStoreResponse_arn = Lens.lens (\GetPolicyStoreResponse' {arn} -> arn) (\s@GetPolicyStoreResponse' {} a -> s {arn = a} :: GetPolicyStoreResponse)

-- | The current validation settings for the policy store.
getPolicyStoreResponse_validationSettings :: Lens.Lens' GetPolicyStoreResponse ValidationSettings
getPolicyStoreResponse_validationSettings = Lens.lens (\GetPolicyStoreResponse' {validationSettings} -> validationSettings) (\s@GetPolicyStoreResponse' {} a -> s {validationSettings = a} :: GetPolicyStoreResponse)

-- | The date and time that the policy store was originally created.
getPolicyStoreResponse_createdDate :: Lens.Lens' GetPolicyStoreResponse Prelude.UTCTime
getPolicyStoreResponse_createdDate = Lens.lens (\GetPolicyStoreResponse' {createdDate} -> createdDate) (\s@GetPolicyStoreResponse' {} a -> s {createdDate = a} :: GetPolicyStoreResponse) Prelude.. Data._Time

-- | The date and time that the policy store was last updated.
getPolicyStoreResponse_lastUpdatedDate :: Lens.Lens' GetPolicyStoreResponse Prelude.UTCTime
getPolicyStoreResponse_lastUpdatedDate = Lens.lens (\GetPolicyStoreResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetPolicyStoreResponse' {} a -> s {lastUpdatedDate = a} :: GetPolicyStoreResponse) Prelude.. Data._Time

instance Prelude.NFData GetPolicyStoreResponse where
  rnf GetPolicyStoreResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf validationSettings
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
