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
-- Module      : Amazonka.VerifiedPermissions.UpdatePolicyStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the validation setting for a policy store.
module Amazonka.VerifiedPermissions.UpdatePolicyStore
  ( -- * Creating a Request
    UpdatePolicyStore (..),
    newUpdatePolicyStore,

    -- * Request Lenses
    updatePolicyStore_policyStoreId,
    updatePolicyStore_validationSettings,

    -- * Destructuring the Response
    UpdatePolicyStoreResponse (..),
    newUpdatePolicyStoreResponse,

    -- * Response Lenses
    updatePolicyStoreResponse_httpStatus,
    updatePolicyStoreResponse_policyStoreId,
    updatePolicyStoreResponse_arn,
    updatePolicyStoreResponse_createdDate,
    updatePolicyStoreResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newUpdatePolicyStore' smart constructor.
data UpdatePolicyStore = UpdatePolicyStore'
  { -- | Specifies the ID of the policy store that you want to update
    policyStoreId :: Prelude.Text,
    -- | A structure that defines the validation settings that want to enable for
    -- the policy store.
    validationSettings :: ValidationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePolicyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'updatePolicyStore_policyStoreId' - Specifies the ID of the policy store that you want to update
--
-- 'validationSettings', 'updatePolicyStore_validationSettings' - A structure that defines the validation settings that want to enable for
-- the policy store.
newUpdatePolicyStore ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'validationSettings'
  ValidationSettings ->
  UpdatePolicyStore
newUpdatePolicyStore
  pPolicyStoreId_
  pValidationSettings_ =
    UpdatePolicyStore'
      { policyStoreId = pPolicyStoreId_,
        validationSettings = pValidationSettings_
      }

-- | Specifies the ID of the policy store that you want to update
updatePolicyStore_policyStoreId :: Lens.Lens' UpdatePolicyStore Prelude.Text
updatePolicyStore_policyStoreId = Lens.lens (\UpdatePolicyStore' {policyStoreId} -> policyStoreId) (\s@UpdatePolicyStore' {} a -> s {policyStoreId = a} :: UpdatePolicyStore)

-- | A structure that defines the validation settings that want to enable for
-- the policy store.
updatePolicyStore_validationSettings :: Lens.Lens' UpdatePolicyStore ValidationSettings
updatePolicyStore_validationSettings = Lens.lens (\UpdatePolicyStore' {validationSettings} -> validationSettings) (\s@UpdatePolicyStore' {} a -> s {validationSettings = a} :: UpdatePolicyStore)

instance Core.AWSRequest UpdatePolicyStore where
  type
    AWSResponse UpdatePolicyStore =
      UpdatePolicyStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePolicyStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable UpdatePolicyStore where
  hashWithSalt _salt UpdatePolicyStore' {..} =
    _salt
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` validationSettings

instance Prelude.NFData UpdatePolicyStore where
  rnf UpdatePolicyStore' {..} =
    Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf validationSettings

instance Data.ToHeaders UpdatePolicyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.UpdatePolicyStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePolicyStore where
  toJSON UpdatePolicyStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId),
            Prelude.Just
              ("validationSettings" Data..= validationSettings)
          ]
      )

instance Data.ToPath UpdatePolicyStore where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePolicyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePolicyStoreResponse' smart constructor.
data UpdatePolicyStoreResponse = UpdatePolicyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the updated policy store.
    policyStoreId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the updated policy store.
    arn :: Prelude.Text,
    -- | The date and time that the policy store was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time that the policy store was most recently updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePolicyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePolicyStoreResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'updatePolicyStoreResponse_policyStoreId' - The ID of the updated policy store.
--
-- 'arn', 'updatePolicyStoreResponse_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the updated policy store.
--
-- 'createdDate', 'updatePolicyStoreResponse_createdDate' - The date and time that the policy store was originally created.
--
-- 'lastUpdatedDate', 'updatePolicyStoreResponse_lastUpdatedDate' - The date and time that the policy store was most recently updated.
newUpdatePolicyStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  UpdatePolicyStoreResponse
newUpdatePolicyStoreResponse
  pHttpStatus_
  pPolicyStoreId_
  pArn_
  pCreatedDate_
  pLastUpdatedDate_ =
    UpdatePolicyStoreResponse'
      { httpStatus =
          pHttpStatus_,
        policyStoreId = pPolicyStoreId_,
        arn = pArn_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The response's http status code.
updatePolicyStoreResponse_httpStatus :: Lens.Lens' UpdatePolicyStoreResponse Prelude.Int
updatePolicyStoreResponse_httpStatus = Lens.lens (\UpdatePolicyStoreResponse' {httpStatus} -> httpStatus) (\s@UpdatePolicyStoreResponse' {} a -> s {httpStatus = a} :: UpdatePolicyStoreResponse)

-- | The ID of the updated policy store.
updatePolicyStoreResponse_policyStoreId :: Lens.Lens' UpdatePolicyStoreResponse Prelude.Text
updatePolicyStoreResponse_policyStoreId = Lens.lens (\UpdatePolicyStoreResponse' {policyStoreId} -> policyStoreId) (\s@UpdatePolicyStoreResponse' {} a -> s {policyStoreId = a} :: UpdatePolicyStoreResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the updated policy store.
updatePolicyStoreResponse_arn :: Lens.Lens' UpdatePolicyStoreResponse Prelude.Text
updatePolicyStoreResponse_arn = Lens.lens (\UpdatePolicyStoreResponse' {arn} -> arn) (\s@UpdatePolicyStoreResponse' {} a -> s {arn = a} :: UpdatePolicyStoreResponse)

-- | The date and time that the policy store was originally created.
updatePolicyStoreResponse_createdDate :: Lens.Lens' UpdatePolicyStoreResponse Prelude.UTCTime
updatePolicyStoreResponse_createdDate = Lens.lens (\UpdatePolicyStoreResponse' {createdDate} -> createdDate) (\s@UpdatePolicyStoreResponse' {} a -> s {createdDate = a} :: UpdatePolicyStoreResponse) Prelude.. Data._Time

-- | The date and time that the policy store was most recently updated.
updatePolicyStoreResponse_lastUpdatedDate :: Lens.Lens' UpdatePolicyStoreResponse Prelude.UTCTime
updatePolicyStoreResponse_lastUpdatedDate = Lens.lens (\UpdatePolicyStoreResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@UpdatePolicyStoreResponse' {} a -> s {lastUpdatedDate = a} :: UpdatePolicyStoreResponse) Prelude.. Data._Time

instance Prelude.NFData UpdatePolicyStoreResponse where
  rnf UpdatePolicyStoreResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
