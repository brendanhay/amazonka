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
-- Module      : Amazonka.VerifiedPermissions.CreatePolicyStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy store. A policy store is a container for policy
-- resources.
--
-- Although
-- <https://docs.cedarpolicy.com/schema.html#namespace Cedar supports multiple namespaces>,
-- Verified Permissions currently supports only one namespace per policy
-- store.
module Amazonka.VerifiedPermissions.CreatePolicyStore
  ( -- * Creating a Request
    CreatePolicyStore (..),
    newCreatePolicyStore,

    -- * Request Lenses
    createPolicyStore_clientToken,
    createPolicyStore_validationSettings,

    -- * Destructuring the Response
    CreatePolicyStoreResponse (..),
    newCreatePolicyStoreResponse,

    -- * Response Lenses
    createPolicyStoreResponse_httpStatus,
    createPolicyStoreResponse_policyStoreId,
    createPolicyStoreResponse_arn,
    createPolicyStoreResponse_createdDate,
    createPolicyStoreResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newCreatePolicyStore' smart constructor.
data CreatePolicyStore = CreatePolicyStore'
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
    -- | Specifies the validation setting for this policy store.
    --
    -- Currently, the only valid and required value is @Mode@.
    --
    -- We recommend that you turn on @STRICT@ mode only after you define a
    -- schema. If a schema doesn\'t exist, then @STRICT@ mode causes any policy
    -- to fail validation, and Verified Permissions rejects the policy. You can
    -- turn off validation by using the
    -- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicyStore UpdatePolicyStore>.
    -- Then, when you have a schema defined, use
    -- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicyStore UpdatePolicyStore>
    -- again to turn validation back on.
    validationSettings :: ValidationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPolicyStore_clientToken' - Specifies a unique, case-sensitive ID that you provide to ensure the
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
-- 'validationSettings', 'createPolicyStore_validationSettings' - Specifies the validation setting for this policy store.
--
-- Currently, the only valid and required value is @Mode@.
--
-- We recommend that you turn on @STRICT@ mode only after you define a
-- schema. If a schema doesn\'t exist, then @STRICT@ mode causes any policy
-- to fail validation, and Verified Permissions rejects the policy. You can
-- turn off validation by using the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicyStore UpdatePolicyStore>.
-- Then, when you have a schema defined, use
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicyStore UpdatePolicyStore>
-- again to turn validation back on.
newCreatePolicyStore ::
  -- | 'validationSettings'
  ValidationSettings ->
  CreatePolicyStore
newCreatePolicyStore pValidationSettings_ =
  CreatePolicyStore'
    { clientToken = Prelude.Nothing,
      validationSettings = pValidationSettings_
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
createPolicyStore_clientToken :: Lens.Lens' CreatePolicyStore (Prelude.Maybe Prelude.Text)
createPolicyStore_clientToken = Lens.lens (\CreatePolicyStore' {clientToken} -> clientToken) (\s@CreatePolicyStore' {} a -> s {clientToken = a} :: CreatePolicyStore)

-- | Specifies the validation setting for this policy store.
--
-- Currently, the only valid and required value is @Mode@.
--
-- We recommend that you turn on @STRICT@ mode only after you define a
-- schema. If a schema doesn\'t exist, then @STRICT@ mode causes any policy
-- to fail validation, and Verified Permissions rejects the policy. You can
-- turn off validation by using the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicyStore UpdatePolicyStore>.
-- Then, when you have a schema defined, use
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicyStore UpdatePolicyStore>
-- again to turn validation back on.
createPolicyStore_validationSettings :: Lens.Lens' CreatePolicyStore ValidationSettings
createPolicyStore_validationSettings = Lens.lens (\CreatePolicyStore' {validationSettings} -> validationSettings) (\s@CreatePolicyStore' {} a -> s {validationSettings = a} :: CreatePolicyStore)

instance Core.AWSRequest CreatePolicyStore where
  type
    AWSResponse CreatePolicyStore =
      CreatePolicyStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePolicyStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable CreatePolicyStore where
  hashWithSalt _salt CreatePolicyStore' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` validationSettings

instance Prelude.NFData CreatePolicyStore where
  rnf CreatePolicyStore' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf validationSettings

instance Data.ToHeaders CreatePolicyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.CreatePolicyStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePolicyStore where
  toJSON CreatePolicyStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("validationSettings" Data..= validationSettings)
          ]
      )

instance Data.ToPath CreatePolicyStore where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePolicyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePolicyStoreResponse' smart constructor.
data CreatePolicyStoreResponse = CreatePolicyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique ID of the new policy store.
    policyStoreId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the new policy store.
    arn :: Prelude.Text,
    -- | The date and time the policy store was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time the policy store was last updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPolicyStoreResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'createPolicyStoreResponse_policyStoreId' - The unique ID of the new policy store.
--
-- 'arn', 'createPolicyStoreResponse_arn' - The Amazon Resource Name (ARN) of the new policy store.
--
-- 'createdDate', 'createPolicyStoreResponse_createdDate' - The date and time the policy store was originally created.
--
-- 'lastUpdatedDate', 'createPolicyStoreResponse_lastUpdatedDate' - The date and time the policy store was last updated.
newCreatePolicyStoreResponse ::
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
  CreatePolicyStoreResponse
newCreatePolicyStoreResponse
  pHttpStatus_
  pPolicyStoreId_
  pArn_
  pCreatedDate_
  pLastUpdatedDate_ =
    CreatePolicyStoreResponse'
      { httpStatus =
          pHttpStatus_,
        policyStoreId = pPolicyStoreId_,
        arn = pArn_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The response's http status code.
createPolicyStoreResponse_httpStatus :: Lens.Lens' CreatePolicyStoreResponse Prelude.Int
createPolicyStoreResponse_httpStatus = Lens.lens (\CreatePolicyStoreResponse' {httpStatus} -> httpStatus) (\s@CreatePolicyStoreResponse' {} a -> s {httpStatus = a} :: CreatePolicyStoreResponse)

-- | The unique ID of the new policy store.
createPolicyStoreResponse_policyStoreId :: Lens.Lens' CreatePolicyStoreResponse Prelude.Text
createPolicyStoreResponse_policyStoreId = Lens.lens (\CreatePolicyStoreResponse' {policyStoreId} -> policyStoreId) (\s@CreatePolicyStoreResponse' {} a -> s {policyStoreId = a} :: CreatePolicyStoreResponse)

-- | The Amazon Resource Name (ARN) of the new policy store.
createPolicyStoreResponse_arn :: Lens.Lens' CreatePolicyStoreResponse Prelude.Text
createPolicyStoreResponse_arn = Lens.lens (\CreatePolicyStoreResponse' {arn} -> arn) (\s@CreatePolicyStoreResponse' {} a -> s {arn = a} :: CreatePolicyStoreResponse)

-- | The date and time the policy store was originally created.
createPolicyStoreResponse_createdDate :: Lens.Lens' CreatePolicyStoreResponse Prelude.UTCTime
createPolicyStoreResponse_createdDate = Lens.lens (\CreatePolicyStoreResponse' {createdDate} -> createdDate) (\s@CreatePolicyStoreResponse' {} a -> s {createdDate = a} :: CreatePolicyStoreResponse) Prelude.. Data._Time

-- | The date and time the policy store was last updated.
createPolicyStoreResponse_lastUpdatedDate :: Lens.Lens' CreatePolicyStoreResponse Prelude.UTCTime
createPolicyStoreResponse_lastUpdatedDate = Lens.lens (\CreatePolicyStoreResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreatePolicyStoreResponse' {} a -> s {lastUpdatedDate = a} :: CreatePolicyStoreResponse) Prelude.. Data._Time

instance Prelude.NFData CreatePolicyStoreResponse where
  rnf CreatePolicyStoreResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
