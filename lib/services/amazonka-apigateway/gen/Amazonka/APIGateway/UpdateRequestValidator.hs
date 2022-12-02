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
-- Module      : Amazonka.APIGateway.UpdateRequestValidator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a RequestValidator of a given RestApi.
module Amazonka.APIGateway.UpdateRequestValidator
  ( -- * Creating a Request
    UpdateRequestValidator (..),
    newUpdateRequestValidator,

    -- * Request Lenses
    updateRequestValidator_patchOperations,
    updateRequestValidator_restApiId,
    updateRequestValidator_requestValidatorId,

    -- * Destructuring the Response
    RequestValidator (..),
    newRequestValidator,

    -- * Response Lenses
    requestValidator_validateRequestBody,
    requestValidator_name,
    requestValidator_validateRequestParameters,
    requestValidator_id,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates a RequestValidator of a given RestApi.
--
-- /See:/ 'newUpdateRequestValidator' smart constructor.
data UpdateRequestValidator = UpdateRequestValidator'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The identifier of RequestValidator to be updated.
    requestValidatorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRequestValidator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateRequestValidator_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateRequestValidator_restApiId' - The string identifier of the associated RestApi.
--
-- 'requestValidatorId', 'updateRequestValidator_requestValidatorId' - The identifier of RequestValidator to be updated.
newUpdateRequestValidator ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'requestValidatorId'
  Prelude.Text ->
  UpdateRequestValidator
newUpdateRequestValidator
  pRestApiId_
  pRequestValidatorId_ =
    UpdateRequestValidator'
      { patchOperations =
          Prelude.Nothing,
        restApiId = pRestApiId_,
        requestValidatorId = pRequestValidatorId_
      }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateRequestValidator_patchOperations :: Lens.Lens' UpdateRequestValidator (Prelude.Maybe [PatchOperation])
updateRequestValidator_patchOperations = Lens.lens (\UpdateRequestValidator' {patchOperations} -> patchOperations) (\s@UpdateRequestValidator' {} a -> s {patchOperations = a} :: UpdateRequestValidator) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
updateRequestValidator_restApiId :: Lens.Lens' UpdateRequestValidator Prelude.Text
updateRequestValidator_restApiId = Lens.lens (\UpdateRequestValidator' {restApiId} -> restApiId) (\s@UpdateRequestValidator' {} a -> s {restApiId = a} :: UpdateRequestValidator)

-- | The identifier of RequestValidator to be updated.
updateRequestValidator_requestValidatorId :: Lens.Lens' UpdateRequestValidator Prelude.Text
updateRequestValidator_requestValidatorId = Lens.lens (\UpdateRequestValidator' {requestValidatorId} -> requestValidatorId) (\s@UpdateRequestValidator' {} a -> s {requestValidatorId = a} :: UpdateRequestValidator)

instance Core.AWSRequest UpdateRequestValidator where
  type
    AWSResponse UpdateRequestValidator =
      RequestValidator
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateRequestValidator where
  hashWithSalt _salt UpdateRequestValidator' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` requestValidatorId

instance Prelude.NFData UpdateRequestValidator where
  rnf UpdateRequestValidator' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf requestValidatorId

instance Data.ToHeaders UpdateRequestValidator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateRequestValidator where
  toJSON UpdateRequestValidator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateRequestValidator where
  toPath UpdateRequestValidator' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/requestvalidators/",
        Data.toBS requestValidatorId
      ]

instance Data.ToQuery UpdateRequestValidator where
  toQuery = Prelude.const Prelude.mempty
