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
-- Module      : Network.AWS.APIGateway.UpdateRequestValidator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a RequestValidator of a given RestApi.
module Network.AWS.APIGateway.UpdateRequestValidator
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
    requestValidator_id,
    requestValidator_validateRequestParameters,
    requestValidator_name,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates a RequestValidator of a given RestApi.
--
-- /See:/ 'newUpdateRequestValidator' smart constructor.
data UpdateRequestValidator = UpdateRequestValidator'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The identifier of RequestValidator to be updated.
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
-- 'patchOperations', 'updateRequestValidator_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'restApiId', 'updateRequestValidator_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'requestValidatorId', 'updateRequestValidator_requestValidatorId' - [Required] The identifier of RequestValidator to be updated.
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

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateRequestValidator_patchOperations :: Lens.Lens' UpdateRequestValidator (Prelude.Maybe [PatchOperation])
updateRequestValidator_patchOperations = Lens.lens (\UpdateRequestValidator' {patchOperations} -> patchOperations) (\s@UpdateRequestValidator' {} a -> s {patchOperations = a} :: UpdateRequestValidator) Prelude.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
updateRequestValidator_restApiId :: Lens.Lens' UpdateRequestValidator Prelude.Text
updateRequestValidator_restApiId = Lens.lens (\UpdateRequestValidator' {restApiId} -> restApiId) (\s@UpdateRequestValidator' {} a -> s {restApiId = a} :: UpdateRequestValidator)

-- | [Required] The identifier of RequestValidator to be updated.
updateRequestValidator_requestValidatorId :: Lens.Lens' UpdateRequestValidator Prelude.Text
updateRequestValidator_requestValidatorId = Lens.lens (\UpdateRequestValidator' {requestValidatorId} -> requestValidatorId) (\s@UpdateRequestValidator' {} a -> s {requestValidatorId = a} :: UpdateRequestValidator)

instance Core.AWSRequest UpdateRequestValidator where
  type
    AWSResponse UpdateRequestValidator =
      RequestValidator
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateRequestValidator

instance Prelude.NFData UpdateRequestValidator

instance Core.ToHeaders UpdateRequestValidator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateRequestValidator where
  toJSON UpdateRequestValidator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateRequestValidator where
  toPath UpdateRequestValidator' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/requestvalidators/",
        Core.toBS requestValidatorId
      ]

instance Core.ToQuery UpdateRequestValidator where
  toQuery = Prelude.const Prelude.mempty
