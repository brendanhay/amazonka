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
-- Module      : Amazonka.APIGateway.UpdateMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Method resource.
module Amazonka.APIGateway.UpdateMethod
  ( -- * Creating a Request
    UpdateMethod (..),
    newUpdateMethod,

    -- * Request Lenses
    updateMethod_patchOperations,
    updateMethod_restApiId,
    updateMethod_resourceId,
    updateMethod_httpMethod,

    -- * Destructuring the Response
    Method (..),
    newMethod,

    -- * Response Lenses
    method_methodResponses,
    method_httpMethod,
    method_authorizationScopes,
    method_requestValidatorId,
    method_requestModels,
    method_requestParameters,
    method_authorizerId,
    method_operationName,
    method_authorizationType,
    method_apiKeyRequired,
    method_methodIntegration,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to update an existing Method resource.
--
-- /See:/ 'newUpdateMethod' smart constructor.
data UpdateMethod = UpdateMethod'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The Resource identifier for the Method resource.
    resourceId :: Prelude.Text,
    -- | [Required] The HTTP verb of the Method resource.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateMethod_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'restApiId', 'updateMethod_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'updateMethod_resourceId' - [Required] The Resource identifier for the Method resource.
--
-- 'httpMethod', 'updateMethod_httpMethod' - [Required] The HTTP verb of the Method resource.
newUpdateMethod ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  UpdateMethod
newUpdateMethod pRestApiId_ pResourceId_ pHttpMethod_ =
  UpdateMethod'
    { patchOperations = Prelude.Nothing,
      restApiId = pRestApiId_,
      resourceId = pResourceId_,
      httpMethod = pHttpMethod_
    }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateMethod_patchOperations :: Lens.Lens' UpdateMethod (Prelude.Maybe [PatchOperation])
updateMethod_patchOperations = Lens.lens (\UpdateMethod' {patchOperations} -> patchOperations) (\s@UpdateMethod' {} a -> s {patchOperations = a} :: UpdateMethod) Prelude.. Lens.mapping Lens.coerced

-- | [Required] The string identifier of the associated RestApi.
updateMethod_restApiId :: Lens.Lens' UpdateMethod Prelude.Text
updateMethod_restApiId = Lens.lens (\UpdateMethod' {restApiId} -> restApiId) (\s@UpdateMethod' {} a -> s {restApiId = a} :: UpdateMethod)

-- | [Required] The Resource identifier for the Method resource.
updateMethod_resourceId :: Lens.Lens' UpdateMethod Prelude.Text
updateMethod_resourceId = Lens.lens (\UpdateMethod' {resourceId} -> resourceId) (\s@UpdateMethod' {} a -> s {resourceId = a} :: UpdateMethod)

-- | [Required] The HTTP verb of the Method resource.
updateMethod_httpMethod :: Lens.Lens' UpdateMethod Prelude.Text
updateMethod_httpMethod = Lens.lens (\UpdateMethod' {httpMethod} -> httpMethod) (\s@UpdateMethod' {} a -> s {httpMethod = a} :: UpdateMethod)

instance Core.AWSRequest UpdateMethod where
  type AWSResponse UpdateMethod = Method
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateMethod

instance Prelude.NFData UpdateMethod

instance Core.ToHeaders UpdateMethod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateMethod where
  toJSON UpdateMethod' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateMethod where
  toPath UpdateMethod' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod
      ]

instance Core.ToQuery UpdateMethod where
  toQuery = Prelude.const Prelude.mempty
