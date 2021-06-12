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
-- Module      : Network.AWS.APIGateway.UpdateMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Method resource.
module Network.AWS.APIGateway.UpdateMethod
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
    method_httpMethod,
    method_methodIntegration,
    method_apiKeyRequired,
    method_authorizationType,
    method_requestModels,
    method_operationName,
    method_requestValidatorId,
    method_methodResponses,
    method_authorizerId,
    method_requestParameters,
    method_authorizationScopes,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update an existing Method resource.
--
-- /See:/ 'newUpdateMethod' smart constructor.
data UpdateMethod = UpdateMethod'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Core.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The Resource identifier for the Method resource.
    resourceId :: Core.Text,
    -- | [Required] The HTTP verb of the Method resource.
    httpMethod :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'httpMethod'
  Core.Text ->
  UpdateMethod
newUpdateMethod pRestApiId_ pResourceId_ pHttpMethod_ =
  UpdateMethod'
    { patchOperations = Core.Nothing,
      restApiId = pRestApiId_,
      resourceId = pResourceId_,
      httpMethod = pHttpMethod_
    }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateMethod_patchOperations :: Lens.Lens' UpdateMethod (Core.Maybe [PatchOperation])
updateMethod_patchOperations = Lens.lens (\UpdateMethod' {patchOperations} -> patchOperations) (\s@UpdateMethod' {} a -> s {patchOperations = a} :: UpdateMethod) Core.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
updateMethod_restApiId :: Lens.Lens' UpdateMethod Core.Text
updateMethod_restApiId = Lens.lens (\UpdateMethod' {restApiId} -> restApiId) (\s@UpdateMethod' {} a -> s {restApiId = a} :: UpdateMethod)

-- | [Required] The Resource identifier for the Method resource.
updateMethod_resourceId :: Lens.Lens' UpdateMethod Core.Text
updateMethod_resourceId = Lens.lens (\UpdateMethod' {resourceId} -> resourceId) (\s@UpdateMethod' {} a -> s {resourceId = a} :: UpdateMethod)

-- | [Required] The HTTP verb of the Method resource.
updateMethod_httpMethod :: Lens.Lens' UpdateMethod Core.Text
updateMethod_httpMethod = Lens.lens (\UpdateMethod' {httpMethod} -> httpMethod) (\s@UpdateMethod' {} a -> s {httpMethod = a} :: UpdateMethod)

instance Core.AWSRequest UpdateMethod where
  type AWSResponse UpdateMethod = Method
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable UpdateMethod

instance Core.NFData UpdateMethod

instance Core.ToHeaders UpdateMethod where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateMethod where
  toJSON UpdateMethod' {..} =
    Core.object
      ( Core.catMaybes
          [ ("patchOperations" Core..=)
              Core.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateMethod where
  toPath UpdateMethod' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod
      ]

instance Core.ToQuery UpdateMethod where
  toQuery = Core.const Core.mempty
