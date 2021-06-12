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
-- Module      : Network.AWS.APIGateway.UpdateMethodResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing MethodResponse resource.
module Network.AWS.APIGateway.UpdateMethodResponse
  ( -- * Creating a Request
    UpdateMethodResponse (..),
    newUpdateMethodResponse,

    -- * Request Lenses
    updateMethodResponse_patchOperations,
    updateMethodResponse_restApiId,
    updateMethodResponse_resourceId,
    updateMethodResponse_httpMethod,
    updateMethodResponse_statusCode,

    -- * Destructuring the Response
    MethodResponse (..),
    newMethodResponse,

    -- * Response Lenses
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update an existing MethodResponse resource.
--
-- /See:/ 'newUpdateMethodResponse' smart constructor.
data UpdateMethodResponse = UpdateMethodResponse'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Core.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The Resource identifier for the MethodResponse resource.
    resourceId :: Core.Text,
    -- | [Required] The HTTP verb of the Method resource.
    httpMethod :: Core.Text,
    -- | [Required] The status code for the MethodResponse resource.
    statusCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateMethodResponse_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'restApiId', 'updateMethodResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'updateMethodResponse_resourceId' - [Required] The Resource identifier for the MethodResponse resource.
--
-- 'httpMethod', 'updateMethodResponse_httpMethod' - [Required] The HTTP verb of the Method resource.
--
-- 'statusCode', 'updateMethodResponse_statusCode' - [Required] The status code for the MethodResponse resource.
newUpdateMethodResponse ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'httpMethod'
  Core.Text ->
  -- | 'statusCode'
  Core.Text ->
  UpdateMethodResponse
newUpdateMethodResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    UpdateMethodResponse'
      { patchOperations =
          Core.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateMethodResponse_patchOperations :: Lens.Lens' UpdateMethodResponse (Core.Maybe [PatchOperation])
updateMethodResponse_patchOperations = Lens.lens (\UpdateMethodResponse' {patchOperations} -> patchOperations) (\s@UpdateMethodResponse' {} a -> s {patchOperations = a} :: UpdateMethodResponse) Core.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
updateMethodResponse_restApiId :: Lens.Lens' UpdateMethodResponse Core.Text
updateMethodResponse_restApiId = Lens.lens (\UpdateMethodResponse' {restApiId} -> restApiId) (\s@UpdateMethodResponse' {} a -> s {restApiId = a} :: UpdateMethodResponse)

-- | [Required] The Resource identifier for the MethodResponse resource.
updateMethodResponse_resourceId :: Lens.Lens' UpdateMethodResponse Core.Text
updateMethodResponse_resourceId = Lens.lens (\UpdateMethodResponse' {resourceId} -> resourceId) (\s@UpdateMethodResponse' {} a -> s {resourceId = a} :: UpdateMethodResponse)

-- | [Required] The HTTP verb of the Method resource.
updateMethodResponse_httpMethod :: Lens.Lens' UpdateMethodResponse Core.Text
updateMethodResponse_httpMethod = Lens.lens (\UpdateMethodResponse' {httpMethod} -> httpMethod) (\s@UpdateMethodResponse' {} a -> s {httpMethod = a} :: UpdateMethodResponse)

-- | [Required] The status code for the MethodResponse resource.
updateMethodResponse_statusCode :: Lens.Lens' UpdateMethodResponse Core.Text
updateMethodResponse_statusCode = Lens.lens (\UpdateMethodResponse' {statusCode} -> statusCode) (\s@UpdateMethodResponse' {} a -> s {statusCode = a} :: UpdateMethodResponse)

instance Core.AWSRequest UpdateMethodResponse where
  type
    AWSResponse UpdateMethodResponse =
      MethodResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable UpdateMethodResponse

instance Core.NFData UpdateMethodResponse

instance Core.ToHeaders UpdateMethodResponse where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateMethodResponse where
  toJSON UpdateMethodResponse' {..} =
    Core.object
      ( Core.catMaybes
          [ ("patchOperations" Core..=)
              Core.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateMethodResponse where
  toPath UpdateMethodResponse' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod,
        "/responses/",
        Core.toBS statusCode
      ]

instance Core.ToQuery UpdateMethodResponse where
  toQuery = Core.const Core.mempty
