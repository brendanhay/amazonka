{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update an existing MethodResponse resource.
--
-- /See:/ 'newUpdateMethodResponse' smart constructor.
data UpdateMethodResponse = UpdateMethodResponse'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The Resource identifier for the MethodResponse resource.
    resourceId :: Prelude.Text,
    -- | [Required] The HTTP verb of the Method resource.
    httpMethod :: Prelude.Text,
    -- | [Required] The status code for the MethodResponse resource.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'statusCode'
  Prelude.Text ->
  UpdateMethodResponse
newUpdateMethodResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    UpdateMethodResponse'
      { patchOperations =
          Prelude.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateMethodResponse_patchOperations :: Lens.Lens' UpdateMethodResponse (Prelude.Maybe [PatchOperation])
updateMethodResponse_patchOperations = Lens.lens (\UpdateMethodResponse' {patchOperations} -> patchOperations) (\s@UpdateMethodResponse' {} a -> s {patchOperations = a} :: UpdateMethodResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | [Required] The string identifier of the associated RestApi.
updateMethodResponse_restApiId :: Lens.Lens' UpdateMethodResponse Prelude.Text
updateMethodResponse_restApiId = Lens.lens (\UpdateMethodResponse' {restApiId} -> restApiId) (\s@UpdateMethodResponse' {} a -> s {restApiId = a} :: UpdateMethodResponse)

-- | [Required] The Resource identifier for the MethodResponse resource.
updateMethodResponse_resourceId :: Lens.Lens' UpdateMethodResponse Prelude.Text
updateMethodResponse_resourceId = Lens.lens (\UpdateMethodResponse' {resourceId} -> resourceId) (\s@UpdateMethodResponse' {} a -> s {resourceId = a} :: UpdateMethodResponse)

-- | [Required] The HTTP verb of the Method resource.
updateMethodResponse_httpMethod :: Lens.Lens' UpdateMethodResponse Prelude.Text
updateMethodResponse_httpMethod = Lens.lens (\UpdateMethodResponse' {httpMethod} -> httpMethod) (\s@UpdateMethodResponse' {} a -> s {httpMethod = a} :: UpdateMethodResponse)

-- | [Required] The status code for the MethodResponse resource.
updateMethodResponse_statusCode :: Lens.Lens' UpdateMethodResponse Prelude.Text
updateMethodResponse_statusCode = Lens.lens (\UpdateMethodResponse' {statusCode} -> statusCode) (\s@UpdateMethodResponse' {} a -> s {statusCode = a} :: UpdateMethodResponse)

instance Prelude.AWSRequest UpdateMethodResponse where
  type Rs UpdateMethodResponse = MethodResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable UpdateMethodResponse

instance Prelude.NFData UpdateMethodResponse

instance Prelude.ToHeaders UpdateMethodResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON UpdateMethodResponse where
  toJSON UpdateMethodResponse' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("patchOperations" Prelude..=)
              Prelude.<$> patchOperations
          ]
      )

instance Prelude.ToPath UpdateMethodResponse where
  toPath UpdateMethodResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/resources/",
        Prelude.toBS resourceId,
        "/methods/",
        Prelude.toBS httpMethod,
        "/responses/",
        Prelude.toBS statusCode
      ]

instance Prelude.ToQuery UpdateMethodResponse where
  toQuery = Prelude.const Prelude.mempty
