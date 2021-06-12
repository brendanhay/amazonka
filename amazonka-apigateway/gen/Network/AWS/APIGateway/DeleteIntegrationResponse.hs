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
-- Module      : Network.AWS.APIGateway.DeleteIntegrationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a delete integration response.
module Network.AWS.APIGateway.DeleteIntegrationResponse
  ( -- * Creating a Request
    DeleteIntegrationResponse (..),
    newDeleteIntegrationResponse,

    -- * Request Lenses
    deleteIntegrationResponse_restApiId,
    deleteIntegrationResponse_resourceId,
    deleteIntegrationResponse_httpMethod,
    deleteIntegrationResponse_statusCode,

    -- * Destructuring the Response
    DeleteIntegrationResponseResponse (..),
    newDeleteIntegrationResponseResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a delete integration response request.
--
-- /See:/ 'newDeleteIntegrationResponse' smart constructor.
data DeleteIntegrationResponse = DeleteIntegrationResponse'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] Specifies a delete integration response request\'s resource
    -- identifier.
    resourceId :: Core.Text,
    -- | [Required] Specifies a delete integration response request\'s HTTP
    -- method.
    httpMethod :: Core.Text,
    -- | [Required] Specifies a delete integration response request\'s status
    -- code.
    statusCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteIntegrationResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'deleteIntegrationResponse_resourceId' - [Required] Specifies a delete integration response request\'s resource
-- identifier.
--
-- 'httpMethod', 'deleteIntegrationResponse_httpMethod' - [Required] Specifies a delete integration response request\'s HTTP
-- method.
--
-- 'statusCode', 'deleteIntegrationResponse_statusCode' - [Required] Specifies a delete integration response request\'s status
-- code.
newDeleteIntegrationResponse ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'httpMethod'
  Core.Text ->
  -- | 'statusCode'
  Core.Text ->
  DeleteIntegrationResponse
newDeleteIntegrationResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    DeleteIntegrationResponse'
      { restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | [Required] The string identifier of the associated RestApi.
deleteIntegrationResponse_restApiId :: Lens.Lens' DeleteIntegrationResponse Core.Text
deleteIntegrationResponse_restApiId = Lens.lens (\DeleteIntegrationResponse' {restApiId} -> restApiId) (\s@DeleteIntegrationResponse' {} a -> s {restApiId = a} :: DeleteIntegrationResponse)

-- | [Required] Specifies a delete integration response request\'s resource
-- identifier.
deleteIntegrationResponse_resourceId :: Lens.Lens' DeleteIntegrationResponse Core.Text
deleteIntegrationResponse_resourceId = Lens.lens (\DeleteIntegrationResponse' {resourceId} -> resourceId) (\s@DeleteIntegrationResponse' {} a -> s {resourceId = a} :: DeleteIntegrationResponse)

-- | [Required] Specifies a delete integration response request\'s HTTP
-- method.
deleteIntegrationResponse_httpMethod :: Lens.Lens' DeleteIntegrationResponse Core.Text
deleteIntegrationResponse_httpMethod = Lens.lens (\DeleteIntegrationResponse' {httpMethod} -> httpMethod) (\s@DeleteIntegrationResponse' {} a -> s {httpMethod = a} :: DeleteIntegrationResponse)

-- | [Required] Specifies a delete integration response request\'s status
-- code.
deleteIntegrationResponse_statusCode :: Lens.Lens' DeleteIntegrationResponse Core.Text
deleteIntegrationResponse_statusCode = Lens.lens (\DeleteIntegrationResponse' {statusCode} -> statusCode) (\s@DeleteIntegrationResponse' {} a -> s {statusCode = a} :: DeleteIntegrationResponse)

instance Core.AWSRequest DeleteIntegrationResponse where
  type
    AWSResponse DeleteIntegrationResponse =
      DeleteIntegrationResponseResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteIntegrationResponseResponse'

instance Core.Hashable DeleteIntegrationResponse

instance Core.NFData DeleteIntegrationResponse

instance Core.ToHeaders DeleteIntegrationResponse where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteIntegrationResponse where
  toPath DeleteIntegrationResponse' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod,
        "/integration/responses/",
        Core.toBS statusCode
      ]

instance Core.ToQuery DeleteIntegrationResponse where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteIntegrationResponseResponse' smart constructor.
data DeleteIntegrationResponseResponse = DeleteIntegrationResponseResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteIntegrationResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntegrationResponseResponse ::
  DeleteIntegrationResponseResponse
newDeleteIntegrationResponseResponse =
  DeleteIntegrationResponseResponse'

instance
  Core.NFData
    DeleteIntegrationResponseResponse
