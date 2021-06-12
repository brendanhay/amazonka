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
-- Module      : Network.AWS.APIGateway.DeleteMethodResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing MethodResponse resource.
module Network.AWS.APIGateway.DeleteMethodResponse
  ( -- * Creating a Request
    DeleteMethodResponse (..),
    newDeleteMethodResponse,

    -- * Request Lenses
    deleteMethodResponse_restApiId,
    deleteMethodResponse_resourceId,
    deleteMethodResponse_httpMethod,
    deleteMethodResponse_statusCode,

    -- * Destructuring the Response
    DeleteMethodResponseResponse (..),
    newDeleteMethodResponseResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete an existing MethodResponse resource.
--
-- /See:/ 'newDeleteMethodResponse' smart constructor.
data DeleteMethodResponse = DeleteMethodResponse'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The Resource identifier for the MethodResponse resource.
    resourceId :: Core.Text,
    -- | [Required] The HTTP verb of the Method resource.
    httpMethod :: Core.Text,
    -- | [Required] The status code identifier for the MethodResponse resource.
    statusCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteMethodResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'deleteMethodResponse_resourceId' - [Required] The Resource identifier for the MethodResponse resource.
--
-- 'httpMethod', 'deleteMethodResponse_httpMethod' - [Required] The HTTP verb of the Method resource.
--
-- 'statusCode', 'deleteMethodResponse_statusCode' - [Required] The status code identifier for the MethodResponse resource.
newDeleteMethodResponse ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'httpMethod'
  Core.Text ->
  -- | 'statusCode'
  Core.Text ->
  DeleteMethodResponse
newDeleteMethodResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    DeleteMethodResponse'
      { restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | [Required] The string identifier of the associated RestApi.
deleteMethodResponse_restApiId :: Lens.Lens' DeleteMethodResponse Core.Text
deleteMethodResponse_restApiId = Lens.lens (\DeleteMethodResponse' {restApiId} -> restApiId) (\s@DeleteMethodResponse' {} a -> s {restApiId = a} :: DeleteMethodResponse)

-- | [Required] The Resource identifier for the MethodResponse resource.
deleteMethodResponse_resourceId :: Lens.Lens' DeleteMethodResponse Core.Text
deleteMethodResponse_resourceId = Lens.lens (\DeleteMethodResponse' {resourceId} -> resourceId) (\s@DeleteMethodResponse' {} a -> s {resourceId = a} :: DeleteMethodResponse)

-- | [Required] The HTTP verb of the Method resource.
deleteMethodResponse_httpMethod :: Lens.Lens' DeleteMethodResponse Core.Text
deleteMethodResponse_httpMethod = Lens.lens (\DeleteMethodResponse' {httpMethod} -> httpMethod) (\s@DeleteMethodResponse' {} a -> s {httpMethod = a} :: DeleteMethodResponse)

-- | [Required] The status code identifier for the MethodResponse resource.
deleteMethodResponse_statusCode :: Lens.Lens' DeleteMethodResponse Core.Text
deleteMethodResponse_statusCode = Lens.lens (\DeleteMethodResponse' {statusCode} -> statusCode) (\s@DeleteMethodResponse' {} a -> s {statusCode = a} :: DeleteMethodResponse)

instance Core.AWSRequest DeleteMethodResponse where
  type
    AWSResponse DeleteMethodResponse =
      DeleteMethodResponseResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteMethodResponseResponse'

instance Core.Hashable DeleteMethodResponse

instance Core.NFData DeleteMethodResponse

instance Core.ToHeaders DeleteMethodResponse where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteMethodResponse where
  toPath DeleteMethodResponse' {..} =
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

instance Core.ToQuery DeleteMethodResponse where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteMethodResponseResponse' smart constructor.
data DeleteMethodResponseResponse = DeleteMethodResponseResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMethodResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMethodResponseResponse ::
  DeleteMethodResponseResponse
newDeleteMethodResponseResponse =
  DeleteMethodResponseResponse'

instance Core.NFData DeleteMethodResponseResponse
