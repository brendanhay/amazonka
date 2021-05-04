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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a delete integration response request.
--
-- /See:/ 'newDeleteIntegrationResponse' smart constructor.
data DeleteIntegrationResponse = DeleteIntegrationResponse'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] Specifies a delete integration response request\'s resource
    -- identifier.
    resourceId :: Prelude.Text,
    -- | [Required] Specifies a delete integration response request\'s HTTP
    -- method.
    httpMethod :: Prelude.Text,
    -- | [Required] Specifies a delete integration response request\'s status
    -- code.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'statusCode'
  Prelude.Text ->
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
deleteIntegrationResponse_restApiId :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_restApiId = Lens.lens (\DeleteIntegrationResponse' {restApiId} -> restApiId) (\s@DeleteIntegrationResponse' {} a -> s {restApiId = a} :: DeleteIntegrationResponse)

-- | [Required] Specifies a delete integration response request\'s resource
-- identifier.
deleteIntegrationResponse_resourceId :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_resourceId = Lens.lens (\DeleteIntegrationResponse' {resourceId} -> resourceId) (\s@DeleteIntegrationResponse' {} a -> s {resourceId = a} :: DeleteIntegrationResponse)

-- | [Required] Specifies a delete integration response request\'s HTTP
-- method.
deleteIntegrationResponse_httpMethod :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_httpMethod = Lens.lens (\DeleteIntegrationResponse' {httpMethod} -> httpMethod) (\s@DeleteIntegrationResponse' {} a -> s {httpMethod = a} :: DeleteIntegrationResponse)

-- | [Required] Specifies a delete integration response request\'s status
-- code.
deleteIntegrationResponse_statusCode :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_statusCode = Lens.lens (\DeleteIntegrationResponse' {statusCode} -> statusCode) (\s@DeleteIntegrationResponse' {} a -> s {statusCode = a} :: DeleteIntegrationResponse)

instance Prelude.AWSRequest DeleteIntegrationResponse where
  type
    Rs DeleteIntegrationResponse =
      DeleteIntegrationResponseResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteIntegrationResponseResponse'

instance Prelude.Hashable DeleteIntegrationResponse

instance Prelude.NFData DeleteIntegrationResponse

instance Prelude.ToHeaders DeleteIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteIntegrationResponse where
  toPath DeleteIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/resources/",
        Prelude.toBS resourceId,
        "/methods/",
        Prelude.toBS httpMethod,
        "/integration/responses/",
        Prelude.toBS statusCode
      ]

instance Prelude.ToQuery DeleteIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntegrationResponseResponse' smart constructor.
data DeleteIntegrationResponseResponse = DeleteIntegrationResponseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntegrationResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntegrationResponseResponse ::
  DeleteIntegrationResponseResponse
newDeleteIntegrationResponseResponse =
  DeleteIntegrationResponseResponse'

instance
  Prelude.NFData
    DeleteIntegrationResponseResponse
