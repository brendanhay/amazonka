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
-- Module      : Amazonka.APIGateway.DeleteIntegrationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a delete integration response.
module Amazonka.APIGateway.DeleteIntegrationResponse
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a delete integration response request.
--
-- /See:/ 'newDeleteIntegrationResponse' smart constructor.
data DeleteIntegrationResponse = DeleteIntegrationResponse'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | Specifies a delete integration response request\'s resource identifier.
    resourceId :: Prelude.Text,
    -- | Specifies a delete integration response request\'s HTTP method.
    httpMethod :: Prelude.Text,
    -- | Specifies a delete integration response request\'s status code.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteIntegrationResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'deleteIntegrationResponse_resourceId' - Specifies a delete integration response request\'s resource identifier.
--
-- 'httpMethod', 'deleteIntegrationResponse_httpMethod' - Specifies a delete integration response request\'s HTTP method.
--
-- 'statusCode', 'deleteIntegrationResponse_statusCode' - Specifies a delete integration response request\'s status code.
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

-- | The string identifier of the associated RestApi.
deleteIntegrationResponse_restApiId :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_restApiId = Lens.lens (\DeleteIntegrationResponse' {restApiId} -> restApiId) (\s@DeleteIntegrationResponse' {} a -> s {restApiId = a} :: DeleteIntegrationResponse)

-- | Specifies a delete integration response request\'s resource identifier.
deleteIntegrationResponse_resourceId :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_resourceId = Lens.lens (\DeleteIntegrationResponse' {resourceId} -> resourceId) (\s@DeleteIntegrationResponse' {} a -> s {resourceId = a} :: DeleteIntegrationResponse)

-- | Specifies a delete integration response request\'s HTTP method.
deleteIntegrationResponse_httpMethod :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_httpMethod = Lens.lens (\DeleteIntegrationResponse' {httpMethod} -> httpMethod) (\s@DeleteIntegrationResponse' {} a -> s {httpMethod = a} :: DeleteIntegrationResponse)

-- | Specifies a delete integration response request\'s status code.
deleteIntegrationResponse_statusCode :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_statusCode = Lens.lens (\DeleteIntegrationResponse' {statusCode} -> statusCode) (\s@DeleteIntegrationResponse' {} a -> s {statusCode = a} :: DeleteIntegrationResponse)

instance Core.AWSRequest DeleteIntegrationResponse where
  type
    AWSResponse DeleteIntegrationResponse =
      DeleteIntegrationResponseResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteIntegrationResponseResponse'

instance Prelude.Hashable DeleteIntegrationResponse where
  hashWithSalt _salt DeleteIntegrationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData DeleteIntegrationResponse where
  rnf DeleteIntegrationResponse' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf statusCode

instance Data.ToHeaders DeleteIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteIntegrationResponse where
  toPath DeleteIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod,
        "/integration/responses/",
        Data.toBS statusCode
      ]

instance Data.ToQuery DeleteIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntegrationResponseResponse' smart constructor.
data DeleteIntegrationResponseResponse = DeleteIntegrationResponseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
