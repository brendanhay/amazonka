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
-- Module      : Amazonka.APIGateway.DeleteMethodResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing MethodResponse resource.
module Amazonka.APIGateway.DeleteMethodResponse
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to delete an existing MethodResponse resource.
--
-- /See:/ 'newDeleteMethodResponse' smart constructor.
data DeleteMethodResponse = DeleteMethodResponse'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The Resource identifier for the MethodResponse resource.
    resourceId :: Prelude.Text,
    -- | The HTTP verb of the Method resource.
    httpMethod :: Prelude.Text,
    -- | The status code identifier for the MethodResponse resource.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteMethodResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'deleteMethodResponse_resourceId' - The Resource identifier for the MethodResponse resource.
--
-- 'httpMethod', 'deleteMethodResponse_httpMethod' - The HTTP verb of the Method resource.
--
-- 'statusCode', 'deleteMethodResponse_statusCode' - The status code identifier for the MethodResponse resource.
newDeleteMethodResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'statusCode'
  Prelude.Text ->
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

-- | The string identifier of the associated RestApi.
deleteMethodResponse_restApiId :: Lens.Lens' DeleteMethodResponse Prelude.Text
deleteMethodResponse_restApiId = Lens.lens (\DeleteMethodResponse' {restApiId} -> restApiId) (\s@DeleteMethodResponse' {} a -> s {restApiId = a} :: DeleteMethodResponse)

-- | The Resource identifier for the MethodResponse resource.
deleteMethodResponse_resourceId :: Lens.Lens' DeleteMethodResponse Prelude.Text
deleteMethodResponse_resourceId = Lens.lens (\DeleteMethodResponse' {resourceId} -> resourceId) (\s@DeleteMethodResponse' {} a -> s {resourceId = a} :: DeleteMethodResponse)

-- | The HTTP verb of the Method resource.
deleteMethodResponse_httpMethod :: Lens.Lens' DeleteMethodResponse Prelude.Text
deleteMethodResponse_httpMethod = Lens.lens (\DeleteMethodResponse' {httpMethod} -> httpMethod) (\s@DeleteMethodResponse' {} a -> s {httpMethod = a} :: DeleteMethodResponse)

-- | The status code identifier for the MethodResponse resource.
deleteMethodResponse_statusCode :: Lens.Lens' DeleteMethodResponse Prelude.Text
deleteMethodResponse_statusCode = Lens.lens (\DeleteMethodResponse' {statusCode} -> statusCode) (\s@DeleteMethodResponse' {} a -> s {statusCode = a} :: DeleteMethodResponse)

instance Core.AWSRequest DeleteMethodResponse where
  type
    AWSResponse DeleteMethodResponse =
      DeleteMethodResponseResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteMethodResponseResponse'

instance Prelude.Hashable DeleteMethodResponse where
  hashWithSalt _salt DeleteMethodResponse' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData DeleteMethodResponse where
  rnf DeleteMethodResponse' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf statusCode

instance Core.ToHeaders DeleteMethodResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath DeleteMethodResponse where
  toPath DeleteMethodResponse' {..} =
    Prelude.mconcat
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
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMethodResponseResponse' smart constructor.
data DeleteMethodResponseResponse = DeleteMethodResponseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMethodResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMethodResponseResponse ::
  DeleteMethodResponseResponse
newDeleteMethodResponseResponse =
  DeleteMethodResponseResponse'

instance Prelude.NFData DeleteMethodResponseResponse where
  rnf _ = ()
