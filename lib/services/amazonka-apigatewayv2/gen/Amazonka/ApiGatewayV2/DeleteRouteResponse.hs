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
-- Module      : Amazonka.ApiGatewayV2.DeleteRouteResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a RouteResponse.
module Amazonka.ApiGatewayV2.DeleteRouteResponse
  ( -- * Creating a Request
    DeleteRouteResponse (..),
    newDeleteRouteResponse,

    -- * Request Lenses
    deleteRouteResponse_routeResponseId,
    deleteRouteResponse_apiId,
    deleteRouteResponse_routeId,

    -- * Destructuring the Response
    DeleteRouteResponseResponse (..),
    newDeleteRouteResponseResponse,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  { -- | The route response ID.
    routeResponseId :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The route ID.
    routeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routeResponseId', 'deleteRouteResponse_routeResponseId' - The route response ID.
--
-- 'apiId', 'deleteRouteResponse_apiId' - The API identifier.
--
-- 'routeId', 'deleteRouteResponse_routeId' - The route ID.
newDeleteRouteResponse ::
  -- | 'routeResponseId'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  -- | 'routeId'
  Prelude.Text ->
  DeleteRouteResponse
newDeleteRouteResponse
  pRouteResponseId_
  pApiId_
  pRouteId_ =
    DeleteRouteResponse'
      { routeResponseId =
          pRouteResponseId_,
        apiId = pApiId_,
        routeId = pRouteId_
      }

-- | The route response ID.
deleteRouteResponse_routeResponseId :: Lens.Lens' DeleteRouteResponse Prelude.Text
deleteRouteResponse_routeResponseId = Lens.lens (\DeleteRouteResponse' {routeResponseId} -> routeResponseId) (\s@DeleteRouteResponse' {} a -> s {routeResponseId = a} :: DeleteRouteResponse)

-- | The API identifier.
deleteRouteResponse_apiId :: Lens.Lens' DeleteRouteResponse Prelude.Text
deleteRouteResponse_apiId = Lens.lens (\DeleteRouteResponse' {apiId} -> apiId) (\s@DeleteRouteResponse' {} a -> s {apiId = a} :: DeleteRouteResponse)

-- | The route ID.
deleteRouteResponse_routeId :: Lens.Lens' DeleteRouteResponse Prelude.Text
deleteRouteResponse_routeId = Lens.lens (\DeleteRouteResponse' {routeId} -> routeId) (\s@DeleteRouteResponse' {} a -> s {routeId = a} :: DeleteRouteResponse)

instance Core.AWSRequest DeleteRouteResponse where
  type
    AWSResponse DeleteRouteResponse =
      DeleteRouteResponseResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteRouteResponseResponse'

instance Prelude.Hashable DeleteRouteResponse where
  hashWithSalt _salt DeleteRouteResponse' {..} =
    _salt
      `Prelude.hashWithSalt` routeResponseId
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` routeId

instance Prelude.NFData DeleteRouteResponse where
  rnf DeleteRouteResponse' {..} =
    Prelude.rnf routeResponseId
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf routeId

instance Data.ToHeaders DeleteRouteResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRouteResponse where
  toPath DeleteRouteResponse' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/routes/",
        Data.toBS routeId,
        "/routeresponses/",
        Data.toBS routeResponseId
      ]

instance Data.ToQuery DeleteRouteResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRouteResponseResponse' smart constructor.
data DeleteRouteResponseResponse = DeleteRouteResponseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRouteResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRouteResponseResponse ::
  DeleteRouteResponseResponse
newDeleteRouteResponseResponse =
  DeleteRouteResponseResponse'

instance Prelude.NFData DeleteRouteResponseResponse where
  rnf _ = ()
