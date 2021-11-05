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
-- Module      : Network.AWS.ApiGatewayV2.DeleteRouteResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a RouteResponse.
module Network.AWS.ApiGatewayV2.DeleteRouteResponse
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

import Network.AWS.ApiGatewayV2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteRouteResponseResponse'

instance Prelude.Hashable DeleteRouteResponse

instance Prelude.NFData DeleteRouteResponse

instance Core.ToHeaders DeleteRouteResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteRouteResponse where
  toPath DeleteRouteResponse' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/routes/",
        Core.toBS routeId,
        "/routeresponses/",
        Core.toBS routeResponseId
      ]

instance Core.ToQuery DeleteRouteResponse where
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

instance Prelude.NFData DeleteRouteResponseResponse
