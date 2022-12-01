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
-- Module      : Amazonka.ApiGatewayV2.DeleteRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Route.
module Amazonka.ApiGatewayV2.DeleteRoute
  ( -- * Creating a Request
    DeleteRoute (..),
    newDeleteRoute,

    -- * Request Lenses
    deleteRoute_apiId,
    deleteRoute_routeId,

    -- * Destructuring the Response
    DeleteRouteResponse' (..),
    newDeleteRouteResponse',
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The route ID.
    routeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteRoute_apiId' - The API identifier.
--
-- 'routeId', 'deleteRoute_routeId' - The route ID.
newDeleteRoute ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'routeId'
  Prelude.Text ->
  DeleteRoute
newDeleteRoute pApiId_ pRouteId_ =
  DeleteRoute' {apiId = pApiId_, routeId = pRouteId_}

-- | The API identifier.
deleteRoute_apiId :: Lens.Lens' DeleteRoute Prelude.Text
deleteRoute_apiId = Lens.lens (\DeleteRoute' {apiId} -> apiId) (\s@DeleteRoute' {} a -> s {apiId = a} :: DeleteRoute)

-- | The route ID.
deleteRoute_routeId :: Lens.Lens' DeleteRoute Prelude.Text
deleteRoute_routeId = Lens.lens (\DeleteRoute' {routeId} -> routeId) (\s@DeleteRoute' {} a -> s {routeId = a} :: DeleteRoute)

instance Core.AWSRequest DeleteRoute where
  type AWSResponse DeleteRoute = DeleteRouteResponse'
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteRouteResponse''

instance Prelude.Hashable DeleteRoute where
  hashWithSalt _salt DeleteRoute' {..} =
    _salt `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` routeId

instance Prelude.NFData DeleteRoute where
  rnf DeleteRoute' {..} =
    Prelude.rnf apiId `Prelude.seq` Prelude.rnf routeId

instance Core.ToHeaders DeleteRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteRoute where
  toPath DeleteRoute' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/routes/",
        Core.toBS routeId
      ]

instance Core.ToQuery DeleteRoute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRouteResponse'' smart constructor.
data DeleteRouteResponse' = DeleteRouteResponse''
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRouteResponse'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRouteResponse' ::
  DeleteRouteResponse'
newDeleteRouteResponse' = DeleteRouteResponse''

instance Prelude.NFData DeleteRouteResponse' where
  rnf _ = ()
