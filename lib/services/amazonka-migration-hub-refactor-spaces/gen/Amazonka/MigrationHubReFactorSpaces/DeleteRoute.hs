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
-- Module      : Amazonka.MigrationHubReFactorSpaces.DeleteRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Web Services Migration Hub Refactor Spaces route.
module Amazonka.MigrationHubReFactorSpaces.DeleteRoute
  ( -- * Creating a Request
    DeleteRoute (..),
    newDeleteRoute,

    -- * Request Lenses
    deleteRoute_applicationIdentifier,
    deleteRoute_environmentIdentifier,
    deleteRoute_routeIdentifier,

    -- * Destructuring the Response
    DeleteRouteResponse (..),
    newDeleteRouteResponse,

    -- * Response Lenses
    deleteRouteResponse_arn,
    deleteRouteResponse_state,
    deleteRouteResponse_lastUpdatedTime,
    deleteRouteResponse_routeId,
    deleteRouteResponse_applicationId,
    deleteRouteResponse_serviceId,
    deleteRouteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { -- | The ID of the application to delete the route from.
    applicationIdentifier :: Prelude.Text,
    -- | The ID of the environment to delete the route from.
    environmentIdentifier :: Prelude.Text,
    -- | The ID of the route to delete.
    routeIdentifier :: Prelude.Text
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
-- 'applicationIdentifier', 'deleteRoute_applicationIdentifier' - The ID of the application to delete the route from.
--
-- 'environmentIdentifier', 'deleteRoute_environmentIdentifier' - The ID of the environment to delete the route from.
--
-- 'routeIdentifier', 'deleteRoute_routeIdentifier' - The ID of the route to delete.
newDeleteRoute ::
  -- | 'applicationIdentifier'
  Prelude.Text ->
  -- | 'environmentIdentifier'
  Prelude.Text ->
  -- | 'routeIdentifier'
  Prelude.Text ->
  DeleteRoute
newDeleteRoute
  pApplicationIdentifier_
  pEnvironmentIdentifier_
  pRouteIdentifier_ =
    DeleteRoute'
      { applicationIdentifier =
          pApplicationIdentifier_,
        environmentIdentifier = pEnvironmentIdentifier_,
        routeIdentifier = pRouteIdentifier_
      }

-- | The ID of the application to delete the route from.
deleteRoute_applicationIdentifier :: Lens.Lens' DeleteRoute Prelude.Text
deleteRoute_applicationIdentifier = Lens.lens (\DeleteRoute' {applicationIdentifier} -> applicationIdentifier) (\s@DeleteRoute' {} a -> s {applicationIdentifier = a} :: DeleteRoute)

-- | The ID of the environment to delete the route from.
deleteRoute_environmentIdentifier :: Lens.Lens' DeleteRoute Prelude.Text
deleteRoute_environmentIdentifier = Lens.lens (\DeleteRoute' {environmentIdentifier} -> environmentIdentifier) (\s@DeleteRoute' {} a -> s {environmentIdentifier = a} :: DeleteRoute)

-- | The ID of the route to delete.
deleteRoute_routeIdentifier :: Lens.Lens' DeleteRoute Prelude.Text
deleteRoute_routeIdentifier = Lens.lens (\DeleteRoute' {routeIdentifier} -> routeIdentifier) (\s@DeleteRoute' {} a -> s {routeIdentifier = a} :: DeleteRoute)

instance Core.AWSRequest DeleteRoute where
  type AWSResponse DeleteRoute = DeleteRouteResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRouteResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "LastUpdatedTime")
            Prelude.<*> (x Data..?> "RouteId")
            Prelude.<*> (x Data..?> "ApplicationId")
            Prelude.<*> (x Data..?> "ServiceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRoute where
  hashWithSalt _salt DeleteRoute' {..} =
    _salt `Prelude.hashWithSalt` applicationIdentifier
      `Prelude.hashWithSalt` environmentIdentifier
      `Prelude.hashWithSalt` routeIdentifier

instance Prelude.NFData DeleteRoute where
  rnf DeleteRoute' {..} =
    Prelude.rnf applicationIdentifier
      `Prelude.seq` Prelude.rnf environmentIdentifier
      `Prelude.seq` Prelude.rnf routeIdentifier

instance Data.ToHeaders DeleteRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRoute where
  toPath DeleteRoute' {..} =
    Prelude.mconcat
      [ "/environments/",
        Data.toBS environmentIdentifier,
        "/applications/",
        Data.toBS applicationIdentifier,
        "/routes/",
        Data.toBS routeIdentifier
      ]

instance Data.ToQuery DeleteRoute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  { -- | The Amazon Resource Name (ARN) of the route.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the route.
    state :: Prelude.Maybe RouteState,
    -- | A timestamp that indicates when the route was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the route to delete.
    routeId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application that the route belongs to.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service that the route belongs to.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'arn', 'deleteRouteResponse_arn' - The Amazon Resource Name (ARN) of the route.
--
-- 'state', 'deleteRouteResponse_state' - The current state of the route.
--
-- 'lastUpdatedTime', 'deleteRouteResponse_lastUpdatedTime' - A timestamp that indicates when the route was last updated.
--
-- 'routeId', 'deleteRouteResponse_routeId' - The ID of the route to delete.
--
-- 'applicationId', 'deleteRouteResponse_applicationId' - The ID of the application that the route belongs to.
--
-- 'serviceId', 'deleteRouteResponse_serviceId' - The ID of the service that the route belongs to.
--
-- 'httpStatus', 'deleteRouteResponse_httpStatus' - The response's http status code.
newDeleteRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRouteResponse
newDeleteRouteResponse pHttpStatus_ =
  DeleteRouteResponse'
    { arn = Prelude.Nothing,
      state = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      routeId = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the route.
deleteRouteResponse_arn :: Lens.Lens' DeleteRouteResponse (Prelude.Maybe Prelude.Text)
deleteRouteResponse_arn = Lens.lens (\DeleteRouteResponse' {arn} -> arn) (\s@DeleteRouteResponse' {} a -> s {arn = a} :: DeleteRouteResponse)

-- | The current state of the route.
deleteRouteResponse_state :: Lens.Lens' DeleteRouteResponse (Prelude.Maybe RouteState)
deleteRouteResponse_state = Lens.lens (\DeleteRouteResponse' {state} -> state) (\s@DeleteRouteResponse' {} a -> s {state = a} :: DeleteRouteResponse)

-- | A timestamp that indicates when the route was last updated.
deleteRouteResponse_lastUpdatedTime :: Lens.Lens' DeleteRouteResponse (Prelude.Maybe Prelude.UTCTime)
deleteRouteResponse_lastUpdatedTime = Lens.lens (\DeleteRouteResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@DeleteRouteResponse' {} a -> s {lastUpdatedTime = a} :: DeleteRouteResponse) Prelude.. Lens.mapping Data._Time

-- | The ID of the route to delete.
deleteRouteResponse_routeId :: Lens.Lens' DeleteRouteResponse (Prelude.Maybe Prelude.Text)
deleteRouteResponse_routeId = Lens.lens (\DeleteRouteResponse' {routeId} -> routeId) (\s@DeleteRouteResponse' {} a -> s {routeId = a} :: DeleteRouteResponse)

-- | The ID of the application that the route belongs to.
deleteRouteResponse_applicationId :: Lens.Lens' DeleteRouteResponse (Prelude.Maybe Prelude.Text)
deleteRouteResponse_applicationId = Lens.lens (\DeleteRouteResponse' {applicationId} -> applicationId) (\s@DeleteRouteResponse' {} a -> s {applicationId = a} :: DeleteRouteResponse)

-- | The ID of the service that the route belongs to.
deleteRouteResponse_serviceId :: Lens.Lens' DeleteRouteResponse (Prelude.Maybe Prelude.Text)
deleteRouteResponse_serviceId = Lens.lens (\DeleteRouteResponse' {serviceId} -> serviceId) (\s@DeleteRouteResponse' {} a -> s {serviceId = a} :: DeleteRouteResponse)

-- | The response's http status code.
deleteRouteResponse_httpStatus :: Lens.Lens' DeleteRouteResponse Prelude.Int
deleteRouteResponse_httpStatus = Lens.lens (\DeleteRouteResponse' {httpStatus} -> httpStatus) (\s@DeleteRouteResponse' {} a -> s {httpStatus = a} :: DeleteRouteResponse)

instance Prelude.NFData DeleteRouteResponse where
  rnf DeleteRouteResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf routeId
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf httpStatus
