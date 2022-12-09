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
-- Module      : Amazonka.MigrationHubReFactorSpaces.UpdateRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon Web Services Migration Hub Refactor Spaces route.
module Amazonka.MigrationHubReFactorSpaces.UpdateRoute
  ( -- * Creating a Request
    UpdateRoute (..),
    newUpdateRoute,

    -- * Request Lenses
    updateRoute_activationState,
    updateRoute_applicationIdentifier,
    updateRoute_environmentIdentifier,
    updateRoute_routeIdentifier,

    -- * Destructuring the Response
    UpdateRouteResponse (..),
    newUpdateRouteResponse,

    -- * Response Lenses
    updateRouteResponse_applicationId,
    updateRouteResponse_arn,
    updateRouteResponse_lastUpdatedTime,
    updateRouteResponse_routeId,
    updateRouteResponse_serviceId,
    updateRouteResponse_state,
    updateRouteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRoute' smart constructor.
data UpdateRoute = UpdateRoute'
  { -- | If set to @ACTIVE@, traffic is forwarded to this route’s service after
    -- the route is updated.
    activationState :: RouteActivationState,
    -- | The ID of the application within which the route is being updated.
    applicationIdentifier :: Prelude.Text,
    -- | The ID of the environment in which the route is being updated.
    environmentIdentifier :: Prelude.Text,
    -- | The unique identifier of the route to update.
    routeIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activationState', 'updateRoute_activationState' - If set to @ACTIVE@, traffic is forwarded to this route’s service after
-- the route is updated.
--
-- 'applicationIdentifier', 'updateRoute_applicationIdentifier' - The ID of the application within which the route is being updated.
--
-- 'environmentIdentifier', 'updateRoute_environmentIdentifier' - The ID of the environment in which the route is being updated.
--
-- 'routeIdentifier', 'updateRoute_routeIdentifier' - The unique identifier of the route to update.
newUpdateRoute ::
  -- | 'activationState'
  RouteActivationState ->
  -- | 'applicationIdentifier'
  Prelude.Text ->
  -- | 'environmentIdentifier'
  Prelude.Text ->
  -- | 'routeIdentifier'
  Prelude.Text ->
  UpdateRoute
newUpdateRoute
  pActivationState_
  pApplicationIdentifier_
  pEnvironmentIdentifier_
  pRouteIdentifier_ =
    UpdateRoute'
      { activationState = pActivationState_,
        applicationIdentifier = pApplicationIdentifier_,
        environmentIdentifier = pEnvironmentIdentifier_,
        routeIdentifier = pRouteIdentifier_
      }

-- | If set to @ACTIVE@, traffic is forwarded to this route’s service after
-- the route is updated.
updateRoute_activationState :: Lens.Lens' UpdateRoute RouteActivationState
updateRoute_activationState = Lens.lens (\UpdateRoute' {activationState} -> activationState) (\s@UpdateRoute' {} a -> s {activationState = a} :: UpdateRoute)

-- | The ID of the application within which the route is being updated.
updateRoute_applicationIdentifier :: Lens.Lens' UpdateRoute Prelude.Text
updateRoute_applicationIdentifier = Lens.lens (\UpdateRoute' {applicationIdentifier} -> applicationIdentifier) (\s@UpdateRoute' {} a -> s {applicationIdentifier = a} :: UpdateRoute)

-- | The ID of the environment in which the route is being updated.
updateRoute_environmentIdentifier :: Lens.Lens' UpdateRoute Prelude.Text
updateRoute_environmentIdentifier = Lens.lens (\UpdateRoute' {environmentIdentifier} -> environmentIdentifier) (\s@UpdateRoute' {} a -> s {environmentIdentifier = a} :: UpdateRoute)

-- | The unique identifier of the route to update.
updateRoute_routeIdentifier :: Lens.Lens' UpdateRoute Prelude.Text
updateRoute_routeIdentifier = Lens.lens (\UpdateRoute' {routeIdentifier} -> routeIdentifier) (\s@UpdateRoute' {} a -> s {routeIdentifier = a} :: UpdateRoute)

instance Core.AWSRequest UpdateRoute where
  type AWSResponse UpdateRoute = UpdateRouteResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRouteResponse'
            Prelude.<$> (x Data..?> "ApplicationId")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "LastUpdatedTime")
            Prelude.<*> (x Data..?> "RouteId")
            Prelude.<*> (x Data..?> "ServiceId")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoute where
  hashWithSalt _salt UpdateRoute' {..} =
    _salt `Prelude.hashWithSalt` activationState
      `Prelude.hashWithSalt` applicationIdentifier
      `Prelude.hashWithSalt` environmentIdentifier
      `Prelude.hashWithSalt` routeIdentifier

instance Prelude.NFData UpdateRoute where
  rnf UpdateRoute' {..} =
    Prelude.rnf activationState
      `Prelude.seq` Prelude.rnf applicationIdentifier
      `Prelude.seq` Prelude.rnf environmentIdentifier
      `Prelude.seq` Prelude.rnf routeIdentifier

instance Data.ToHeaders UpdateRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRoute where
  toJSON UpdateRoute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ActivationState" Data..= activationState)
          ]
      )

instance Data.ToPath UpdateRoute where
  toPath UpdateRoute' {..} =
    Prelude.mconcat
      [ "/environments/",
        Data.toBS environmentIdentifier,
        "/applications/",
        Data.toBS applicationIdentifier,
        "/routes/",
        Data.toBS routeIdentifier
      ]

instance Data.ToQuery UpdateRoute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRouteResponse' smart constructor.
data UpdateRouteResponse = UpdateRouteResponse'
  { -- | The ID of the application in which the route is being updated.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the route. The format for this ARN is
    -- @arn:aws:refactor-spaces:region:account-id:resource-type\/resource-id @.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the route was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier of the route.
    routeId :: Prelude.Maybe Prelude.Text,
    -- | The ID of service in which the route was created. Traffic that matches
    -- this route is forwarded to this service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the route.
    state :: Prelude.Maybe RouteState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateRouteResponse_applicationId' - The ID of the application in which the route is being updated.
--
-- 'arn', 'updateRouteResponse_arn' - The Amazon Resource Name (ARN) of the route. The format for this ARN is
-- @arn:aws:refactor-spaces:region:account-id:resource-type\/resource-id @.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'lastUpdatedTime', 'updateRouteResponse_lastUpdatedTime' - A timestamp that indicates when the route was last updated.
--
-- 'routeId', 'updateRouteResponse_routeId' - The unique identifier of the route.
--
-- 'serviceId', 'updateRouteResponse_serviceId' - The ID of service in which the route was created. Traffic that matches
-- this route is forwarded to this service.
--
-- 'state', 'updateRouteResponse_state' - The current state of the route.
--
-- 'httpStatus', 'updateRouteResponse_httpStatus' - The response's http status code.
newUpdateRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRouteResponse
newUpdateRouteResponse pHttpStatus_ =
  UpdateRouteResponse'
    { applicationId =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      routeId = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the application in which the route is being updated.
updateRouteResponse_applicationId :: Lens.Lens' UpdateRouteResponse (Prelude.Maybe Prelude.Text)
updateRouteResponse_applicationId = Lens.lens (\UpdateRouteResponse' {applicationId} -> applicationId) (\s@UpdateRouteResponse' {} a -> s {applicationId = a} :: UpdateRouteResponse)

-- | The Amazon Resource Name (ARN) of the route. The format for this ARN is
-- @arn:aws:refactor-spaces:region:account-id:resource-type\/resource-id @.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
updateRouteResponse_arn :: Lens.Lens' UpdateRouteResponse (Prelude.Maybe Prelude.Text)
updateRouteResponse_arn = Lens.lens (\UpdateRouteResponse' {arn} -> arn) (\s@UpdateRouteResponse' {} a -> s {arn = a} :: UpdateRouteResponse)

-- | A timestamp that indicates when the route was last updated.
updateRouteResponse_lastUpdatedTime :: Lens.Lens' UpdateRouteResponse (Prelude.Maybe Prelude.UTCTime)
updateRouteResponse_lastUpdatedTime = Lens.lens (\UpdateRouteResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@UpdateRouteResponse' {} a -> s {lastUpdatedTime = a} :: UpdateRouteResponse) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the route.
updateRouteResponse_routeId :: Lens.Lens' UpdateRouteResponse (Prelude.Maybe Prelude.Text)
updateRouteResponse_routeId = Lens.lens (\UpdateRouteResponse' {routeId} -> routeId) (\s@UpdateRouteResponse' {} a -> s {routeId = a} :: UpdateRouteResponse)

-- | The ID of service in which the route was created. Traffic that matches
-- this route is forwarded to this service.
updateRouteResponse_serviceId :: Lens.Lens' UpdateRouteResponse (Prelude.Maybe Prelude.Text)
updateRouteResponse_serviceId = Lens.lens (\UpdateRouteResponse' {serviceId} -> serviceId) (\s@UpdateRouteResponse' {} a -> s {serviceId = a} :: UpdateRouteResponse)

-- | The current state of the route.
updateRouteResponse_state :: Lens.Lens' UpdateRouteResponse (Prelude.Maybe RouteState)
updateRouteResponse_state = Lens.lens (\UpdateRouteResponse' {state} -> state) (\s@UpdateRouteResponse' {} a -> s {state = a} :: UpdateRouteResponse)

-- | The response's http status code.
updateRouteResponse_httpStatus :: Lens.Lens' UpdateRouteResponse Prelude.Int
updateRouteResponse_httpStatus = Lens.lens (\UpdateRouteResponse' {httpStatus} -> httpStatus) (\s@UpdateRouteResponse' {} a -> s {httpStatus = a} :: UpdateRouteResponse)

instance Prelude.NFData UpdateRouteResponse where
  rnf UpdateRouteResponse' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf routeId
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
