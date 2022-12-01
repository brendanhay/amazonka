{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppMesh.Types.RouteSpec
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.RouteSpec where

import Amazonka.AppMesh.Types.GrpcRoute
import Amazonka.AppMesh.Types.HttpRoute
import Amazonka.AppMesh.Types.TcpRoute
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a route specification. Specify one route type.
--
-- /See:/ 'newRouteSpec' smart constructor.
data RouteSpec = RouteSpec'
  { -- | An object that represents the specification of an HTTP route.
    httpRoute :: Prelude.Maybe HttpRoute,
    -- | An object that represents the specification of a TCP route.
    tcpRoute :: Prelude.Maybe TcpRoute,
    -- | An object that represents the specification of an HTTP\/2 route.
    http2Route :: Prelude.Maybe HttpRoute,
    -- | An object that represents the specification of a gRPC route.
    grpcRoute :: Prelude.Maybe GrpcRoute,
    -- | The priority for the route. Routes are matched based on the specified
    -- value, where 0 is the highest priority.
    priority :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpRoute', 'routeSpec_httpRoute' - An object that represents the specification of an HTTP route.
--
-- 'tcpRoute', 'routeSpec_tcpRoute' - An object that represents the specification of a TCP route.
--
-- 'http2Route', 'routeSpec_http2Route' - An object that represents the specification of an HTTP\/2 route.
--
-- 'grpcRoute', 'routeSpec_grpcRoute' - An object that represents the specification of a gRPC route.
--
-- 'priority', 'routeSpec_priority' - The priority for the route. Routes are matched based on the specified
-- value, where 0 is the highest priority.
newRouteSpec ::
  RouteSpec
newRouteSpec =
  RouteSpec'
    { httpRoute = Prelude.Nothing,
      tcpRoute = Prelude.Nothing,
      http2Route = Prelude.Nothing,
      grpcRoute = Prelude.Nothing,
      priority = Prelude.Nothing
    }

-- | An object that represents the specification of an HTTP route.
routeSpec_httpRoute :: Lens.Lens' RouteSpec (Prelude.Maybe HttpRoute)
routeSpec_httpRoute = Lens.lens (\RouteSpec' {httpRoute} -> httpRoute) (\s@RouteSpec' {} a -> s {httpRoute = a} :: RouteSpec)

-- | An object that represents the specification of a TCP route.
routeSpec_tcpRoute :: Lens.Lens' RouteSpec (Prelude.Maybe TcpRoute)
routeSpec_tcpRoute = Lens.lens (\RouteSpec' {tcpRoute} -> tcpRoute) (\s@RouteSpec' {} a -> s {tcpRoute = a} :: RouteSpec)

-- | An object that represents the specification of an HTTP\/2 route.
routeSpec_http2Route :: Lens.Lens' RouteSpec (Prelude.Maybe HttpRoute)
routeSpec_http2Route = Lens.lens (\RouteSpec' {http2Route} -> http2Route) (\s@RouteSpec' {} a -> s {http2Route = a} :: RouteSpec)

-- | An object that represents the specification of a gRPC route.
routeSpec_grpcRoute :: Lens.Lens' RouteSpec (Prelude.Maybe GrpcRoute)
routeSpec_grpcRoute = Lens.lens (\RouteSpec' {grpcRoute} -> grpcRoute) (\s@RouteSpec' {} a -> s {grpcRoute = a} :: RouteSpec)

-- | The priority for the route. Routes are matched based on the specified
-- value, where 0 is the highest priority.
routeSpec_priority :: Lens.Lens' RouteSpec (Prelude.Maybe Prelude.Natural)
routeSpec_priority = Lens.lens (\RouteSpec' {priority} -> priority) (\s@RouteSpec' {} a -> s {priority = a} :: RouteSpec)

instance Core.FromJSON RouteSpec where
  parseJSON =
    Core.withObject
      "RouteSpec"
      ( \x ->
          RouteSpec'
            Prelude.<$> (x Core..:? "httpRoute")
            Prelude.<*> (x Core..:? "tcpRoute")
            Prelude.<*> (x Core..:? "http2Route")
            Prelude.<*> (x Core..:? "grpcRoute")
            Prelude.<*> (x Core..:? "priority")
      )

instance Prelude.Hashable RouteSpec where
  hashWithSalt _salt RouteSpec' {..} =
    _salt `Prelude.hashWithSalt` httpRoute
      `Prelude.hashWithSalt` tcpRoute
      `Prelude.hashWithSalt` http2Route
      `Prelude.hashWithSalt` grpcRoute
      `Prelude.hashWithSalt` priority

instance Prelude.NFData RouteSpec where
  rnf RouteSpec' {..} =
    Prelude.rnf httpRoute
      `Prelude.seq` Prelude.rnf tcpRoute
      `Prelude.seq` Prelude.rnf http2Route
      `Prelude.seq` Prelude.rnf grpcRoute
      `Prelude.seq` Prelude.rnf priority

instance Core.ToJSON RouteSpec where
  toJSON RouteSpec' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("httpRoute" Core..=) Prelude.<$> httpRoute,
            ("tcpRoute" Core..=) Prelude.<$> tcpRoute,
            ("http2Route" Core..=) Prelude.<$> http2Route,
            ("grpcRoute" Core..=) Prelude.<$> grpcRoute,
            ("priority" Core..=) Prelude.<$> priority
          ]
      )
