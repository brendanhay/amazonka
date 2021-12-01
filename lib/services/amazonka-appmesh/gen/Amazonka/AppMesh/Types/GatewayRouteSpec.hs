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
-- Module      : Amazonka.AppMesh.Types.GatewayRouteSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GatewayRouteSpec where

import Amazonka.AppMesh.Types.GrpcGatewayRoute
import Amazonka.AppMesh.Types.HttpGatewayRoute
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a gateway route specification. Specify one
-- gateway route type.
--
-- /See:/ 'newGatewayRouteSpec' smart constructor.
data GatewayRouteSpec = GatewayRouteSpec'
  { -- | The ordering of the gateway routes spec.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | An object that represents the specification of an HTTP\/2 gateway route.
    http2Route :: Prelude.Maybe HttpGatewayRoute,
    -- | An object that represents the specification of a gRPC gateway route.
    grpcRoute :: Prelude.Maybe GrpcGatewayRoute,
    -- | An object that represents the specification of an HTTP gateway route.
    httpRoute :: Prelude.Maybe HttpGatewayRoute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayRouteSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'gatewayRouteSpec_priority' - The ordering of the gateway routes spec.
--
-- 'http2Route', 'gatewayRouteSpec_http2Route' - An object that represents the specification of an HTTP\/2 gateway route.
--
-- 'grpcRoute', 'gatewayRouteSpec_grpcRoute' - An object that represents the specification of a gRPC gateway route.
--
-- 'httpRoute', 'gatewayRouteSpec_httpRoute' - An object that represents the specification of an HTTP gateway route.
newGatewayRouteSpec ::
  GatewayRouteSpec
newGatewayRouteSpec =
  GatewayRouteSpec'
    { priority = Prelude.Nothing,
      http2Route = Prelude.Nothing,
      grpcRoute = Prelude.Nothing,
      httpRoute = Prelude.Nothing
    }

-- | The ordering of the gateway routes spec.
gatewayRouteSpec_priority :: Lens.Lens' GatewayRouteSpec (Prelude.Maybe Prelude.Natural)
gatewayRouteSpec_priority = Lens.lens (\GatewayRouteSpec' {priority} -> priority) (\s@GatewayRouteSpec' {} a -> s {priority = a} :: GatewayRouteSpec)

-- | An object that represents the specification of an HTTP\/2 gateway route.
gatewayRouteSpec_http2Route :: Lens.Lens' GatewayRouteSpec (Prelude.Maybe HttpGatewayRoute)
gatewayRouteSpec_http2Route = Lens.lens (\GatewayRouteSpec' {http2Route} -> http2Route) (\s@GatewayRouteSpec' {} a -> s {http2Route = a} :: GatewayRouteSpec)

-- | An object that represents the specification of a gRPC gateway route.
gatewayRouteSpec_grpcRoute :: Lens.Lens' GatewayRouteSpec (Prelude.Maybe GrpcGatewayRoute)
gatewayRouteSpec_grpcRoute = Lens.lens (\GatewayRouteSpec' {grpcRoute} -> grpcRoute) (\s@GatewayRouteSpec' {} a -> s {grpcRoute = a} :: GatewayRouteSpec)

-- | An object that represents the specification of an HTTP gateway route.
gatewayRouteSpec_httpRoute :: Lens.Lens' GatewayRouteSpec (Prelude.Maybe HttpGatewayRoute)
gatewayRouteSpec_httpRoute = Lens.lens (\GatewayRouteSpec' {httpRoute} -> httpRoute) (\s@GatewayRouteSpec' {} a -> s {httpRoute = a} :: GatewayRouteSpec)

instance Core.FromJSON GatewayRouteSpec where
  parseJSON =
    Core.withObject
      "GatewayRouteSpec"
      ( \x ->
          GatewayRouteSpec'
            Prelude.<$> (x Core..:? "priority")
            Prelude.<*> (x Core..:? "http2Route")
            Prelude.<*> (x Core..:? "grpcRoute")
            Prelude.<*> (x Core..:? "httpRoute")
      )

instance Prelude.Hashable GatewayRouteSpec where
  hashWithSalt salt' GatewayRouteSpec' {..} =
    salt' `Prelude.hashWithSalt` httpRoute
      `Prelude.hashWithSalt` grpcRoute
      `Prelude.hashWithSalt` http2Route
      `Prelude.hashWithSalt` priority

instance Prelude.NFData GatewayRouteSpec where
  rnf GatewayRouteSpec' {..} =
    Prelude.rnf priority
      `Prelude.seq` Prelude.rnf httpRoute
      `Prelude.seq` Prelude.rnf grpcRoute
      `Prelude.seq` Prelude.rnf http2Route

instance Core.ToJSON GatewayRouteSpec where
  toJSON GatewayRouteSpec' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("priority" Core..=) Prelude.<$> priority,
            ("http2Route" Core..=) Prelude.<$> http2Route,
            ("grpcRoute" Core..=) Prelude.<$> grpcRoute,
            ("httpRoute" Core..=) Prelude.<$> httpRoute
          ]
      )
