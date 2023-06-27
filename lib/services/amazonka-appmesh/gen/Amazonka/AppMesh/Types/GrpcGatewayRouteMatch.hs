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
-- Module      : Amazonka.AppMesh.Types.GrpcGatewayRouteMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcGatewayRouteMatch where

import Amazonka.AppMesh.Types.GatewayRouteHostnameMatch
import Amazonka.AppMesh.Types.GrpcGatewayRouteMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the criteria for determining a request match.
--
-- /See:/ 'newGrpcGatewayRouteMatch' smart constructor.
data GrpcGatewayRouteMatch = GrpcGatewayRouteMatch'
  { -- | The gateway route host name to be matched on.
    hostname :: Prelude.Maybe GatewayRouteHostnameMatch,
    -- | The gateway route metadata to be matched on.
    metadata :: Prelude.Maybe (Prelude.NonEmpty GrpcGatewayRouteMetadata),
    -- | The port number to match from the request.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The fully qualified domain name for the service to match from the
    -- request.
    serviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcGatewayRouteMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'grpcGatewayRouteMatch_hostname' - The gateway route host name to be matched on.
--
-- 'metadata', 'grpcGatewayRouteMatch_metadata' - The gateway route metadata to be matched on.
--
-- 'port', 'grpcGatewayRouteMatch_port' - The port number to match from the request.
--
-- 'serviceName', 'grpcGatewayRouteMatch_serviceName' - The fully qualified domain name for the service to match from the
-- request.
newGrpcGatewayRouteMatch ::
  GrpcGatewayRouteMatch
newGrpcGatewayRouteMatch =
  GrpcGatewayRouteMatch'
    { hostname = Prelude.Nothing,
      metadata = Prelude.Nothing,
      port = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | The gateway route host name to be matched on.
grpcGatewayRouteMatch_hostname :: Lens.Lens' GrpcGatewayRouteMatch (Prelude.Maybe GatewayRouteHostnameMatch)
grpcGatewayRouteMatch_hostname = Lens.lens (\GrpcGatewayRouteMatch' {hostname} -> hostname) (\s@GrpcGatewayRouteMatch' {} a -> s {hostname = a} :: GrpcGatewayRouteMatch)

-- | The gateway route metadata to be matched on.
grpcGatewayRouteMatch_metadata :: Lens.Lens' GrpcGatewayRouteMatch (Prelude.Maybe (Prelude.NonEmpty GrpcGatewayRouteMetadata))
grpcGatewayRouteMatch_metadata = Lens.lens (\GrpcGatewayRouteMatch' {metadata} -> metadata) (\s@GrpcGatewayRouteMatch' {} a -> s {metadata = a} :: GrpcGatewayRouteMatch) Prelude.. Lens.mapping Lens.coerced

-- | The port number to match from the request.
grpcGatewayRouteMatch_port :: Lens.Lens' GrpcGatewayRouteMatch (Prelude.Maybe Prelude.Natural)
grpcGatewayRouteMatch_port = Lens.lens (\GrpcGatewayRouteMatch' {port} -> port) (\s@GrpcGatewayRouteMatch' {} a -> s {port = a} :: GrpcGatewayRouteMatch)

-- | The fully qualified domain name for the service to match from the
-- request.
grpcGatewayRouteMatch_serviceName :: Lens.Lens' GrpcGatewayRouteMatch (Prelude.Maybe Prelude.Text)
grpcGatewayRouteMatch_serviceName = Lens.lens (\GrpcGatewayRouteMatch' {serviceName} -> serviceName) (\s@GrpcGatewayRouteMatch' {} a -> s {serviceName = a} :: GrpcGatewayRouteMatch)

instance Data.FromJSON GrpcGatewayRouteMatch where
  parseJSON =
    Data.withObject
      "GrpcGatewayRouteMatch"
      ( \x ->
          GrpcGatewayRouteMatch'
            Prelude.<$> (x Data..:? "hostname")
            Prelude.<*> (x Data..:? "metadata")
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "serviceName")
      )

instance Prelude.Hashable GrpcGatewayRouteMatch where
  hashWithSalt _salt GrpcGatewayRouteMatch' {..} =
    _salt
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData GrpcGatewayRouteMatch where
  rnf GrpcGatewayRouteMatch' {..} =
    Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToJSON GrpcGatewayRouteMatch where
  toJSON GrpcGatewayRouteMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("hostname" Data..=) Prelude.<$> hostname,
            ("metadata" Data..=) Prelude.<$> metadata,
            ("port" Data..=) Prelude.<$> port,
            ("serviceName" Data..=) Prelude.<$> serviceName
          ]
      )
