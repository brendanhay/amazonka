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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcGatewayRouteMatch where

import Amazonka.AppMesh.Types.GatewayRouteHostnameMatch
import Amazonka.AppMesh.Types.GrpcGatewayRouteMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the criteria for determining a request match.
--
-- /See:/ 'newGrpcGatewayRouteMatch' smart constructor.
data GrpcGatewayRouteMatch = GrpcGatewayRouteMatch'
  { -- | The port number to match from the request.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The gateway route metadata to be matched on.
    metadata :: Prelude.Maybe (Prelude.NonEmpty GrpcGatewayRouteMetadata),
    -- | The gateway route host name to be matched on.
    hostname :: Prelude.Maybe GatewayRouteHostnameMatch,
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
-- 'port', 'grpcGatewayRouteMatch_port' - The port number to match from the request.
--
-- 'metadata', 'grpcGatewayRouteMatch_metadata' - The gateway route metadata to be matched on.
--
-- 'hostname', 'grpcGatewayRouteMatch_hostname' - The gateway route host name to be matched on.
--
-- 'serviceName', 'grpcGatewayRouteMatch_serviceName' - The fully qualified domain name for the service to match from the
-- request.
newGrpcGatewayRouteMatch ::
  GrpcGatewayRouteMatch
newGrpcGatewayRouteMatch =
  GrpcGatewayRouteMatch'
    { port = Prelude.Nothing,
      metadata = Prelude.Nothing,
      hostname = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | The port number to match from the request.
grpcGatewayRouteMatch_port :: Lens.Lens' GrpcGatewayRouteMatch (Prelude.Maybe Prelude.Natural)
grpcGatewayRouteMatch_port = Lens.lens (\GrpcGatewayRouteMatch' {port} -> port) (\s@GrpcGatewayRouteMatch' {} a -> s {port = a} :: GrpcGatewayRouteMatch)

-- | The gateway route metadata to be matched on.
grpcGatewayRouteMatch_metadata :: Lens.Lens' GrpcGatewayRouteMatch (Prelude.Maybe (Prelude.NonEmpty GrpcGatewayRouteMetadata))
grpcGatewayRouteMatch_metadata = Lens.lens (\GrpcGatewayRouteMatch' {metadata} -> metadata) (\s@GrpcGatewayRouteMatch' {} a -> s {metadata = a} :: GrpcGatewayRouteMatch) Prelude.. Lens.mapping Lens.coerced

-- | The gateway route host name to be matched on.
grpcGatewayRouteMatch_hostname :: Lens.Lens' GrpcGatewayRouteMatch (Prelude.Maybe GatewayRouteHostnameMatch)
grpcGatewayRouteMatch_hostname = Lens.lens (\GrpcGatewayRouteMatch' {hostname} -> hostname) (\s@GrpcGatewayRouteMatch' {} a -> s {hostname = a} :: GrpcGatewayRouteMatch)

-- | The fully qualified domain name for the service to match from the
-- request.
grpcGatewayRouteMatch_serviceName :: Lens.Lens' GrpcGatewayRouteMatch (Prelude.Maybe Prelude.Text)
grpcGatewayRouteMatch_serviceName = Lens.lens (\GrpcGatewayRouteMatch' {serviceName} -> serviceName) (\s@GrpcGatewayRouteMatch' {} a -> s {serviceName = a} :: GrpcGatewayRouteMatch)

instance Core.FromJSON GrpcGatewayRouteMatch where
  parseJSON =
    Core.withObject
      "GrpcGatewayRouteMatch"
      ( \x ->
          GrpcGatewayRouteMatch'
            Prelude.<$> (x Core..:? "port")
            Prelude.<*> (x Core..:? "metadata")
            Prelude.<*> (x Core..:? "hostname")
            Prelude.<*> (x Core..:? "serviceName")
      )

instance Prelude.Hashable GrpcGatewayRouteMatch where
  hashWithSalt _salt GrpcGatewayRouteMatch' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData GrpcGatewayRouteMatch where
  rnf GrpcGatewayRouteMatch' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf serviceName

instance Core.ToJSON GrpcGatewayRouteMatch where
  toJSON GrpcGatewayRouteMatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("port" Core..=) Prelude.<$> port,
            ("metadata" Core..=) Prelude.<$> metadata,
            ("hostname" Core..=) Prelude.<$> hostname,
            ("serviceName" Core..=) Prelude.<$> serviceName
          ]
      )
