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
-- Module      : Amazonka.AppMesh.Types.GrpcRouteMatch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcRouteMatch where

import Amazonka.AppMesh.Types.GrpcRouteMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the criteria for determining a request match.
--
-- /See:/ 'newGrpcRouteMatch' smart constructor.
data GrpcRouteMatch = GrpcRouteMatch'
  { -- | The port number to match on.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The method name to match from the request. If you specify a name, you
    -- must also specify a @serviceName@.
    methodName :: Prelude.Maybe Prelude.Text,
    -- | An object that represents the data to match from the request.
    metadata :: Prelude.Maybe (Prelude.NonEmpty GrpcRouteMetadata),
    -- | The fully qualified domain name for the service to match from the
    -- request.
    serviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcRouteMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'grpcRouteMatch_port' - The port number to match on.
--
-- 'methodName', 'grpcRouteMatch_methodName' - The method name to match from the request. If you specify a name, you
-- must also specify a @serviceName@.
--
-- 'metadata', 'grpcRouteMatch_metadata' - An object that represents the data to match from the request.
--
-- 'serviceName', 'grpcRouteMatch_serviceName' - The fully qualified domain name for the service to match from the
-- request.
newGrpcRouteMatch ::
  GrpcRouteMatch
newGrpcRouteMatch =
  GrpcRouteMatch'
    { port = Prelude.Nothing,
      methodName = Prelude.Nothing,
      metadata = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | The port number to match on.
grpcRouteMatch_port :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe Prelude.Natural)
grpcRouteMatch_port = Lens.lens (\GrpcRouteMatch' {port} -> port) (\s@GrpcRouteMatch' {} a -> s {port = a} :: GrpcRouteMatch)

-- | The method name to match from the request. If you specify a name, you
-- must also specify a @serviceName@.
grpcRouteMatch_methodName :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe Prelude.Text)
grpcRouteMatch_methodName = Lens.lens (\GrpcRouteMatch' {methodName} -> methodName) (\s@GrpcRouteMatch' {} a -> s {methodName = a} :: GrpcRouteMatch)

-- | An object that represents the data to match from the request.
grpcRouteMatch_metadata :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe (Prelude.NonEmpty GrpcRouteMetadata))
grpcRouteMatch_metadata = Lens.lens (\GrpcRouteMatch' {metadata} -> metadata) (\s@GrpcRouteMatch' {} a -> s {metadata = a} :: GrpcRouteMatch) Prelude.. Lens.mapping Lens.coerced

-- | The fully qualified domain name for the service to match from the
-- request.
grpcRouteMatch_serviceName :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe Prelude.Text)
grpcRouteMatch_serviceName = Lens.lens (\GrpcRouteMatch' {serviceName} -> serviceName) (\s@GrpcRouteMatch' {} a -> s {serviceName = a} :: GrpcRouteMatch)

instance Core.FromJSON GrpcRouteMatch where
  parseJSON =
    Core.withObject
      "GrpcRouteMatch"
      ( \x ->
          GrpcRouteMatch'
            Prelude.<$> (x Core..:? "port")
            Prelude.<*> (x Core..:? "methodName")
            Prelude.<*> (x Core..:? "metadata")
            Prelude.<*> (x Core..:? "serviceName")
      )

instance Prelude.Hashable GrpcRouteMatch where
  hashWithSalt _salt GrpcRouteMatch' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` methodName
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData GrpcRouteMatch where
  rnf GrpcRouteMatch' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf methodName
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf serviceName

instance Core.ToJSON GrpcRouteMatch where
  toJSON GrpcRouteMatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("port" Core..=) Prelude.<$> port,
            ("methodName" Core..=) Prelude.<$> methodName,
            ("metadata" Core..=) Prelude.<$> metadata,
            ("serviceName" Core..=) Prelude.<$> serviceName
          ]
      )
