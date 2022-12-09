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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the criteria for determining a request match.
--
-- /See:/ 'newGrpcRouteMatch' smart constructor.
data GrpcRouteMatch = GrpcRouteMatch'
  { -- | An object that represents the data to match from the request.
    metadata :: Prelude.Maybe (Prelude.NonEmpty GrpcRouteMetadata),
    -- | The method name to match from the request. If you specify a name, you
    -- must also specify a @serviceName@.
    methodName :: Prelude.Maybe Prelude.Text,
    -- | The port number to match on.
    port :: Prelude.Maybe Prelude.Natural,
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
-- 'metadata', 'grpcRouteMatch_metadata' - An object that represents the data to match from the request.
--
-- 'methodName', 'grpcRouteMatch_methodName' - The method name to match from the request. If you specify a name, you
-- must also specify a @serviceName@.
--
-- 'port', 'grpcRouteMatch_port' - The port number to match on.
--
-- 'serviceName', 'grpcRouteMatch_serviceName' - The fully qualified domain name for the service to match from the
-- request.
newGrpcRouteMatch ::
  GrpcRouteMatch
newGrpcRouteMatch =
  GrpcRouteMatch'
    { metadata = Prelude.Nothing,
      methodName = Prelude.Nothing,
      port = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | An object that represents the data to match from the request.
grpcRouteMatch_metadata :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe (Prelude.NonEmpty GrpcRouteMetadata))
grpcRouteMatch_metadata = Lens.lens (\GrpcRouteMatch' {metadata} -> metadata) (\s@GrpcRouteMatch' {} a -> s {metadata = a} :: GrpcRouteMatch) Prelude.. Lens.mapping Lens.coerced

-- | The method name to match from the request. If you specify a name, you
-- must also specify a @serviceName@.
grpcRouteMatch_methodName :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe Prelude.Text)
grpcRouteMatch_methodName = Lens.lens (\GrpcRouteMatch' {methodName} -> methodName) (\s@GrpcRouteMatch' {} a -> s {methodName = a} :: GrpcRouteMatch)

-- | The port number to match on.
grpcRouteMatch_port :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe Prelude.Natural)
grpcRouteMatch_port = Lens.lens (\GrpcRouteMatch' {port} -> port) (\s@GrpcRouteMatch' {} a -> s {port = a} :: GrpcRouteMatch)

-- | The fully qualified domain name for the service to match from the
-- request.
grpcRouteMatch_serviceName :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe Prelude.Text)
grpcRouteMatch_serviceName = Lens.lens (\GrpcRouteMatch' {serviceName} -> serviceName) (\s@GrpcRouteMatch' {} a -> s {serviceName = a} :: GrpcRouteMatch)

instance Data.FromJSON GrpcRouteMatch where
  parseJSON =
    Data.withObject
      "GrpcRouteMatch"
      ( \x ->
          GrpcRouteMatch'
            Prelude.<$> (x Data..:? "metadata")
            Prelude.<*> (x Data..:? "methodName")
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "serviceName")
      )

instance Prelude.Hashable GrpcRouteMatch where
  hashWithSalt _salt GrpcRouteMatch' {..} =
    _salt `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` methodName
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData GrpcRouteMatch where
  rnf GrpcRouteMatch' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf methodName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToJSON GrpcRouteMatch where
  toJSON GrpcRouteMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("metadata" Data..=) Prelude.<$> metadata,
            ("methodName" Data..=) Prelude.<$> methodName,
            ("port" Data..=) Prelude.<$> port,
            ("serviceName" Data..=) Prelude.<$> serviceName
          ]
      )
