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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcRouteMatch where

import Amazonka.AppMesh.Types.GrpcRouteMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the criteria for determining a request match.
--
-- /See:/ 'newGrpcRouteMatch' smart constructor.
data GrpcRouteMatch = GrpcRouteMatch'
  { -- | The method name to match from the request. If you specify a name, you
    -- must also specify a @serviceName@.
    methodName :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified domain name for the service to match from the
    -- request.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | An object that represents the data to match from the request.
    metadata :: Prelude.Maybe (Prelude.NonEmpty GrpcRouteMetadata)
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
-- 'methodName', 'grpcRouteMatch_methodName' - The method name to match from the request. If you specify a name, you
-- must also specify a @serviceName@.
--
-- 'serviceName', 'grpcRouteMatch_serviceName' - The fully qualified domain name for the service to match from the
-- request.
--
-- 'metadata', 'grpcRouteMatch_metadata' - An object that represents the data to match from the request.
newGrpcRouteMatch ::
  GrpcRouteMatch
newGrpcRouteMatch =
  GrpcRouteMatch'
    { methodName = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      metadata = Prelude.Nothing
    }

-- | The method name to match from the request. If you specify a name, you
-- must also specify a @serviceName@.
grpcRouteMatch_methodName :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe Prelude.Text)
grpcRouteMatch_methodName = Lens.lens (\GrpcRouteMatch' {methodName} -> methodName) (\s@GrpcRouteMatch' {} a -> s {methodName = a} :: GrpcRouteMatch)

-- | The fully qualified domain name for the service to match from the
-- request.
grpcRouteMatch_serviceName :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe Prelude.Text)
grpcRouteMatch_serviceName = Lens.lens (\GrpcRouteMatch' {serviceName} -> serviceName) (\s@GrpcRouteMatch' {} a -> s {serviceName = a} :: GrpcRouteMatch)

-- | An object that represents the data to match from the request.
grpcRouteMatch_metadata :: Lens.Lens' GrpcRouteMatch (Prelude.Maybe (Prelude.NonEmpty GrpcRouteMetadata))
grpcRouteMatch_metadata = Lens.lens (\GrpcRouteMatch' {metadata} -> metadata) (\s@GrpcRouteMatch' {} a -> s {metadata = a} :: GrpcRouteMatch) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON GrpcRouteMatch where
  parseJSON =
    Core.withObject
      "GrpcRouteMatch"
      ( \x ->
          GrpcRouteMatch'
            Prelude.<$> (x Core..:? "methodName")
            Prelude.<*> (x Core..:? "serviceName")
            Prelude.<*> (x Core..:? "metadata")
      )

instance Prelude.Hashable GrpcRouteMatch

instance Prelude.NFData GrpcRouteMatch

instance Core.ToJSON GrpcRouteMatch where
  toJSON GrpcRouteMatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("methodName" Core..=) Prelude.<$> methodName,
            ("serviceName" Core..=) Prelude.<$> serviceName,
            ("metadata" Core..=) Prelude.<$> metadata
          ]
      )
