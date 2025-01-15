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
-- Module      : Amazonka.AppMesh.Types.GrpcGatewayRouteMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcGatewayRouteMetadata where

import Amazonka.AppMesh.Types.GrpcMetadataMatchMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the metadata of the gateway route.
--
-- /See:/ 'newGrpcGatewayRouteMetadata' smart constructor.
data GrpcGatewayRouteMetadata = GrpcGatewayRouteMetadata'
  { -- | Specify @True@ to match anything except the match criteria. The default
    -- value is @False@.
    invert :: Prelude.Maybe Prelude.Bool,
    -- | The criteria for determining a metadata match.
    match :: Prelude.Maybe GrpcMetadataMatchMethod,
    -- | A name for the gateway route metadata.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcGatewayRouteMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invert', 'grpcGatewayRouteMetadata_invert' - Specify @True@ to match anything except the match criteria. The default
-- value is @False@.
--
-- 'match', 'grpcGatewayRouteMetadata_match' - The criteria for determining a metadata match.
--
-- 'name', 'grpcGatewayRouteMetadata_name' - A name for the gateway route metadata.
newGrpcGatewayRouteMetadata ::
  -- | 'name'
  Prelude.Text ->
  GrpcGatewayRouteMetadata
newGrpcGatewayRouteMetadata pName_ =
  GrpcGatewayRouteMetadata'
    { invert = Prelude.Nothing,
      match = Prelude.Nothing,
      name = pName_
    }

-- | Specify @True@ to match anything except the match criteria. The default
-- value is @False@.
grpcGatewayRouteMetadata_invert :: Lens.Lens' GrpcGatewayRouteMetadata (Prelude.Maybe Prelude.Bool)
grpcGatewayRouteMetadata_invert = Lens.lens (\GrpcGatewayRouteMetadata' {invert} -> invert) (\s@GrpcGatewayRouteMetadata' {} a -> s {invert = a} :: GrpcGatewayRouteMetadata)

-- | The criteria for determining a metadata match.
grpcGatewayRouteMetadata_match :: Lens.Lens' GrpcGatewayRouteMetadata (Prelude.Maybe GrpcMetadataMatchMethod)
grpcGatewayRouteMetadata_match = Lens.lens (\GrpcGatewayRouteMetadata' {match} -> match) (\s@GrpcGatewayRouteMetadata' {} a -> s {match = a} :: GrpcGatewayRouteMetadata)

-- | A name for the gateway route metadata.
grpcGatewayRouteMetadata_name :: Lens.Lens' GrpcGatewayRouteMetadata Prelude.Text
grpcGatewayRouteMetadata_name = Lens.lens (\GrpcGatewayRouteMetadata' {name} -> name) (\s@GrpcGatewayRouteMetadata' {} a -> s {name = a} :: GrpcGatewayRouteMetadata)

instance Data.FromJSON GrpcGatewayRouteMetadata where
  parseJSON =
    Data.withObject
      "GrpcGatewayRouteMetadata"
      ( \x ->
          GrpcGatewayRouteMetadata'
            Prelude.<$> (x Data..:? "invert")
            Prelude.<*> (x Data..:? "match")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable GrpcGatewayRouteMetadata where
  hashWithSalt _salt GrpcGatewayRouteMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` invert
      `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` name

instance Prelude.NFData GrpcGatewayRouteMetadata where
  rnf GrpcGatewayRouteMetadata' {..} =
    Prelude.rnf invert `Prelude.seq`
      Prelude.rnf match `Prelude.seq`
        Prelude.rnf name

instance Data.ToJSON GrpcGatewayRouteMetadata where
  toJSON GrpcGatewayRouteMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("invert" Data..=) Prelude.<$> invert,
            ("match" Data..=) Prelude.<$> match,
            Prelude.Just ("name" Data..= name)
          ]
      )
