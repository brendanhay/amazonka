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
-- Module      : Amazonka.AppMesh.Types.GrpcRouteMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcRouteMetadata where

import Amazonka.AppMesh.Types.GrpcRouteMetadataMatchMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the match metadata for the route.
--
-- /See:/ 'newGrpcRouteMetadata' smart constructor.
data GrpcRouteMetadata = GrpcRouteMetadata'
  { -- | Specify @True@ to match anything except the match criteria. The default
    -- value is @False@.
    invert :: Prelude.Maybe Prelude.Bool,
    -- | An object that represents the data to match from the request.
    match :: Prelude.Maybe GrpcRouteMetadataMatchMethod,
    -- | The name of the route.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcRouteMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invert', 'grpcRouteMetadata_invert' - Specify @True@ to match anything except the match criteria. The default
-- value is @False@.
--
-- 'match', 'grpcRouteMetadata_match' - An object that represents the data to match from the request.
--
-- 'name', 'grpcRouteMetadata_name' - The name of the route.
newGrpcRouteMetadata ::
  -- | 'name'
  Prelude.Text ->
  GrpcRouteMetadata
newGrpcRouteMetadata pName_ =
  GrpcRouteMetadata'
    { invert = Prelude.Nothing,
      match = Prelude.Nothing,
      name = pName_
    }

-- | Specify @True@ to match anything except the match criteria. The default
-- value is @False@.
grpcRouteMetadata_invert :: Lens.Lens' GrpcRouteMetadata (Prelude.Maybe Prelude.Bool)
grpcRouteMetadata_invert = Lens.lens (\GrpcRouteMetadata' {invert} -> invert) (\s@GrpcRouteMetadata' {} a -> s {invert = a} :: GrpcRouteMetadata)

-- | An object that represents the data to match from the request.
grpcRouteMetadata_match :: Lens.Lens' GrpcRouteMetadata (Prelude.Maybe GrpcRouteMetadataMatchMethod)
grpcRouteMetadata_match = Lens.lens (\GrpcRouteMetadata' {match} -> match) (\s@GrpcRouteMetadata' {} a -> s {match = a} :: GrpcRouteMetadata)

-- | The name of the route.
grpcRouteMetadata_name :: Lens.Lens' GrpcRouteMetadata Prelude.Text
grpcRouteMetadata_name = Lens.lens (\GrpcRouteMetadata' {name} -> name) (\s@GrpcRouteMetadata' {} a -> s {name = a} :: GrpcRouteMetadata)

instance Data.FromJSON GrpcRouteMetadata where
  parseJSON =
    Data.withObject
      "GrpcRouteMetadata"
      ( \x ->
          GrpcRouteMetadata'
            Prelude.<$> (x Data..:? "invert")
            Prelude.<*> (x Data..:? "match")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable GrpcRouteMetadata where
  hashWithSalt _salt GrpcRouteMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` invert
      `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` name

instance Prelude.NFData GrpcRouteMetadata where
  rnf GrpcRouteMetadata' {..} =
    Prelude.rnf invert `Prelude.seq`
      Prelude.rnf match `Prelude.seq`
        Prelude.rnf name

instance Data.ToJSON GrpcRouteMetadata where
  toJSON GrpcRouteMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("invert" Data..=) Prelude.<$> invert,
            ("match" Data..=) Prelude.<$> match,
            Prelude.Just ("name" Data..= name)
          ]
      )
