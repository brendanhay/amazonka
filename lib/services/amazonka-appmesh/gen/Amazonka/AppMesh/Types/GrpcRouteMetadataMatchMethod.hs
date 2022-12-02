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
-- Module      : Amazonka.AppMesh.Types.GrpcRouteMetadataMatchMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcRouteMetadataMatchMethod where

import Amazonka.AppMesh.Types.MatchRange
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the match method. Specify one of the match
-- values.
--
-- /See:/ 'newGrpcRouteMetadataMatchMethod' smart constructor.
data GrpcRouteMetadataMatchMethod = GrpcRouteMetadataMatchMethod'
  { -- | The value sent by the client must match the specified value exactly.
    exact :: Prelude.Maybe Prelude.Text,
    -- | The value sent by the client must include the specified characters.
    regex :: Prelude.Maybe Prelude.Text,
    -- | An object that represents the range of values to match on.
    range :: Prelude.Maybe MatchRange,
    -- | The value sent by the client must begin with the specified characters.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The value sent by the client must end with the specified characters.
    suffix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcRouteMetadataMatchMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exact', 'grpcRouteMetadataMatchMethod_exact' - The value sent by the client must match the specified value exactly.
--
-- 'regex', 'grpcRouteMetadataMatchMethod_regex' - The value sent by the client must include the specified characters.
--
-- 'range', 'grpcRouteMetadataMatchMethod_range' - An object that represents the range of values to match on.
--
-- 'prefix', 'grpcRouteMetadataMatchMethod_prefix' - The value sent by the client must begin with the specified characters.
--
-- 'suffix', 'grpcRouteMetadataMatchMethod_suffix' - The value sent by the client must end with the specified characters.
newGrpcRouteMetadataMatchMethod ::
  GrpcRouteMetadataMatchMethod
newGrpcRouteMetadataMatchMethod =
  GrpcRouteMetadataMatchMethod'
    { exact =
        Prelude.Nothing,
      regex = Prelude.Nothing,
      range = Prelude.Nothing,
      prefix = Prelude.Nothing,
      suffix = Prelude.Nothing
    }

-- | The value sent by the client must match the specified value exactly.
grpcRouteMetadataMatchMethod_exact :: Lens.Lens' GrpcRouteMetadataMatchMethod (Prelude.Maybe Prelude.Text)
grpcRouteMetadataMatchMethod_exact = Lens.lens (\GrpcRouteMetadataMatchMethod' {exact} -> exact) (\s@GrpcRouteMetadataMatchMethod' {} a -> s {exact = a} :: GrpcRouteMetadataMatchMethod)

-- | The value sent by the client must include the specified characters.
grpcRouteMetadataMatchMethod_regex :: Lens.Lens' GrpcRouteMetadataMatchMethod (Prelude.Maybe Prelude.Text)
grpcRouteMetadataMatchMethod_regex = Lens.lens (\GrpcRouteMetadataMatchMethod' {regex} -> regex) (\s@GrpcRouteMetadataMatchMethod' {} a -> s {regex = a} :: GrpcRouteMetadataMatchMethod)

-- | An object that represents the range of values to match on.
grpcRouteMetadataMatchMethod_range :: Lens.Lens' GrpcRouteMetadataMatchMethod (Prelude.Maybe MatchRange)
grpcRouteMetadataMatchMethod_range = Lens.lens (\GrpcRouteMetadataMatchMethod' {range} -> range) (\s@GrpcRouteMetadataMatchMethod' {} a -> s {range = a} :: GrpcRouteMetadataMatchMethod)

-- | The value sent by the client must begin with the specified characters.
grpcRouteMetadataMatchMethod_prefix :: Lens.Lens' GrpcRouteMetadataMatchMethod (Prelude.Maybe Prelude.Text)
grpcRouteMetadataMatchMethod_prefix = Lens.lens (\GrpcRouteMetadataMatchMethod' {prefix} -> prefix) (\s@GrpcRouteMetadataMatchMethod' {} a -> s {prefix = a} :: GrpcRouteMetadataMatchMethod)

-- | The value sent by the client must end with the specified characters.
grpcRouteMetadataMatchMethod_suffix :: Lens.Lens' GrpcRouteMetadataMatchMethod (Prelude.Maybe Prelude.Text)
grpcRouteMetadataMatchMethod_suffix = Lens.lens (\GrpcRouteMetadataMatchMethod' {suffix} -> suffix) (\s@GrpcRouteMetadataMatchMethod' {} a -> s {suffix = a} :: GrpcRouteMetadataMatchMethod)

instance Data.FromJSON GrpcRouteMetadataMatchMethod where
  parseJSON =
    Data.withObject
      "GrpcRouteMetadataMatchMethod"
      ( \x ->
          GrpcRouteMetadataMatchMethod'
            Prelude.<$> (x Data..:? "exact")
            Prelude.<*> (x Data..:? "regex")
            Prelude.<*> (x Data..:? "range")
            Prelude.<*> (x Data..:? "prefix")
            Prelude.<*> (x Data..:? "suffix")
      )

instance
  Prelude.Hashable
    GrpcRouteMetadataMatchMethod
  where
  hashWithSalt _salt GrpcRouteMetadataMatchMethod' {..} =
    _salt `Prelude.hashWithSalt` exact
      `Prelude.hashWithSalt` regex
      `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` suffix

instance Prelude.NFData GrpcRouteMetadataMatchMethod where
  rnf GrpcRouteMetadataMatchMethod' {..} =
    Prelude.rnf exact
      `Prelude.seq` Prelude.rnf regex
      `Prelude.seq` Prelude.rnf range
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf suffix

instance Data.ToJSON GrpcRouteMetadataMatchMethod where
  toJSON GrpcRouteMetadataMatchMethod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exact" Data..=) Prelude.<$> exact,
            ("regex" Data..=) Prelude.<$> regex,
            ("range" Data..=) Prelude.<$> range,
            ("prefix" Data..=) Prelude.<$> prefix,
            ("suffix" Data..=) Prelude.<$> suffix
          ]
      )
