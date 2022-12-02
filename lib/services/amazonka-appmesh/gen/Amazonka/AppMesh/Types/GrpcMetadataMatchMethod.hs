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
-- Module      : Amazonka.AppMesh.Types.GrpcMetadataMatchMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcMetadataMatchMethod where

import Amazonka.AppMesh.Types.MatchRange
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the method header to be matched.
--
-- /See:/ 'newGrpcMetadataMatchMethod' smart constructor.
data GrpcMetadataMatchMethod = GrpcMetadataMatchMethod'
  { -- | The exact method header to be matched on.
    exact :: Prelude.Maybe Prelude.Text,
    -- | The regex used to match the method header.
    regex :: Prelude.Maybe Prelude.Text,
    range :: Prelude.Maybe MatchRange,
    -- | The specified beginning characters of the method header to be matched
    -- on.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The specified ending characters of the method header to match on.
    suffix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcMetadataMatchMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exact', 'grpcMetadataMatchMethod_exact' - The exact method header to be matched on.
--
-- 'regex', 'grpcMetadataMatchMethod_regex' - The regex used to match the method header.
--
-- 'range', 'grpcMetadataMatchMethod_range' - Undocumented member.
--
-- 'prefix', 'grpcMetadataMatchMethod_prefix' - The specified beginning characters of the method header to be matched
-- on.
--
-- 'suffix', 'grpcMetadataMatchMethod_suffix' - The specified ending characters of the method header to match on.
newGrpcMetadataMatchMethod ::
  GrpcMetadataMatchMethod
newGrpcMetadataMatchMethod =
  GrpcMetadataMatchMethod'
    { exact = Prelude.Nothing,
      regex = Prelude.Nothing,
      range = Prelude.Nothing,
      prefix = Prelude.Nothing,
      suffix = Prelude.Nothing
    }

-- | The exact method header to be matched on.
grpcMetadataMatchMethod_exact :: Lens.Lens' GrpcMetadataMatchMethod (Prelude.Maybe Prelude.Text)
grpcMetadataMatchMethod_exact = Lens.lens (\GrpcMetadataMatchMethod' {exact} -> exact) (\s@GrpcMetadataMatchMethod' {} a -> s {exact = a} :: GrpcMetadataMatchMethod)

-- | The regex used to match the method header.
grpcMetadataMatchMethod_regex :: Lens.Lens' GrpcMetadataMatchMethod (Prelude.Maybe Prelude.Text)
grpcMetadataMatchMethod_regex = Lens.lens (\GrpcMetadataMatchMethod' {regex} -> regex) (\s@GrpcMetadataMatchMethod' {} a -> s {regex = a} :: GrpcMetadataMatchMethod)

-- | Undocumented member.
grpcMetadataMatchMethod_range :: Lens.Lens' GrpcMetadataMatchMethod (Prelude.Maybe MatchRange)
grpcMetadataMatchMethod_range = Lens.lens (\GrpcMetadataMatchMethod' {range} -> range) (\s@GrpcMetadataMatchMethod' {} a -> s {range = a} :: GrpcMetadataMatchMethod)

-- | The specified beginning characters of the method header to be matched
-- on.
grpcMetadataMatchMethod_prefix :: Lens.Lens' GrpcMetadataMatchMethod (Prelude.Maybe Prelude.Text)
grpcMetadataMatchMethod_prefix = Lens.lens (\GrpcMetadataMatchMethod' {prefix} -> prefix) (\s@GrpcMetadataMatchMethod' {} a -> s {prefix = a} :: GrpcMetadataMatchMethod)

-- | The specified ending characters of the method header to match on.
grpcMetadataMatchMethod_suffix :: Lens.Lens' GrpcMetadataMatchMethod (Prelude.Maybe Prelude.Text)
grpcMetadataMatchMethod_suffix = Lens.lens (\GrpcMetadataMatchMethod' {suffix} -> suffix) (\s@GrpcMetadataMatchMethod' {} a -> s {suffix = a} :: GrpcMetadataMatchMethod)

instance Data.FromJSON GrpcMetadataMatchMethod where
  parseJSON =
    Data.withObject
      "GrpcMetadataMatchMethod"
      ( \x ->
          GrpcMetadataMatchMethod'
            Prelude.<$> (x Data..:? "exact")
            Prelude.<*> (x Data..:? "regex")
            Prelude.<*> (x Data..:? "range")
            Prelude.<*> (x Data..:? "prefix")
            Prelude.<*> (x Data..:? "suffix")
      )

instance Prelude.Hashable GrpcMetadataMatchMethod where
  hashWithSalt _salt GrpcMetadataMatchMethod' {..} =
    _salt `Prelude.hashWithSalt` exact
      `Prelude.hashWithSalt` regex
      `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` suffix

instance Prelude.NFData GrpcMetadataMatchMethod where
  rnf GrpcMetadataMatchMethod' {..} =
    Prelude.rnf exact
      `Prelude.seq` Prelude.rnf regex
      `Prelude.seq` Prelude.rnf range
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf suffix

instance Data.ToJSON GrpcMetadataMatchMethod where
  toJSON GrpcMetadataMatchMethod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exact" Data..=) Prelude.<$> exact,
            ("regex" Data..=) Prelude.<$> regex,
            ("range" Data..=) Prelude.<$> range,
            ("prefix" Data..=) Prelude.<$> prefix,
            ("suffix" Data..=) Prelude.<$> suffix
          ]
      )
