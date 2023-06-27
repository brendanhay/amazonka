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
-- Module      : Amazonka.VPCLattice.Types.HeaderMatchType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.HeaderMatchType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a header match type. Only one can be provided.
--
-- /See:/ 'newHeaderMatchType' smart constructor.
data HeaderMatchType = HeaderMatchType'
  { -- | Specifies a contains type match.
    contains :: Prelude.Maybe Prelude.Text,
    -- | Specifies an exact type match.
    exact :: Prelude.Maybe Prelude.Text,
    -- | Specifies a prefix type match. Matches the value with the prefix.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeaderMatchType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contains', 'headerMatchType_contains' - Specifies a contains type match.
--
-- 'exact', 'headerMatchType_exact' - Specifies an exact type match.
--
-- 'prefix', 'headerMatchType_prefix' - Specifies a prefix type match. Matches the value with the prefix.
newHeaderMatchType ::
  HeaderMatchType
newHeaderMatchType =
  HeaderMatchType'
    { contains = Prelude.Nothing,
      exact = Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | Specifies a contains type match.
headerMatchType_contains :: Lens.Lens' HeaderMatchType (Prelude.Maybe Prelude.Text)
headerMatchType_contains = Lens.lens (\HeaderMatchType' {contains} -> contains) (\s@HeaderMatchType' {} a -> s {contains = a} :: HeaderMatchType)

-- | Specifies an exact type match.
headerMatchType_exact :: Lens.Lens' HeaderMatchType (Prelude.Maybe Prelude.Text)
headerMatchType_exact = Lens.lens (\HeaderMatchType' {exact} -> exact) (\s@HeaderMatchType' {} a -> s {exact = a} :: HeaderMatchType)

-- | Specifies a prefix type match. Matches the value with the prefix.
headerMatchType_prefix :: Lens.Lens' HeaderMatchType (Prelude.Maybe Prelude.Text)
headerMatchType_prefix = Lens.lens (\HeaderMatchType' {prefix} -> prefix) (\s@HeaderMatchType' {} a -> s {prefix = a} :: HeaderMatchType)

instance Data.FromJSON HeaderMatchType where
  parseJSON =
    Data.withObject
      "HeaderMatchType"
      ( \x ->
          HeaderMatchType'
            Prelude.<$> (x Data..:? "contains")
            Prelude.<*> (x Data..:? "exact")
            Prelude.<*> (x Data..:? "prefix")
      )

instance Prelude.Hashable HeaderMatchType where
  hashWithSalt _salt HeaderMatchType' {..} =
    _salt
      `Prelude.hashWithSalt` contains
      `Prelude.hashWithSalt` exact
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData HeaderMatchType where
  rnf HeaderMatchType' {..} =
    Prelude.rnf contains
      `Prelude.seq` Prelude.rnf exact
      `Prelude.seq` Prelude.rnf prefix

instance Data.ToJSON HeaderMatchType where
  toJSON HeaderMatchType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contains" Data..=) Prelude.<$> contains,
            ("exact" Data..=) Prelude.<$> exact,
            ("prefix" Data..=) Prelude.<$> prefix
          ]
      )
