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
-- Module      : Amazonka.Discovery.Types.ImportTaskFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.ImportTaskFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Discovery.Types.ImportTaskFilterName
import qualified Amazonka.Prelude as Prelude

-- | A name-values pair of elements you can use to filter the results when
-- querying your import tasks. Currently, wildcards are not supported for
-- filters.
--
-- When filtering by import status, all other filter values are ignored.
--
-- /See:/ 'newImportTaskFilter' smart constructor.
data ImportTaskFilter = ImportTaskFilter'
  { -- | The name, status, or import task ID for a specific import task.
    name :: Prelude.Maybe ImportTaskFilterName,
    -- | An array of strings that you can provide to match against a specific
    -- name, status, or import task ID to filter the results for your import
    -- task queries.
    values :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTaskFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'importTaskFilter_name' - The name, status, or import task ID for a specific import task.
--
-- 'values', 'importTaskFilter_values' - An array of strings that you can provide to match against a specific
-- name, status, or import task ID to filter the results for your import
-- task queries.
newImportTaskFilter ::
  ImportTaskFilter
newImportTaskFilter =
  ImportTaskFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name, status, or import task ID for a specific import task.
importTaskFilter_name :: Lens.Lens' ImportTaskFilter (Prelude.Maybe ImportTaskFilterName)
importTaskFilter_name = Lens.lens (\ImportTaskFilter' {name} -> name) (\s@ImportTaskFilter' {} a -> s {name = a} :: ImportTaskFilter)

-- | An array of strings that you can provide to match against a specific
-- name, status, or import task ID to filter the results for your import
-- task queries.
importTaskFilter_values :: Lens.Lens' ImportTaskFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
importTaskFilter_values = Lens.lens (\ImportTaskFilter' {values} -> values) (\s@ImportTaskFilter' {} a -> s {values = a} :: ImportTaskFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ImportTaskFilter where
  hashWithSalt _salt ImportTaskFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData ImportTaskFilter where
  rnf ImportTaskFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Core.ToJSON ImportTaskFilter where
  toJSON ImportTaskFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("values" Core..=) Prelude.<$> values
          ]
      )
