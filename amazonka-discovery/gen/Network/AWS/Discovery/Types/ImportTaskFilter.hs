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
-- Module      : Network.AWS.Discovery.Types.ImportTaskFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportTaskFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types.ImportTaskFilterName
import qualified Network.AWS.Lens as Lens

-- | A name-values pair of elements you can use to filter the results when
-- querying your import tasks. Currently, wildcards are not supported for
-- filters.
--
-- When filtering by import status, all other filter values are ignored.
--
-- /See:/ 'newImportTaskFilter' smart constructor.
data ImportTaskFilter = ImportTaskFilter'
  { -- | An array of strings that you can provide to match against a specific
    -- name, status, or import task ID to filter the results for your import
    -- task queries.
    values :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The name, status, or import task ID for a specific import task.
    name :: Core.Maybe ImportTaskFilterName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportTaskFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'importTaskFilter_values' - An array of strings that you can provide to match against a specific
-- name, status, or import task ID to filter the results for your import
-- task queries.
--
-- 'name', 'importTaskFilter_name' - The name, status, or import task ID for a specific import task.
newImportTaskFilter ::
  ImportTaskFilter
newImportTaskFilter =
  ImportTaskFilter'
    { values = Core.Nothing,
      name = Core.Nothing
    }

-- | An array of strings that you can provide to match against a specific
-- name, status, or import task ID to filter the results for your import
-- task queries.
importTaskFilter_values :: Lens.Lens' ImportTaskFilter (Core.Maybe (Core.NonEmpty Core.Text))
importTaskFilter_values = Lens.lens (\ImportTaskFilter' {values} -> values) (\s@ImportTaskFilter' {} a -> s {values = a} :: ImportTaskFilter) Core.. Lens.mapping Lens._Coerce

-- | The name, status, or import task ID for a specific import task.
importTaskFilter_name :: Lens.Lens' ImportTaskFilter (Core.Maybe ImportTaskFilterName)
importTaskFilter_name = Lens.lens (\ImportTaskFilter' {name} -> name) (\s@ImportTaskFilter' {} a -> s {name = a} :: ImportTaskFilter)

instance Core.Hashable ImportTaskFilter

instance Core.NFData ImportTaskFilter

instance Core.ToJSON ImportTaskFilter where
  toJSON ImportTaskFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("values" Core..=) Core.<$> values,
            ("name" Core..=) Core.<$> name
          ]
      )
