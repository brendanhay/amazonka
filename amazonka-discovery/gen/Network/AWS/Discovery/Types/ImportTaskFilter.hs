{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Discovery.Types.ImportTaskFilterName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    values :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name, status, or import task ID for a specific import task.
    name :: Prelude.Maybe ImportTaskFilterName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { values = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | An array of strings that you can provide to match against a specific
-- name, status, or import task ID to filter the results for your import
-- task queries.
importTaskFilter_values :: Lens.Lens' ImportTaskFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
importTaskFilter_values = Lens.lens (\ImportTaskFilter' {values} -> values) (\s@ImportTaskFilter' {} a -> s {values = a} :: ImportTaskFilter) Prelude.. Lens.mapping Prelude._Coerce

-- | The name, status, or import task ID for a specific import task.
importTaskFilter_name :: Lens.Lens' ImportTaskFilter (Prelude.Maybe ImportTaskFilterName)
importTaskFilter_name = Lens.lens (\ImportTaskFilter' {name} -> name) (\s@ImportTaskFilter' {} a -> s {name = a} :: ImportTaskFilter)

instance Prelude.Hashable ImportTaskFilter

instance Prelude.NFData ImportTaskFilter

instance Prelude.ToJSON ImportTaskFilter where
  toJSON ImportTaskFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("values" Prelude..=) Prelude.<$> values,
            ("name" Prelude..=) Prelude.<$> name
          ]
      )
