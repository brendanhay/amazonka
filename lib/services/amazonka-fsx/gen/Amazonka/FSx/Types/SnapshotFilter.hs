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
-- Module      : Amazonka.FSx.Types.SnapshotFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.SnapshotFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.SnapshotFilterName
import qualified Amazonka.Prelude as Prelude

-- | A filter used to restrict the results of @DescribeSnapshots@ calls. You
-- can use multiple filters to return results that meet all applied filter
-- requirements.
--
-- /See:/ 'newSnapshotFilter' smart constructor.
data SnapshotFilter = SnapshotFilter'
  { -- | The name of the filter to use. You can filter by the @file-system-id@ or
    -- by @volume-id@.
    name :: Prelude.Maybe SnapshotFilterName,
    -- | The @file-system-id@ or @volume-id@ that you are filtering for.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'snapshotFilter_name' - The name of the filter to use. You can filter by the @file-system-id@ or
-- by @volume-id@.
--
-- 'values', 'snapshotFilter_values' - The @file-system-id@ or @volume-id@ that you are filtering for.
newSnapshotFilter ::
  SnapshotFilter
newSnapshotFilter =
  SnapshotFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter to use. You can filter by the @file-system-id@ or
-- by @volume-id@.
snapshotFilter_name :: Lens.Lens' SnapshotFilter (Prelude.Maybe SnapshotFilterName)
snapshotFilter_name = Lens.lens (\SnapshotFilter' {name} -> name) (\s@SnapshotFilter' {} a -> s {name = a} :: SnapshotFilter)

-- | The @file-system-id@ or @volume-id@ that you are filtering for.
snapshotFilter_values :: Lens.Lens' SnapshotFilter (Prelude.Maybe [Prelude.Text])
snapshotFilter_values = Lens.lens (\SnapshotFilter' {values} -> values) (\s@SnapshotFilter' {} a -> s {values = a} :: SnapshotFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable SnapshotFilter where
  hashWithSalt _salt SnapshotFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData SnapshotFilter where
  rnf SnapshotFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON SnapshotFilter where
  toJSON SnapshotFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
