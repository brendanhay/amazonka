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
-- Module      : Amazonka.QuickSight.Types.PivotTableSortConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotFieldSortOptions

-- | The sort configuration for a @PivotTableVisual@.
--
-- /See:/ 'newPivotTableSortConfiguration' smart constructor.
data PivotTableSortConfiguration = PivotTableSortConfiguration'
  { -- | The field sort options for a pivot table sort configuration.
    fieldSortOptions :: Prelude.Maybe [PivotFieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldSortOptions', 'pivotTableSortConfiguration_fieldSortOptions' - The field sort options for a pivot table sort configuration.
newPivotTableSortConfiguration ::
  PivotTableSortConfiguration
newPivotTableSortConfiguration =
  PivotTableSortConfiguration'
    { fieldSortOptions =
        Prelude.Nothing
    }

-- | The field sort options for a pivot table sort configuration.
pivotTableSortConfiguration_fieldSortOptions :: Lens.Lens' PivotTableSortConfiguration (Prelude.Maybe [PivotFieldSortOptions])
pivotTableSortConfiguration_fieldSortOptions = Lens.lens (\PivotTableSortConfiguration' {fieldSortOptions} -> fieldSortOptions) (\s@PivotTableSortConfiguration' {} a -> s {fieldSortOptions = a} :: PivotTableSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PivotTableSortConfiguration where
  parseJSON =
    Data.withObject
      "PivotTableSortConfiguration"
      ( \x ->
          PivotTableSortConfiguration'
            Prelude.<$> ( x Data..:? "FieldSortOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PivotTableSortConfiguration where
  hashWithSalt _salt PivotTableSortConfiguration' {..} =
    _salt `Prelude.hashWithSalt` fieldSortOptions

instance Prelude.NFData PivotTableSortConfiguration where
  rnf PivotTableSortConfiguration' {..} =
    Prelude.rnf fieldSortOptions

instance Data.ToJSON PivotTableSortConfiguration where
  toJSON PivotTableSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldSortOptions" Data..=)
              Prelude.<$> fieldSortOptions
          ]
      )
