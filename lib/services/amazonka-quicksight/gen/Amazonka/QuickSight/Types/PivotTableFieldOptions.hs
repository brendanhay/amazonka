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
-- Module      : Amazonka.QuickSight.Types.PivotTableFieldOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableFieldOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableDataPathOption
import Amazonka.QuickSight.Types.PivotTableFieldOption

-- | The field options for a pivot table visual.
--
-- /See:/ 'newPivotTableFieldOptions' smart constructor.
data PivotTableFieldOptions = PivotTableFieldOptions'
  { -- | The data path options for the pivot table field options.
    dataPathOptions :: Prelude.Maybe [PivotTableDataPathOption],
    -- | The selected field options for the pivot table field options.
    selectedFieldOptions :: Prelude.Maybe [PivotTableFieldOption]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableFieldOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataPathOptions', 'pivotTableFieldOptions_dataPathOptions' - The data path options for the pivot table field options.
--
-- 'selectedFieldOptions', 'pivotTableFieldOptions_selectedFieldOptions' - The selected field options for the pivot table field options.
newPivotTableFieldOptions ::
  PivotTableFieldOptions
newPivotTableFieldOptions =
  PivotTableFieldOptions'
    { dataPathOptions =
        Prelude.Nothing,
      selectedFieldOptions = Prelude.Nothing
    }

-- | The data path options for the pivot table field options.
pivotTableFieldOptions_dataPathOptions :: Lens.Lens' PivotTableFieldOptions (Prelude.Maybe [PivotTableDataPathOption])
pivotTableFieldOptions_dataPathOptions = Lens.lens (\PivotTableFieldOptions' {dataPathOptions} -> dataPathOptions) (\s@PivotTableFieldOptions' {} a -> s {dataPathOptions = a} :: PivotTableFieldOptions) Prelude.. Lens.mapping Lens.coerced

-- | The selected field options for the pivot table field options.
pivotTableFieldOptions_selectedFieldOptions :: Lens.Lens' PivotTableFieldOptions (Prelude.Maybe [PivotTableFieldOption])
pivotTableFieldOptions_selectedFieldOptions = Lens.lens (\PivotTableFieldOptions' {selectedFieldOptions} -> selectedFieldOptions) (\s@PivotTableFieldOptions' {} a -> s {selectedFieldOptions = a} :: PivotTableFieldOptions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PivotTableFieldOptions where
  parseJSON =
    Data.withObject
      "PivotTableFieldOptions"
      ( \x ->
          PivotTableFieldOptions'
            Prelude.<$> ( x Data..:? "DataPathOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "SelectedFieldOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PivotTableFieldOptions where
  hashWithSalt _salt PivotTableFieldOptions' {..} =
    _salt `Prelude.hashWithSalt` dataPathOptions
      `Prelude.hashWithSalt` selectedFieldOptions

instance Prelude.NFData PivotTableFieldOptions where
  rnf PivotTableFieldOptions' {..} =
    Prelude.rnf dataPathOptions
      `Prelude.seq` Prelude.rnf selectedFieldOptions

instance Data.ToJSON PivotTableFieldOptions where
  toJSON PivotTableFieldOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataPathOptions" Data..=)
              Prelude.<$> dataPathOptions,
            ("SelectedFieldOptions" Data..=)
              Prelude.<$> selectedFieldOptions
          ]
      )
