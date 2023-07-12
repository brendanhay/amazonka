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
-- Module      : Amazonka.QuickSight.Types.FilterScopeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterScopeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SelectedSheetsFilterScopeConfiguration

-- | The scope configuration for a @FilterGroup@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newFilterScopeConfiguration' smart constructor.
data FilterScopeConfiguration = FilterScopeConfiguration'
  { -- | The configuration for applying a filter to specific sheets.
    selectedSheets :: Prelude.Maybe SelectedSheetsFilterScopeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterScopeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectedSheets', 'filterScopeConfiguration_selectedSheets' - The configuration for applying a filter to specific sheets.
newFilterScopeConfiguration ::
  FilterScopeConfiguration
newFilterScopeConfiguration =
  FilterScopeConfiguration'
    { selectedSheets =
        Prelude.Nothing
    }

-- | The configuration for applying a filter to specific sheets.
filterScopeConfiguration_selectedSheets :: Lens.Lens' FilterScopeConfiguration (Prelude.Maybe SelectedSheetsFilterScopeConfiguration)
filterScopeConfiguration_selectedSheets = Lens.lens (\FilterScopeConfiguration' {selectedSheets} -> selectedSheets) (\s@FilterScopeConfiguration' {} a -> s {selectedSheets = a} :: FilterScopeConfiguration)

instance Data.FromJSON FilterScopeConfiguration where
  parseJSON =
    Data.withObject
      "FilterScopeConfiguration"
      ( \x ->
          FilterScopeConfiguration'
            Prelude.<$> (x Data..:? "SelectedSheets")
      )

instance Prelude.Hashable FilterScopeConfiguration where
  hashWithSalt _salt FilterScopeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` selectedSheets

instance Prelude.NFData FilterScopeConfiguration where
  rnf FilterScopeConfiguration' {..} =
    Prelude.rnf selectedSheets

instance Data.ToJSON FilterScopeConfiguration where
  toJSON FilterScopeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SelectedSheets" Data..=)
              Prelude.<$> selectedSheets
          ]
      )
