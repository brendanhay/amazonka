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
-- Module      : Amazonka.QuickSight.Types.SelectedSheetsFilterScopeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SelectedSheetsFilterScopeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SheetVisualScopingConfiguration

-- | The configuration for applying a filter to specific sheets or visuals.
-- You can apply this filter to multiple visuals that are on one sheet or
-- to all visuals on a sheet.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newSelectedSheetsFilterScopeConfiguration' smart constructor.
data SelectedSheetsFilterScopeConfiguration = SelectedSheetsFilterScopeConfiguration'
  { -- | The sheet ID and visual IDs of the sheet and visuals that the filter is
    -- applied to.
    sheetVisualScopingConfigurations :: Prelude.Maybe (Prelude.NonEmpty SheetVisualScopingConfiguration)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectedSheetsFilterScopeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sheetVisualScopingConfigurations', 'selectedSheetsFilterScopeConfiguration_sheetVisualScopingConfigurations' - The sheet ID and visual IDs of the sheet and visuals that the filter is
-- applied to.
newSelectedSheetsFilterScopeConfiguration ::
  SelectedSheetsFilterScopeConfiguration
newSelectedSheetsFilterScopeConfiguration =
  SelectedSheetsFilterScopeConfiguration'
    { sheetVisualScopingConfigurations =
        Prelude.Nothing
    }

-- | The sheet ID and visual IDs of the sheet and visuals that the filter is
-- applied to.
selectedSheetsFilterScopeConfiguration_sheetVisualScopingConfigurations :: Lens.Lens' SelectedSheetsFilterScopeConfiguration (Prelude.Maybe (Prelude.NonEmpty SheetVisualScopingConfiguration))
selectedSheetsFilterScopeConfiguration_sheetVisualScopingConfigurations = Lens.lens (\SelectedSheetsFilterScopeConfiguration' {sheetVisualScopingConfigurations} -> sheetVisualScopingConfigurations) (\s@SelectedSheetsFilterScopeConfiguration' {} a -> s {sheetVisualScopingConfigurations = a} :: SelectedSheetsFilterScopeConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    SelectedSheetsFilterScopeConfiguration
  where
  parseJSON =
    Data.withObject
      "SelectedSheetsFilterScopeConfiguration"
      ( \x ->
          SelectedSheetsFilterScopeConfiguration'
            Prelude.<$> (x Data..:? "SheetVisualScopingConfigurations")
      )

instance
  Prelude.Hashable
    SelectedSheetsFilterScopeConfiguration
  where
  hashWithSalt
    _salt
    SelectedSheetsFilterScopeConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` sheetVisualScopingConfigurations

instance
  Prelude.NFData
    SelectedSheetsFilterScopeConfiguration
  where
  rnf SelectedSheetsFilterScopeConfiguration' {..} =
    Prelude.rnf sheetVisualScopingConfigurations

instance
  Data.ToJSON
    SelectedSheetsFilterScopeConfiguration
  where
  toJSON SelectedSheetsFilterScopeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SheetVisualScopingConfigurations" Data..=)
              Prelude.<$> sheetVisualScopingConfigurations
          ]
      )
