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
-- Module      : Amazonka.DataBrew.Types.ExcelOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.ExcelOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a set of options that define how DataBrew will interpret a
-- Microsoft Excel file when creating a dataset from that file.
--
-- /See:/ 'newExcelOptions' smart constructor.
data ExcelOptions = ExcelOptions'
  { -- | A variable that specifies whether the first row in the file is parsed as
    -- the header. If this value is false, column names are auto-generated.
    headerRow :: Prelude.Maybe Prelude.Bool,
    -- | One or more sheet numbers in the Excel file that will be included in the
    -- dataset.
    sheetIndexes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Natural),
    -- | One or more named sheets in the Excel file that will be included in the
    -- dataset.
    sheetNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExcelOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerRow', 'excelOptions_headerRow' - A variable that specifies whether the first row in the file is parsed as
-- the header. If this value is false, column names are auto-generated.
--
-- 'sheetIndexes', 'excelOptions_sheetIndexes' - One or more sheet numbers in the Excel file that will be included in the
-- dataset.
--
-- 'sheetNames', 'excelOptions_sheetNames' - One or more named sheets in the Excel file that will be included in the
-- dataset.
newExcelOptions ::
  ExcelOptions
newExcelOptions =
  ExcelOptions'
    { headerRow = Prelude.Nothing,
      sheetIndexes = Prelude.Nothing,
      sheetNames = Prelude.Nothing
    }

-- | A variable that specifies whether the first row in the file is parsed as
-- the header. If this value is false, column names are auto-generated.
excelOptions_headerRow :: Lens.Lens' ExcelOptions (Prelude.Maybe Prelude.Bool)
excelOptions_headerRow = Lens.lens (\ExcelOptions' {headerRow} -> headerRow) (\s@ExcelOptions' {} a -> s {headerRow = a} :: ExcelOptions)

-- | One or more sheet numbers in the Excel file that will be included in the
-- dataset.
excelOptions_sheetIndexes :: Lens.Lens' ExcelOptions (Prelude.Maybe (Prelude.NonEmpty Prelude.Natural))
excelOptions_sheetIndexes = Lens.lens (\ExcelOptions' {sheetIndexes} -> sheetIndexes) (\s@ExcelOptions' {} a -> s {sheetIndexes = a} :: ExcelOptions) Prelude.. Lens.mapping Lens.coerced

-- | One or more named sheets in the Excel file that will be included in the
-- dataset.
excelOptions_sheetNames :: Lens.Lens' ExcelOptions (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
excelOptions_sheetNames = Lens.lens (\ExcelOptions' {sheetNames} -> sheetNames) (\s@ExcelOptions' {} a -> s {sheetNames = a} :: ExcelOptions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExcelOptions where
  parseJSON =
    Data.withObject
      "ExcelOptions"
      ( \x ->
          ExcelOptions'
            Prelude.<$> (x Data..:? "HeaderRow")
            Prelude.<*> (x Data..:? "SheetIndexes")
            Prelude.<*> (x Data..:? "SheetNames")
      )

instance Prelude.Hashable ExcelOptions where
  hashWithSalt _salt ExcelOptions' {..} =
    _salt
      `Prelude.hashWithSalt` headerRow
      `Prelude.hashWithSalt` sheetIndexes
      `Prelude.hashWithSalt` sheetNames

instance Prelude.NFData ExcelOptions where
  rnf ExcelOptions' {..} =
    Prelude.rnf headerRow
      `Prelude.seq` Prelude.rnf sheetIndexes
      `Prelude.seq` Prelude.rnf sheetNames

instance Data.ToJSON ExcelOptions where
  toJSON ExcelOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HeaderRow" Data..=) Prelude.<$> headerRow,
            ("SheetIndexes" Data..=) Prelude.<$> sheetIndexes,
            ("SheetNames" Data..=) Prelude.<$> sheetNames
          ]
      )
