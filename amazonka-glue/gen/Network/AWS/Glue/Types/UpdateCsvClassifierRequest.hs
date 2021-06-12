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
-- Module      : Network.AWS.Glue.Types.UpdateCsvClassifierRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateCsvClassifierRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.CsvHeaderOption
import qualified Network.AWS.Lens as Lens

-- | Specifies a custom CSV classifier to be updated.
--
-- /See:/ 'newUpdateCsvClassifierRequest' smart constructor.
data UpdateCsvClassifierRequest = UpdateCsvClassifierRequest'
  { -- | Indicates whether the CSV file contains a header.
    containsHeader :: Core.Maybe CsvHeaderOption,
    -- | A custom symbol to denote what separates each column entry in the row.
    delimiter :: Core.Maybe Core.Text,
    -- | Specifies not to trim values before identifying the type of column
    -- values. The default value is true.
    disableValueTrimming :: Core.Maybe Core.Bool,
    -- | A list of strings representing column names.
    header :: Core.Maybe [Core.Text],
    -- | A custom symbol to denote what combines content into a single column
    -- value. It must be different from the column delimiter.
    quoteSymbol :: Core.Maybe Core.Text,
    -- | Enables the processing of files that contain only one column.
    allowSingleColumn :: Core.Maybe Core.Bool,
    -- | The name of the classifier.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCsvClassifierRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containsHeader', 'updateCsvClassifierRequest_containsHeader' - Indicates whether the CSV file contains a header.
--
-- 'delimiter', 'updateCsvClassifierRequest_delimiter' - A custom symbol to denote what separates each column entry in the row.
--
-- 'disableValueTrimming', 'updateCsvClassifierRequest_disableValueTrimming' - Specifies not to trim values before identifying the type of column
-- values. The default value is true.
--
-- 'header', 'updateCsvClassifierRequest_header' - A list of strings representing column names.
--
-- 'quoteSymbol', 'updateCsvClassifierRequest_quoteSymbol' - A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
--
-- 'allowSingleColumn', 'updateCsvClassifierRequest_allowSingleColumn' - Enables the processing of files that contain only one column.
--
-- 'name', 'updateCsvClassifierRequest_name' - The name of the classifier.
newUpdateCsvClassifierRequest ::
  -- | 'name'
  Core.Text ->
  UpdateCsvClassifierRequest
newUpdateCsvClassifierRequest pName_ =
  UpdateCsvClassifierRequest'
    { containsHeader =
        Core.Nothing,
      delimiter = Core.Nothing,
      disableValueTrimming = Core.Nothing,
      header = Core.Nothing,
      quoteSymbol = Core.Nothing,
      allowSingleColumn = Core.Nothing,
      name = pName_
    }

-- | Indicates whether the CSV file contains a header.
updateCsvClassifierRequest_containsHeader :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe CsvHeaderOption)
updateCsvClassifierRequest_containsHeader = Lens.lens (\UpdateCsvClassifierRequest' {containsHeader} -> containsHeader) (\s@UpdateCsvClassifierRequest' {} a -> s {containsHeader = a} :: UpdateCsvClassifierRequest)

-- | A custom symbol to denote what separates each column entry in the row.
updateCsvClassifierRequest_delimiter :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe Core.Text)
updateCsvClassifierRequest_delimiter = Lens.lens (\UpdateCsvClassifierRequest' {delimiter} -> delimiter) (\s@UpdateCsvClassifierRequest' {} a -> s {delimiter = a} :: UpdateCsvClassifierRequest)

-- | Specifies not to trim values before identifying the type of column
-- values. The default value is true.
updateCsvClassifierRequest_disableValueTrimming :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe Core.Bool)
updateCsvClassifierRequest_disableValueTrimming = Lens.lens (\UpdateCsvClassifierRequest' {disableValueTrimming} -> disableValueTrimming) (\s@UpdateCsvClassifierRequest' {} a -> s {disableValueTrimming = a} :: UpdateCsvClassifierRequest)

-- | A list of strings representing column names.
updateCsvClassifierRequest_header :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe [Core.Text])
updateCsvClassifierRequest_header = Lens.lens (\UpdateCsvClassifierRequest' {header} -> header) (\s@UpdateCsvClassifierRequest' {} a -> s {header = a} :: UpdateCsvClassifierRequest) Core.. Lens.mapping Lens._Coerce

-- | A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
updateCsvClassifierRequest_quoteSymbol :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe Core.Text)
updateCsvClassifierRequest_quoteSymbol = Lens.lens (\UpdateCsvClassifierRequest' {quoteSymbol} -> quoteSymbol) (\s@UpdateCsvClassifierRequest' {} a -> s {quoteSymbol = a} :: UpdateCsvClassifierRequest)

-- | Enables the processing of files that contain only one column.
updateCsvClassifierRequest_allowSingleColumn :: Lens.Lens' UpdateCsvClassifierRequest (Core.Maybe Core.Bool)
updateCsvClassifierRequest_allowSingleColumn = Lens.lens (\UpdateCsvClassifierRequest' {allowSingleColumn} -> allowSingleColumn) (\s@UpdateCsvClassifierRequest' {} a -> s {allowSingleColumn = a} :: UpdateCsvClassifierRequest)

-- | The name of the classifier.
updateCsvClassifierRequest_name :: Lens.Lens' UpdateCsvClassifierRequest Core.Text
updateCsvClassifierRequest_name = Lens.lens (\UpdateCsvClassifierRequest' {name} -> name) (\s@UpdateCsvClassifierRequest' {} a -> s {name = a} :: UpdateCsvClassifierRequest)

instance Core.Hashable UpdateCsvClassifierRequest

instance Core.NFData UpdateCsvClassifierRequest

instance Core.ToJSON UpdateCsvClassifierRequest where
  toJSON UpdateCsvClassifierRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContainsHeader" Core..=) Core.<$> containsHeader,
            ("Delimiter" Core..=) Core.<$> delimiter,
            ("DisableValueTrimming" Core..=)
              Core.<$> disableValueTrimming,
            ("Header" Core..=) Core.<$> header,
            ("QuoteSymbol" Core..=) Core.<$> quoteSymbol,
            ("AllowSingleColumn" Core..=)
              Core.<$> allowSingleColumn,
            Core.Just ("Name" Core..= name)
          ]
      )
