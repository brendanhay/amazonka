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
-- Module      : Amazonka.Glue.Types.UpdateCsvClassifierRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.UpdateCsvClassifierRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CsvHeaderOption
import qualified Amazonka.Prelude as Prelude

-- | Specifies a custom CSV classifier to be updated.
--
-- /See:/ 'newUpdateCsvClassifierRequest' smart constructor.
data UpdateCsvClassifierRequest = UpdateCsvClassifierRequest'
  { -- | Enables the processing of files that contain only one column.
    allowSingleColumn :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the CSV file contains a header.
    containsHeader :: Prelude.Maybe CsvHeaderOption,
    -- | Specifies the configuration of custom datatypes.
    customDatatypeConfigured :: Prelude.Maybe Prelude.Bool,
    -- | Specifies a list of supported custom datatypes.
    customDatatypes :: Prelude.Maybe [Prelude.Text],
    -- | A custom symbol to denote what separates each column entry in the row.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | Specifies not to trim values before identifying the type of column
    -- values. The default value is true.
    disableValueTrimming :: Prelude.Maybe Prelude.Bool,
    -- | A list of strings representing column names.
    header :: Prelude.Maybe [Prelude.Text],
    -- | A custom symbol to denote what combines content into a single column
    -- value. It must be different from the column delimiter.
    quoteSymbol :: Prelude.Maybe Prelude.Text,
    -- | The name of the classifier.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCsvClassifierRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowSingleColumn', 'updateCsvClassifierRequest_allowSingleColumn' - Enables the processing of files that contain only one column.
--
-- 'containsHeader', 'updateCsvClassifierRequest_containsHeader' - Indicates whether the CSV file contains a header.
--
-- 'customDatatypeConfigured', 'updateCsvClassifierRequest_customDatatypeConfigured' - Specifies the configuration of custom datatypes.
--
-- 'customDatatypes', 'updateCsvClassifierRequest_customDatatypes' - Specifies a list of supported custom datatypes.
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
-- 'name', 'updateCsvClassifierRequest_name' - The name of the classifier.
newUpdateCsvClassifierRequest ::
  -- | 'name'
  Prelude.Text ->
  UpdateCsvClassifierRequest
newUpdateCsvClassifierRequest pName_ =
  UpdateCsvClassifierRequest'
    { allowSingleColumn =
        Prelude.Nothing,
      containsHeader = Prelude.Nothing,
      customDatatypeConfigured = Prelude.Nothing,
      customDatatypes = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      disableValueTrimming = Prelude.Nothing,
      header = Prelude.Nothing,
      quoteSymbol = Prelude.Nothing,
      name = pName_
    }

-- | Enables the processing of files that contain only one column.
updateCsvClassifierRequest_allowSingleColumn :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe Prelude.Bool)
updateCsvClassifierRequest_allowSingleColumn = Lens.lens (\UpdateCsvClassifierRequest' {allowSingleColumn} -> allowSingleColumn) (\s@UpdateCsvClassifierRequest' {} a -> s {allowSingleColumn = a} :: UpdateCsvClassifierRequest)

-- | Indicates whether the CSV file contains a header.
updateCsvClassifierRequest_containsHeader :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe CsvHeaderOption)
updateCsvClassifierRequest_containsHeader = Lens.lens (\UpdateCsvClassifierRequest' {containsHeader} -> containsHeader) (\s@UpdateCsvClassifierRequest' {} a -> s {containsHeader = a} :: UpdateCsvClassifierRequest)

-- | Specifies the configuration of custom datatypes.
updateCsvClassifierRequest_customDatatypeConfigured :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe Prelude.Bool)
updateCsvClassifierRequest_customDatatypeConfigured = Lens.lens (\UpdateCsvClassifierRequest' {customDatatypeConfigured} -> customDatatypeConfigured) (\s@UpdateCsvClassifierRequest' {} a -> s {customDatatypeConfigured = a} :: UpdateCsvClassifierRequest)

-- | Specifies a list of supported custom datatypes.
updateCsvClassifierRequest_customDatatypes :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe [Prelude.Text])
updateCsvClassifierRequest_customDatatypes = Lens.lens (\UpdateCsvClassifierRequest' {customDatatypes} -> customDatatypes) (\s@UpdateCsvClassifierRequest' {} a -> s {customDatatypes = a} :: UpdateCsvClassifierRequest) Prelude.. Lens.mapping Lens.coerced

-- | A custom symbol to denote what separates each column entry in the row.
updateCsvClassifierRequest_delimiter :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe Prelude.Text)
updateCsvClassifierRequest_delimiter = Lens.lens (\UpdateCsvClassifierRequest' {delimiter} -> delimiter) (\s@UpdateCsvClassifierRequest' {} a -> s {delimiter = a} :: UpdateCsvClassifierRequest)

-- | Specifies not to trim values before identifying the type of column
-- values. The default value is true.
updateCsvClassifierRequest_disableValueTrimming :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe Prelude.Bool)
updateCsvClassifierRequest_disableValueTrimming = Lens.lens (\UpdateCsvClassifierRequest' {disableValueTrimming} -> disableValueTrimming) (\s@UpdateCsvClassifierRequest' {} a -> s {disableValueTrimming = a} :: UpdateCsvClassifierRequest)

-- | A list of strings representing column names.
updateCsvClassifierRequest_header :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe [Prelude.Text])
updateCsvClassifierRequest_header = Lens.lens (\UpdateCsvClassifierRequest' {header} -> header) (\s@UpdateCsvClassifierRequest' {} a -> s {header = a} :: UpdateCsvClassifierRequest) Prelude.. Lens.mapping Lens.coerced

-- | A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
updateCsvClassifierRequest_quoteSymbol :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe Prelude.Text)
updateCsvClassifierRequest_quoteSymbol = Lens.lens (\UpdateCsvClassifierRequest' {quoteSymbol} -> quoteSymbol) (\s@UpdateCsvClassifierRequest' {} a -> s {quoteSymbol = a} :: UpdateCsvClassifierRequest)

-- | The name of the classifier.
updateCsvClassifierRequest_name :: Lens.Lens' UpdateCsvClassifierRequest Prelude.Text
updateCsvClassifierRequest_name = Lens.lens (\UpdateCsvClassifierRequest' {name} -> name) (\s@UpdateCsvClassifierRequest' {} a -> s {name = a} :: UpdateCsvClassifierRequest)

instance Prelude.Hashable UpdateCsvClassifierRequest where
  hashWithSalt _salt UpdateCsvClassifierRequest' {..} =
    _salt
      `Prelude.hashWithSalt` allowSingleColumn
      `Prelude.hashWithSalt` containsHeader
      `Prelude.hashWithSalt` customDatatypeConfigured
      `Prelude.hashWithSalt` customDatatypes
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` disableValueTrimming
      `Prelude.hashWithSalt` header
      `Prelude.hashWithSalt` quoteSymbol
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateCsvClassifierRequest where
  rnf UpdateCsvClassifierRequest' {..} =
    Prelude.rnf allowSingleColumn
      `Prelude.seq` Prelude.rnf containsHeader
      `Prelude.seq` Prelude.rnf customDatatypeConfigured
      `Prelude.seq` Prelude.rnf customDatatypes
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf disableValueTrimming
      `Prelude.seq` Prelude.rnf header
      `Prelude.seq` Prelude.rnf quoteSymbol
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON UpdateCsvClassifierRequest where
  toJSON UpdateCsvClassifierRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowSingleColumn" Data..=)
              Prelude.<$> allowSingleColumn,
            ("ContainsHeader" Data..=)
              Prelude.<$> containsHeader,
            ("CustomDatatypeConfigured" Data..=)
              Prelude.<$> customDatatypeConfigured,
            ("CustomDatatypes" Data..=)
              Prelude.<$> customDatatypes,
            ("Delimiter" Data..=) Prelude.<$> delimiter,
            ("DisableValueTrimming" Data..=)
              Prelude.<$> disableValueTrimming,
            ("Header" Data..=) Prelude.<$> header,
            ("QuoteSymbol" Data..=) Prelude.<$> quoteSymbol,
            Prelude.Just ("Name" Data..= name)
          ]
      )
