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
-- Module      : Amazonka.Glue.Types.CreateCsvClassifierRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CreateCsvClassifierRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CsvHeaderOption
import qualified Amazonka.Prelude as Prelude

-- | Specifies a custom CSV classifier for @CreateClassifier@ to create.
--
-- /See:/ 'newCreateCsvClassifierRequest' smart constructor.
data CreateCsvClassifierRequest = CreateCsvClassifierRequest'
  { -- | Enables the processing of files that contain only one column.
    allowSingleColumn :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the CSV file contains a header.
    containsHeader :: Prelude.Maybe CsvHeaderOption,
    -- | Enables the configuration of custom datatypes.
    customDatatypeConfigured :: Prelude.Maybe Prelude.Bool,
    -- | Creates a list of supported custom datatypes.
    customDatatypes :: Prelude.Maybe [Prelude.Text],
    -- | A custom symbol to denote what separates each column entry in the row.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | Specifies not to trim values before identifying the type of column
    -- values. The default value is true.
    disableValueTrimming :: Prelude.Maybe Prelude.Bool,
    -- | A list of strings representing column names.
    header :: Prelude.Maybe [Prelude.Text],
    -- | A custom symbol to denote what combines content into a single column
    -- value. Must be different from the column delimiter.
    quoteSymbol :: Prelude.Maybe Prelude.Text,
    -- | The name of the classifier.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCsvClassifierRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowSingleColumn', 'createCsvClassifierRequest_allowSingleColumn' - Enables the processing of files that contain only one column.
--
-- 'containsHeader', 'createCsvClassifierRequest_containsHeader' - Indicates whether the CSV file contains a header.
--
-- 'customDatatypeConfigured', 'createCsvClassifierRequest_customDatatypeConfigured' - Enables the configuration of custom datatypes.
--
-- 'customDatatypes', 'createCsvClassifierRequest_customDatatypes' - Creates a list of supported custom datatypes.
--
-- 'delimiter', 'createCsvClassifierRequest_delimiter' - A custom symbol to denote what separates each column entry in the row.
--
-- 'disableValueTrimming', 'createCsvClassifierRequest_disableValueTrimming' - Specifies not to trim values before identifying the type of column
-- values. The default value is true.
--
-- 'header', 'createCsvClassifierRequest_header' - A list of strings representing column names.
--
-- 'quoteSymbol', 'createCsvClassifierRequest_quoteSymbol' - A custom symbol to denote what combines content into a single column
-- value. Must be different from the column delimiter.
--
-- 'name', 'createCsvClassifierRequest_name' - The name of the classifier.
newCreateCsvClassifierRequest ::
  -- | 'name'
  Prelude.Text ->
  CreateCsvClassifierRequest
newCreateCsvClassifierRequest pName_ =
  CreateCsvClassifierRequest'
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
createCsvClassifierRequest_allowSingleColumn :: Lens.Lens' CreateCsvClassifierRequest (Prelude.Maybe Prelude.Bool)
createCsvClassifierRequest_allowSingleColumn = Lens.lens (\CreateCsvClassifierRequest' {allowSingleColumn} -> allowSingleColumn) (\s@CreateCsvClassifierRequest' {} a -> s {allowSingleColumn = a} :: CreateCsvClassifierRequest)

-- | Indicates whether the CSV file contains a header.
createCsvClassifierRequest_containsHeader :: Lens.Lens' CreateCsvClassifierRequest (Prelude.Maybe CsvHeaderOption)
createCsvClassifierRequest_containsHeader = Lens.lens (\CreateCsvClassifierRequest' {containsHeader} -> containsHeader) (\s@CreateCsvClassifierRequest' {} a -> s {containsHeader = a} :: CreateCsvClassifierRequest)

-- | Enables the configuration of custom datatypes.
createCsvClassifierRequest_customDatatypeConfigured :: Lens.Lens' CreateCsvClassifierRequest (Prelude.Maybe Prelude.Bool)
createCsvClassifierRequest_customDatatypeConfigured = Lens.lens (\CreateCsvClassifierRequest' {customDatatypeConfigured} -> customDatatypeConfigured) (\s@CreateCsvClassifierRequest' {} a -> s {customDatatypeConfigured = a} :: CreateCsvClassifierRequest)

-- | Creates a list of supported custom datatypes.
createCsvClassifierRequest_customDatatypes :: Lens.Lens' CreateCsvClassifierRequest (Prelude.Maybe [Prelude.Text])
createCsvClassifierRequest_customDatatypes = Lens.lens (\CreateCsvClassifierRequest' {customDatatypes} -> customDatatypes) (\s@CreateCsvClassifierRequest' {} a -> s {customDatatypes = a} :: CreateCsvClassifierRequest) Prelude.. Lens.mapping Lens.coerced

-- | A custom symbol to denote what separates each column entry in the row.
createCsvClassifierRequest_delimiter :: Lens.Lens' CreateCsvClassifierRequest (Prelude.Maybe Prelude.Text)
createCsvClassifierRequest_delimiter = Lens.lens (\CreateCsvClassifierRequest' {delimiter} -> delimiter) (\s@CreateCsvClassifierRequest' {} a -> s {delimiter = a} :: CreateCsvClassifierRequest)

-- | Specifies not to trim values before identifying the type of column
-- values. The default value is true.
createCsvClassifierRequest_disableValueTrimming :: Lens.Lens' CreateCsvClassifierRequest (Prelude.Maybe Prelude.Bool)
createCsvClassifierRequest_disableValueTrimming = Lens.lens (\CreateCsvClassifierRequest' {disableValueTrimming} -> disableValueTrimming) (\s@CreateCsvClassifierRequest' {} a -> s {disableValueTrimming = a} :: CreateCsvClassifierRequest)

-- | A list of strings representing column names.
createCsvClassifierRequest_header :: Lens.Lens' CreateCsvClassifierRequest (Prelude.Maybe [Prelude.Text])
createCsvClassifierRequest_header = Lens.lens (\CreateCsvClassifierRequest' {header} -> header) (\s@CreateCsvClassifierRequest' {} a -> s {header = a} :: CreateCsvClassifierRequest) Prelude.. Lens.mapping Lens.coerced

-- | A custom symbol to denote what combines content into a single column
-- value. Must be different from the column delimiter.
createCsvClassifierRequest_quoteSymbol :: Lens.Lens' CreateCsvClassifierRequest (Prelude.Maybe Prelude.Text)
createCsvClassifierRequest_quoteSymbol = Lens.lens (\CreateCsvClassifierRequest' {quoteSymbol} -> quoteSymbol) (\s@CreateCsvClassifierRequest' {} a -> s {quoteSymbol = a} :: CreateCsvClassifierRequest)

-- | The name of the classifier.
createCsvClassifierRequest_name :: Lens.Lens' CreateCsvClassifierRequest Prelude.Text
createCsvClassifierRequest_name = Lens.lens (\CreateCsvClassifierRequest' {name} -> name) (\s@CreateCsvClassifierRequest' {} a -> s {name = a} :: CreateCsvClassifierRequest)

instance Prelude.Hashable CreateCsvClassifierRequest where
  hashWithSalt _salt CreateCsvClassifierRequest' {..} =
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

instance Prelude.NFData CreateCsvClassifierRequest where
  rnf CreateCsvClassifierRequest' {..} =
    Prelude.rnf allowSingleColumn
      `Prelude.seq` Prelude.rnf containsHeader
      `Prelude.seq` Prelude.rnf customDatatypeConfigured
      `Prelude.seq` Prelude.rnf customDatatypes
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf disableValueTrimming
      `Prelude.seq` Prelude.rnf header
      `Prelude.seq` Prelude.rnf quoteSymbol
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON CreateCsvClassifierRequest where
  toJSON CreateCsvClassifierRequest' {..} =
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
