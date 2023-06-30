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
-- Module      : Amazonka.Glue.Types.CsvClassifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CsvClassifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CsvHeaderOption
import qualified Amazonka.Prelude as Prelude

-- | A classifier for custom @CSV@ content.
--
-- /See:/ 'newCsvClassifier' smart constructor.
data CsvClassifier = CsvClassifier'
  { -- | Enables the processing of files that contain only one column.
    allowSingleColumn :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the CSV file contains a header.
    containsHeader :: Prelude.Maybe CsvHeaderOption,
    -- | The time that this classifier was registered.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Enables the custom datatype to be configured.
    customDatatypeConfigured :: Prelude.Maybe Prelude.Bool,
    -- | A list of custom datatypes including \"BINARY\", \"BOOLEAN\", \"DATE\",
    -- \"DECIMAL\", \"DOUBLE\", \"FLOAT\", \"INT\", \"LONG\", \"SHORT\",
    -- \"STRING\", \"TIMESTAMP\".
    customDatatypes :: Prelude.Maybe [Prelude.Text],
    -- | A custom symbol to denote what separates each column entry in the row.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | Specifies not to trim values before identifying the type of column
    -- values. The default value is @true@.
    disableValueTrimming :: Prelude.Maybe Prelude.Bool,
    -- | A list of strings representing column names.
    header :: Prelude.Maybe [Prelude.Text],
    -- | The time that this classifier was last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | A custom symbol to denote what combines content into a single column
    -- value. It must be different from the column delimiter.
    quoteSymbol :: Prelude.Maybe Prelude.Text,
    -- | The version of this classifier.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The name of the classifier.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CsvClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowSingleColumn', 'csvClassifier_allowSingleColumn' - Enables the processing of files that contain only one column.
--
-- 'containsHeader', 'csvClassifier_containsHeader' - Indicates whether the CSV file contains a header.
--
-- 'creationTime', 'csvClassifier_creationTime' - The time that this classifier was registered.
--
-- 'customDatatypeConfigured', 'csvClassifier_customDatatypeConfigured' - Enables the custom datatype to be configured.
--
-- 'customDatatypes', 'csvClassifier_customDatatypes' - A list of custom datatypes including \"BINARY\", \"BOOLEAN\", \"DATE\",
-- \"DECIMAL\", \"DOUBLE\", \"FLOAT\", \"INT\", \"LONG\", \"SHORT\",
-- \"STRING\", \"TIMESTAMP\".
--
-- 'delimiter', 'csvClassifier_delimiter' - A custom symbol to denote what separates each column entry in the row.
--
-- 'disableValueTrimming', 'csvClassifier_disableValueTrimming' - Specifies not to trim values before identifying the type of column
-- values. The default value is @true@.
--
-- 'header', 'csvClassifier_header' - A list of strings representing column names.
--
-- 'lastUpdated', 'csvClassifier_lastUpdated' - The time that this classifier was last updated.
--
-- 'quoteSymbol', 'csvClassifier_quoteSymbol' - A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
--
-- 'version', 'csvClassifier_version' - The version of this classifier.
--
-- 'name', 'csvClassifier_name' - The name of the classifier.
newCsvClassifier ::
  -- | 'name'
  Prelude.Text ->
  CsvClassifier
newCsvClassifier pName_ =
  CsvClassifier'
    { allowSingleColumn = Prelude.Nothing,
      containsHeader = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      customDatatypeConfigured = Prelude.Nothing,
      customDatatypes = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      disableValueTrimming = Prelude.Nothing,
      header = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      quoteSymbol = Prelude.Nothing,
      version = Prelude.Nothing,
      name = pName_
    }

-- | Enables the processing of files that contain only one column.
csvClassifier_allowSingleColumn :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Bool)
csvClassifier_allowSingleColumn = Lens.lens (\CsvClassifier' {allowSingleColumn} -> allowSingleColumn) (\s@CsvClassifier' {} a -> s {allowSingleColumn = a} :: CsvClassifier)

-- | Indicates whether the CSV file contains a header.
csvClassifier_containsHeader :: Lens.Lens' CsvClassifier (Prelude.Maybe CsvHeaderOption)
csvClassifier_containsHeader = Lens.lens (\CsvClassifier' {containsHeader} -> containsHeader) (\s@CsvClassifier' {} a -> s {containsHeader = a} :: CsvClassifier)

-- | The time that this classifier was registered.
csvClassifier_creationTime :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.UTCTime)
csvClassifier_creationTime = Lens.lens (\CsvClassifier' {creationTime} -> creationTime) (\s@CsvClassifier' {} a -> s {creationTime = a} :: CsvClassifier) Prelude.. Lens.mapping Data._Time

-- | Enables the custom datatype to be configured.
csvClassifier_customDatatypeConfigured :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Bool)
csvClassifier_customDatatypeConfigured = Lens.lens (\CsvClassifier' {customDatatypeConfigured} -> customDatatypeConfigured) (\s@CsvClassifier' {} a -> s {customDatatypeConfigured = a} :: CsvClassifier)

-- | A list of custom datatypes including \"BINARY\", \"BOOLEAN\", \"DATE\",
-- \"DECIMAL\", \"DOUBLE\", \"FLOAT\", \"INT\", \"LONG\", \"SHORT\",
-- \"STRING\", \"TIMESTAMP\".
csvClassifier_customDatatypes :: Lens.Lens' CsvClassifier (Prelude.Maybe [Prelude.Text])
csvClassifier_customDatatypes = Lens.lens (\CsvClassifier' {customDatatypes} -> customDatatypes) (\s@CsvClassifier' {} a -> s {customDatatypes = a} :: CsvClassifier) Prelude.. Lens.mapping Lens.coerced

-- | A custom symbol to denote what separates each column entry in the row.
csvClassifier_delimiter :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Text)
csvClassifier_delimiter = Lens.lens (\CsvClassifier' {delimiter} -> delimiter) (\s@CsvClassifier' {} a -> s {delimiter = a} :: CsvClassifier)

-- | Specifies not to trim values before identifying the type of column
-- values. The default value is @true@.
csvClassifier_disableValueTrimming :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Bool)
csvClassifier_disableValueTrimming = Lens.lens (\CsvClassifier' {disableValueTrimming} -> disableValueTrimming) (\s@CsvClassifier' {} a -> s {disableValueTrimming = a} :: CsvClassifier)

-- | A list of strings representing column names.
csvClassifier_header :: Lens.Lens' CsvClassifier (Prelude.Maybe [Prelude.Text])
csvClassifier_header = Lens.lens (\CsvClassifier' {header} -> header) (\s@CsvClassifier' {} a -> s {header = a} :: CsvClassifier) Prelude.. Lens.mapping Lens.coerced

-- | The time that this classifier was last updated.
csvClassifier_lastUpdated :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.UTCTime)
csvClassifier_lastUpdated = Lens.lens (\CsvClassifier' {lastUpdated} -> lastUpdated) (\s@CsvClassifier' {} a -> s {lastUpdated = a} :: CsvClassifier) Prelude.. Lens.mapping Data._Time

-- | A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
csvClassifier_quoteSymbol :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Text)
csvClassifier_quoteSymbol = Lens.lens (\CsvClassifier' {quoteSymbol} -> quoteSymbol) (\s@CsvClassifier' {} a -> s {quoteSymbol = a} :: CsvClassifier)

-- | The version of this classifier.
csvClassifier_version :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Integer)
csvClassifier_version = Lens.lens (\CsvClassifier' {version} -> version) (\s@CsvClassifier' {} a -> s {version = a} :: CsvClassifier)

-- | The name of the classifier.
csvClassifier_name :: Lens.Lens' CsvClassifier Prelude.Text
csvClassifier_name = Lens.lens (\CsvClassifier' {name} -> name) (\s@CsvClassifier' {} a -> s {name = a} :: CsvClassifier)

instance Data.FromJSON CsvClassifier where
  parseJSON =
    Data.withObject
      "CsvClassifier"
      ( \x ->
          CsvClassifier'
            Prelude.<$> (x Data..:? "AllowSingleColumn")
            Prelude.<*> (x Data..:? "ContainsHeader")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "CustomDatatypeConfigured")
            Prelude.<*> ( x
                            Data..:? "CustomDatatypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Delimiter")
            Prelude.<*> (x Data..:? "DisableValueTrimming")
            Prelude.<*> (x Data..:? "Header" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "QuoteSymbol")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable CsvClassifier where
  hashWithSalt _salt CsvClassifier' {..} =
    _salt
      `Prelude.hashWithSalt` allowSingleColumn
      `Prelude.hashWithSalt` containsHeader
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` customDatatypeConfigured
      `Prelude.hashWithSalt` customDatatypes
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` disableValueTrimming
      `Prelude.hashWithSalt` header
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` quoteSymbol
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name

instance Prelude.NFData CsvClassifier where
  rnf CsvClassifier' {..} =
    Prelude.rnf allowSingleColumn
      `Prelude.seq` Prelude.rnf containsHeader
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf customDatatypeConfigured
      `Prelude.seq` Prelude.rnf customDatatypes
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf disableValueTrimming
      `Prelude.seq` Prelude.rnf header
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf quoteSymbol
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
