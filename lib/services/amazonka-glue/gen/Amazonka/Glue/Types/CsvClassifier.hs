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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CsvClassifier where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.CsvHeaderOption
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A classifier for custom @CSV@ content.
--
-- /See:/ 'newCsvClassifier' smart constructor.
data CsvClassifier = CsvClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | A custom symbol to denote what combines content into a single column
    -- value. It must be different from the column delimiter.
    quoteSymbol :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the CSV file contains a header.
    containsHeader :: Prelude.Maybe CsvHeaderOption,
    -- | The time that this classifier was last updated.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | Specifies not to trim values before identifying the type of column
    -- values. The default value is @true@.
    disableValueTrimming :: Prelude.Maybe Prelude.Bool,
    -- | A list of strings representing column names.
    header :: Prelude.Maybe [Prelude.Text],
    -- | The version of this classifier.
    version :: Prelude.Maybe Prelude.Integer,
    -- | Enables the processing of files that contain only one column.
    allowSingleColumn :: Prelude.Maybe Prelude.Bool,
    -- | A custom symbol to denote what separates each column entry in the row.
    delimiter :: Prelude.Maybe Prelude.Text,
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
-- 'creationTime', 'csvClassifier_creationTime' - The time that this classifier was registered.
--
-- 'quoteSymbol', 'csvClassifier_quoteSymbol' - A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
--
-- 'containsHeader', 'csvClassifier_containsHeader' - Indicates whether the CSV file contains a header.
--
-- 'lastUpdated', 'csvClassifier_lastUpdated' - The time that this classifier was last updated.
--
-- 'disableValueTrimming', 'csvClassifier_disableValueTrimming' - Specifies not to trim values before identifying the type of column
-- values. The default value is @true@.
--
-- 'header', 'csvClassifier_header' - A list of strings representing column names.
--
-- 'version', 'csvClassifier_version' - The version of this classifier.
--
-- 'allowSingleColumn', 'csvClassifier_allowSingleColumn' - Enables the processing of files that contain only one column.
--
-- 'delimiter', 'csvClassifier_delimiter' - A custom symbol to denote what separates each column entry in the row.
--
-- 'name', 'csvClassifier_name' - The name of the classifier.
newCsvClassifier ::
  -- | 'name'
  Prelude.Text ->
  CsvClassifier
newCsvClassifier pName_ =
  CsvClassifier'
    { creationTime = Prelude.Nothing,
      quoteSymbol = Prelude.Nothing,
      containsHeader = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      disableValueTrimming = Prelude.Nothing,
      header = Prelude.Nothing,
      version = Prelude.Nothing,
      allowSingleColumn = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      name = pName_
    }

-- | The time that this classifier was registered.
csvClassifier_creationTime :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.UTCTime)
csvClassifier_creationTime = Lens.lens (\CsvClassifier' {creationTime} -> creationTime) (\s@CsvClassifier' {} a -> s {creationTime = a} :: CsvClassifier) Prelude.. Lens.mapping Core._Time

-- | A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
csvClassifier_quoteSymbol :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Text)
csvClassifier_quoteSymbol = Lens.lens (\CsvClassifier' {quoteSymbol} -> quoteSymbol) (\s@CsvClassifier' {} a -> s {quoteSymbol = a} :: CsvClassifier)

-- | Indicates whether the CSV file contains a header.
csvClassifier_containsHeader :: Lens.Lens' CsvClassifier (Prelude.Maybe CsvHeaderOption)
csvClassifier_containsHeader = Lens.lens (\CsvClassifier' {containsHeader} -> containsHeader) (\s@CsvClassifier' {} a -> s {containsHeader = a} :: CsvClassifier)

-- | The time that this classifier was last updated.
csvClassifier_lastUpdated :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.UTCTime)
csvClassifier_lastUpdated = Lens.lens (\CsvClassifier' {lastUpdated} -> lastUpdated) (\s@CsvClassifier' {} a -> s {lastUpdated = a} :: CsvClassifier) Prelude.. Lens.mapping Core._Time

-- | Specifies not to trim values before identifying the type of column
-- values. The default value is @true@.
csvClassifier_disableValueTrimming :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Bool)
csvClassifier_disableValueTrimming = Lens.lens (\CsvClassifier' {disableValueTrimming} -> disableValueTrimming) (\s@CsvClassifier' {} a -> s {disableValueTrimming = a} :: CsvClassifier)

-- | A list of strings representing column names.
csvClassifier_header :: Lens.Lens' CsvClassifier (Prelude.Maybe [Prelude.Text])
csvClassifier_header = Lens.lens (\CsvClassifier' {header} -> header) (\s@CsvClassifier' {} a -> s {header = a} :: CsvClassifier) Prelude.. Lens.mapping Lens.coerced

-- | The version of this classifier.
csvClassifier_version :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Integer)
csvClassifier_version = Lens.lens (\CsvClassifier' {version} -> version) (\s@CsvClassifier' {} a -> s {version = a} :: CsvClassifier)

-- | Enables the processing of files that contain only one column.
csvClassifier_allowSingleColumn :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Bool)
csvClassifier_allowSingleColumn = Lens.lens (\CsvClassifier' {allowSingleColumn} -> allowSingleColumn) (\s@CsvClassifier' {} a -> s {allowSingleColumn = a} :: CsvClassifier)

-- | A custom symbol to denote what separates each column entry in the row.
csvClassifier_delimiter :: Lens.Lens' CsvClassifier (Prelude.Maybe Prelude.Text)
csvClassifier_delimiter = Lens.lens (\CsvClassifier' {delimiter} -> delimiter) (\s@CsvClassifier' {} a -> s {delimiter = a} :: CsvClassifier)

-- | The name of the classifier.
csvClassifier_name :: Lens.Lens' CsvClassifier Prelude.Text
csvClassifier_name = Lens.lens (\CsvClassifier' {name} -> name) (\s@CsvClassifier' {} a -> s {name = a} :: CsvClassifier)

instance Core.FromJSON CsvClassifier where
  parseJSON =
    Core.withObject
      "CsvClassifier"
      ( \x ->
          CsvClassifier'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "QuoteSymbol")
            Prelude.<*> (x Core..:? "ContainsHeader")
            Prelude.<*> (x Core..:? "LastUpdated")
            Prelude.<*> (x Core..:? "DisableValueTrimming")
            Prelude.<*> (x Core..:? "Header" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "AllowSingleColumn")
            Prelude.<*> (x Core..:? "Delimiter")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable CsvClassifier

instance Prelude.NFData CsvClassifier
