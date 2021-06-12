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
-- Module      : Network.AWS.Glue.Types.CsvClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CsvClassifier where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.CsvHeaderOption
import qualified Network.AWS.Lens as Lens

-- | A classifier for custom @CSV@ content.
--
-- /See:/ 'newCsvClassifier' smart constructor.
data CsvClassifier = CsvClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Core.Maybe Core.POSIX,
    -- | Indicates whether the CSV file contains a header.
    containsHeader :: Core.Maybe CsvHeaderOption,
    -- | A custom symbol to denote what separates each column entry in the row.
    delimiter :: Core.Maybe Core.Text,
    -- | Specifies not to trim values before identifying the type of column
    -- values. The default value is @true@.
    disableValueTrimming :: Core.Maybe Core.Bool,
    -- | The version of this classifier.
    version :: Core.Maybe Core.Integer,
    -- | The time that this classifier was last updated.
    lastUpdated :: Core.Maybe Core.POSIX,
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
-- Create a value of 'CsvClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'csvClassifier_creationTime' - The time that this classifier was registered.
--
-- 'containsHeader', 'csvClassifier_containsHeader' - Indicates whether the CSV file contains a header.
--
-- 'delimiter', 'csvClassifier_delimiter' - A custom symbol to denote what separates each column entry in the row.
--
-- 'disableValueTrimming', 'csvClassifier_disableValueTrimming' - Specifies not to trim values before identifying the type of column
-- values. The default value is @true@.
--
-- 'version', 'csvClassifier_version' - The version of this classifier.
--
-- 'lastUpdated', 'csvClassifier_lastUpdated' - The time that this classifier was last updated.
--
-- 'header', 'csvClassifier_header' - A list of strings representing column names.
--
-- 'quoteSymbol', 'csvClassifier_quoteSymbol' - A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
--
-- 'allowSingleColumn', 'csvClassifier_allowSingleColumn' - Enables the processing of files that contain only one column.
--
-- 'name', 'csvClassifier_name' - The name of the classifier.
newCsvClassifier ::
  -- | 'name'
  Core.Text ->
  CsvClassifier
newCsvClassifier pName_ =
  CsvClassifier'
    { creationTime = Core.Nothing,
      containsHeader = Core.Nothing,
      delimiter = Core.Nothing,
      disableValueTrimming = Core.Nothing,
      version = Core.Nothing,
      lastUpdated = Core.Nothing,
      header = Core.Nothing,
      quoteSymbol = Core.Nothing,
      allowSingleColumn = Core.Nothing,
      name = pName_
    }

-- | The time that this classifier was registered.
csvClassifier_creationTime :: Lens.Lens' CsvClassifier (Core.Maybe Core.UTCTime)
csvClassifier_creationTime = Lens.lens (\CsvClassifier' {creationTime} -> creationTime) (\s@CsvClassifier' {} a -> s {creationTime = a} :: CsvClassifier) Core.. Lens.mapping Core._Time

-- | Indicates whether the CSV file contains a header.
csvClassifier_containsHeader :: Lens.Lens' CsvClassifier (Core.Maybe CsvHeaderOption)
csvClassifier_containsHeader = Lens.lens (\CsvClassifier' {containsHeader} -> containsHeader) (\s@CsvClassifier' {} a -> s {containsHeader = a} :: CsvClassifier)

-- | A custom symbol to denote what separates each column entry in the row.
csvClassifier_delimiter :: Lens.Lens' CsvClassifier (Core.Maybe Core.Text)
csvClassifier_delimiter = Lens.lens (\CsvClassifier' {delimiter} -> delimiter) (\s@CsvClassifier' {} a -> s {delimiter = a} :: CsvClassifier)

-- | Specifies not to trim values before identifying the type of column
-- values. The default value is @true@.
csvClassifier_disableValueTrimming :: Lens.Lens' CsvClassifier (Core.Maybe Core.Bool)
csvClassifier_disableValueTrimming = Lens.lens (\CsvClassifier' {disableValueTrimming} -> disableValueTrimming) (\s@CsvClassifier' {} a -> s {disableValueTrimming = a} :: CsvClassifier)

-- | The version of this classifier.
csvClassifier_version :: Lens.Lens' CsvClassifier (Core.Maybe Core.Integer)
csvClassifier_version = Lens.lens (\CsvClassifier' {version} -> version) (\s@CsvClassifier' {} a -> s {version = a} :: CsvClassifier)

-- | The time that this classifier was last updated.
csvClassifier_lastUpdated :: Lens.Lens' CsvClassifier (Core.Maybe Core.UTCTime)
csvClassifier_lastUpdated = Lens.lens (\CsvClassifier' {lastUpdated} -> lastUpdated) (\s@CsvClassifier' {} a -> s {lastUpdated = a} :: CsvClassifier) Core.. Lens.mapping Core._Time

-- | A list of strings representing column names.
csvClassifier_header :: Lens.Lens' CsvClassifier (Core.Maybe [Core.Text])
csvClassifier_header = Lens.lens (\CsvClassifier' {header} -> header) (\s@CsvClassifier' {} a -> s {header = a} :: CsvClassifier) Core.. Lens.mapping Lens._Coerce

-- | A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
csvClassifier_quoteSymbol :: Lens.Lens' CsvClassifier (Core.Maybe Core.Text)
csvClassifier_quoteSymbol = Lens.lens (\CsvClassifier' {quoteSymbol} -> quoteSymbol) (\s@CsvClassifier' {} a -> s {quoteSymbol = a} :: CsvClassifier)

-- | Enables the processing of files that contain only one column.
csvClassifier_allowSingleColumn :: Lens.Lens' CsvClassifier (Core.Maybe Core.Bool)
csvClassifier_allowSingleColumn = Lens.lens (\CsvClassifier' {allowSingleColumn} -> allowSingleColumn) (\s@CsvClassifier' {} a -> s {allowSingleColumn = a} :: CsvClassifier)

-- | The name of the classifier.
csvClassifier_name :: Lens.Lens' CsvClassifier Core.Text
csvClassifier_name = Lens.lens (\CsvClassifier' {name} -> name) (\s@CsvClassifier' {} a -> s {name = a} :: CsvClassifier)

instance Core.FromJSON CsvClassifier where
  parseJSON =
    Core.withObject
      "CsvClassifier"
      ( \x ->
          CsvClassifier'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ContainsHeader")
            Core.<*> (x Core..:? "Delimiter")
            Core.<*> (x Core..:? "DisableValueTrimming")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "LastUpdated")
            Core.<*> (x Core..:? "Header" Core..!= Core.mempty)
            Core.<*> (x Core..:? "QuoteSymbol")
            Core.<*> (x Core..:? "AllowSingleColumn")
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable CsvClassifier

instance Core.NFData CsvClassifier
