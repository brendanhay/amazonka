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
-- Module      : Amazonka.HoneyCode.Types.DelimitedTextImportOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.DelimitedTextImportOptions where

import qualified Amazonka.Core as Core
import Amazonka.HoneyCode.Types.ImportDataCharacterEncoding
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the options relating to parsing delimited text
-- as part of an import request.
--
-- /See:/ 'newDelimitedTextImportOptions' smart constructor.
data DelimitedTextImportOptions = DelimitedTextImportOptions'
  { -- | A parameter to indicate whether empty rows should be ignored or be
    -- included in the import.
    ignoreEmptyRows :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the input file has a header row at the top containing
    -- the column names.
    hasHeaderRow :: Prelude.Maybe Prelude.Bool,
    -- | The encoding of the data in the input file.
    dataCharacterEncoding :: Prelude.Maybe ImportDataCharacterEncoding,
    -- | The delimiter to use for separating columns in a single row of the
    -- input.
    delimiter :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DelimitedTextImportOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ignoreEmptyRows', 'delimitedTextImportOptions_ignoreEmptyRows' - A parameter to indicate whether empty rows should be ignored or be
-- included in the import.
--
-- 'hasHeaderRow', 'delimitedTextImportOptions_hasHeaderRow' - Indicates whether the input file has a header row at the top containing
-- the column names.
--
-- 'dataCharacterEncoding', 'delimitedTextImportOptions_dataCharacterEncoding' - The encoding of the data in the input file.
--
-- 'delimiter', 'delimitedTextImportOptions_delimiter' - The delimiter to use for separating columns in a single row of the
-- input.
newDelimitedTextImportOptions ::
  -- | 'delimiter'
  Prelude.Text ->
  DelimitedTextImportOptions
newDelimitedTextImportOptions pDelimiter_ =
  DelimitedTextImportOptions'
    { ignoreEmptyRows =
        Prelude.Nothing,
      hasHeaderRow = Prelude.Nothing,
      dataCharacterEncoding = Prelude.Nothing,
      delimiter = pDelimiter_
    }

-- | A parameter to indicate whether empty rows should be ignored or be
-- included in the import.
delimitedTextImportOptions_ignoreEmptyRows :: Lens.Lens' DelimitedTextImportOptions (Prelude.Maybe Prelude.Bool)
delimitedTextImportOptions_ignoreEmptyRows = Lens.lens (\DelimitedTextImportOptions' {ignoreEmptyRows} -> ignoreEmptyRows) (\s@DelimitedTextImportOptions' {} a -> s {ignoreEmptyRows = a} :: DelimitedTextImportOptions)

-- | Indicates whether the input file has a header row at the top containing
-- the column names.
delimitedTextImportOptions_hasHeaderRow :: Lens.Lens' DelimitedTextImportOptions (Prelude.Maybe Prelude.Bool)
delimitedTextImportOptions_hasHeaderRow = Lens.lens (\DelimitedTextImportOptions' {hasHeaderRow} -> hasHeaderRow) (\s@DelimitedTextImportOptions' {} a -> s {hasHeaderRow = a} :: DelimitedTextImportOptions)

-- | The encoding of the data in the input file.
delimitedTextImportOptions_dataCharacterEncoding :: Lens.Lens' DelimitedTextImportOptions (Prelude.Maybe ImportDataCharacterEncoding)
delimitedTextImportOptions_dataCharacterEncoding = Lens.lens (\DelimitedTextImportOptions' {dataCharacterEncoding} -> dataCharacterEncoding) (\s@DelimitedTextImportOptions' {} a -> s {dataCharacterEncoding = a} :: DelimitedTextImportOptions)

-- | The delimiter to use for separating columns in a single row of the
-- input.
delimitedTextImportOptions_delimiter :: Lens.Lens' DelimitedTextImportOptions Prelude.Text
delimitedTextImportOptions_delimiter = Lens.lens (\DelimitedTextImportOptions' {delimiter} -> delimiter) (\s@DelimitedTextImportOptions' {} a -> s {delimiter = a} :: DelimitedTextImportOptions)

instance Core.FromJSON DelimitedTextImportOptions where
  parseJSON =
    Core.withObject
      "DelimitedTextImportOptions"
      ( \x ->
          DelimitedTextImportOptions'
            Prelude.<$> (x Core..:? "ignoreEmptyRows")
            Prelude.<*> (x Core..:? "hasHeaderRow")
            Prelude.<*> (x Core..:? "dataCharacterEncoding")
            Prelude.<*> (x Core..: "delimiter")
      )

instance Prelude.Hashable DelimitedTextImportOptions

instance Prelude.NFData DelimitedTextImportOptions

instance Core.ToJSON DelimitedTextImportOptions where
  toJSON DelimitedTextImportOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ignoreEmptyRows" Core..=)
              Prelude.<$> ignoreEmptyRows,
            ("hasHeaderRow" Core..=) Prelude.<$> hasHeaderRow,
            ("dataCharacterEncoding" Core..=)
              Prelude.<$> dataCharacterEncoding,
            Prelude.Just ("delimiter" Core..= delimiter)
          ]
      )
