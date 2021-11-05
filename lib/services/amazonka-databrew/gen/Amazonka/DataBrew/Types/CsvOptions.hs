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
-- Module      : Amazonka.DataBrew.Types.CsvOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.CsvOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a set of options that define how DataBrew will read a
-- comma-separated value (CSV) file when creating a dataset from that file.
--
-- /See:/ 'newCsvOptions' smart constructor.
data CsvOptions = CsvOptions'
  { -- | A variable that specifies whether the first row in the file is parsed as
    -- the header. If this value is false, column names are auto-generated.
    headerRow :: Prelude.Maybe Prelude.Bool,
    -- | A single character that specifies the delimiter being used in the CSV
    -- file.
    delimiter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CsvOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerRow', 'csvOptions_headerRow' - A variable that specifies whether the first row in the file is parsed as
-- the header. If this value is false, column names are auto-generated.
--
-- 'delimiter', 'csvOptions_delimiter' - A single character that specifies the delimiter being used in the CSV
-- file.
newCsvOptions ::
  CsvOptions
newCsvOptions =
  CsvOptions'
    { headerRow = Prelude.Nothing,
      delimiter = Prelude.Nothing
    }

-- | A variable that specifies whether the first row in the file is parsed as
-- the header. If this value is false, column names are auto-generated.
csvOptions_headerRow :: Lens.Lens' CsvOptions (Prelude.Maybe Prelude.Bool)
csvOptions_headerRow = Lens.lens (\CsvOptions' {headerRow} -> headerRow) (\s@CsvOptions' {} a -> s {headerRow = a} :: CsvOptions)

-- | A single character that specifies the delimiter being used in the CSV
-- file.
csvOptions_delimiter :: Lens.Lens' CsvOptions (Prelude.Maybe Prelude.Text)
csvOptions_delimiter = Lens.lens (\CsvOptions' {delimiter} -> delimiter) (\s@CsvOptions' {} a -> s {delimiter = a} :: CsvOptions)

instance Core.FromJSON CsvOptions where
  parseJSON =
    Core.withObject
      "CsvOptions"
      ( \x ->
          CsvOptions'
            Prelude.<$> (x Core..:? "HeaderRow")
            Prelude.<*> (x Core..:? "Delimiter")
      )

instance Prelude.Hashable CsvOptions

instance Prelude.NFData CsvOptions

instance Core.ToJSON CsvOptions where
  toJSON CsvOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HeaderRow" Core..=) Prelude.<$> headerRow,
            ("Delimiter" Core..=) Prelude.<$> delimiter
          ]
      )
