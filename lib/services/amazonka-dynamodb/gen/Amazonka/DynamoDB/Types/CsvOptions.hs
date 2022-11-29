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
-- Module      : Amazonka.DynamoDB.Types.CsvOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.CsvOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Processing options for the CSV file being imported.
--
-- /See:/ 'newCsvOptions' smart constructor.
data CsvOptions = CsvOptions'
  { -- | The delimiter used for separating items in the CSV file being imported.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | List of the headers used to specify a common header for all source CSV
    -- files being imported. If this field is specified then the first line of
    -- each CSV file is treated as data instead of the header. If this field is
    -- not specified the the first line of each CSV file is treated as the
    -- header.
    headerList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'delimiter', 'csvOptions_delimiter' - The delimiter used for separating items in the CSV file being imported.
--
-- 'headerList', 'csvOptions_headerList' - List of the headers used to specify a common header for all source CSV
-- files being imported. If this field is specified then the first line of
-- each CSV file is treated as data instead of the header. If this field is
-- not specified the the first line of each CSV file is treated as the
-- header.
newCsvOptions ::
  CsvOptions
newCsvOptions =
  CsvOptions'
    { delimiter = Prelude.Nothing,
      headerList = Prelude.Nothing
    }

-- | The delimiter used for separating items in the CSV file being imported.
csvOptions_delimiter :: Lens.Lens' CsvOptions (Prelude.Maybe Prelude.Text)
csvOptions_delimiter = Lens.lens (\CsvOptions' {delimiter} -> delimiter) (\s@CsvOptions' {} a -> s {delimiter = a} :: CsvOptions)

-- | List of the headers used to specify a common header for all source CSV
-- files being imported. If this field is specified then the first line of
-- each CSV file is treated as data instead of the header. If this field is
-- not specified the the first line of each CSV file is treated as the
-- header.
csvOptions_headerList :: Lens.Lens' CsvOptions (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
csvOptions_headerList = Lens.lens (\CsvOptions' {headerList} -> headerList) (\s@CsvOptions' {} a -> s {headerList = a} :: CsvOptions) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CsvOptions where
  parseJSON =
    Core.withObject
      "CsvOptions"
      ( \x ->
          CsvOptions'
            Prelude.<$> (x Core..:? "Delimiter")
            Prelude.<*> (x Core..:? "HeaderList")
      )

instance Prelude.Hashable CsvOptions where
  hashWithSalt _salt CsvOptions' {..} =
    _salt `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` headerList

instance Prelude.NFData CsvOptions where
  rnf CsvOptions' {..} =
    Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf headerList

instance Core.ToJSON CsvOptions where
  toJSON CsvOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Delimiter" Core..=) Prelude.<$> delimiter,
            ("HeaderList" Core..=) Prelude.<$> headerList
          ]
      )
