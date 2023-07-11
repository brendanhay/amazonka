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
-- Module      : Amazonka.DataBrew.Types.CsvOutputOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.CsvOutputOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a set of options that define how DataBrew will write a
-- comma-separated value (CSV) file.
--
-- /See:/ 'newCsvOutputOptions' smart constructor.
data CsvOutputOptions = CsvOutputOptions'
  { -- | A single character that specifies the delimiter used to create CSV job
    -- output.
    delimiter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CsvOutputOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delimiter', 'csvOutputOptions_delimiter' - A single character that specifies the delimiter used to create CSV job
-- output.
newCsvOutputOptions ::
  CsvOutputOptions
newCsvOutputOptions =
  CsvOutputOptions' {delimiter = Prelude.Nothing}

-- | A single character that specifies the delimiter used to create CSV job
-- output.
csvOutputOptions_delimiter :: Lens.Lens' CsvOutputOptions (Prelude.Maybe Prelude.Text)
csvOutputOptions_delimiter = Lens.lens (\CsvOutputOptions' {delimiter} -> delimiter) (\s@CsvOutputOptions' {} a -> s {delimiter = a} :: CsvOutputOptions)

instance Data.FromJSON CsvOutputOptions where
  parseJSON =
    Data.withObject
      "CsvOutputOptions"
      ( \x ->
          CsvOutputOptions'
            Prelude.<$> (x Data..:? "Delimiter")
      )

instance Prelude.Hashable CsvOutputOptions where
  hashWithSalt _salt CsvOutputOptions' {..} =
    _salt `Prelude.hashWithSalt` delimiter

instance Prelude.NFData CsvOutputOptions where
  rnf CsvOutputOptions' {..} = Prelude.rnf delimiter

instance Data.ToJSON CsvOutputOptions where
  toJSON CsvOutputOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Delimiter" Data..=) Prelude.<$> delimiter]
      )
