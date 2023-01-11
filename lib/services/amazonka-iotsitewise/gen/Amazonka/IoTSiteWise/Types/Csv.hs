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
-- Module      : Amazonka.IoTSiteWise.Types.Csv
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Csv where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.ColumnName
import qualified Amazonka.Prelude as Prelude

-- | A .csv file.
--
-- /See:/ 'newCsv' smart constructor.
data Csv = Csv'
  { -- | The column names specified in the .csv file.
    columnNames :: Prelude.Maybe [ColumnName]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Csv' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnNames', 'csv_columnNames' - The column names specified in the .csv file.
newCsv ::
  Csv
newCsv = Csv' {columnNames = Prelude.Nothing}

-- | The column names specified in the .csv file.
csv_columnNames :: Lens.Lens' Csv (Prelude.Maybe [ColumnName])
csv_columnNames = Lens.lens (\Csv' {columnNames} -> columnNames) (\s@Csv' {} a -> s {columnNames = a} :: Csv) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Csv where
  parseJSON =
    Data.withObject
      "Csv"
      ( \x ->
          Csv'
            Prelude.<$> (x Data..:? "columnNames" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Csv where
  hashWithSalt _salt Csv' {..} =
    _salt `Prelude.hashWithSalt` columnNames

instance Prelude.NFData Csv where
  rnf Csv' {..} = Prelude.rnf columnNames

instance Data.ToJSON Csv where
  toJSON Csv' {..} =
    Data.object
      ( Prelude.catMaybes
          [("columnNames" Data..=) Prelude.<$> columnNames]
      )
