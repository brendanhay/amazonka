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
-- Module      : Amazonka.LookoutMetrics.Types.TimestampColumn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.TimestampColumn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the column used to track time in a source
-- data file.
--
-- /See:/ 'newTimestampColumn' smart constructor.
data TimestampColumn = TimestampColumn'
  { -- | The format of the timestamp column.
    columnFormat :: Prelude.Maybe Prelude.Text,
    -- | The name of the timestamp column.
    columnName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestampColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnFormat', 'timestampColumn_columnFormat' - The format of the timestamp column.
--
-- 'columnName', 'timestampColumn_columnName' - The name of the timestamp column.
newTimestampColumn ::
  TimestampColumn
newTimestampColumn =
  TimestampColumn'
    { columnFormat = Prelude.Nothing,
      columnName = Prelude.Nothing
    }

-- | The format of the timestamp column.
timestampColumn_columnFormat :: Lens.Lens' TimestampColumn (Prelude.Maybe Prelude.Text)
timestampColumn_columnFormat = Lens.lens (\TimestampColumn' {columnFormat} -> columnFormat) (\s@TimestampColumn' {} a -> s {columnFormat = a} :: TimestampColumn)

-- | The name of the timestamp column.
timestampColumn_columnName :: Lens.Lens' TimestampColumn (Prelude.Maybe Prelude.Text)
timestampColumn_columnName = Lens.lens (\TimestampColumn' {columnName} -> columnName) (\s@TimestampColumn' {} a -> s {columnName = a} :: TimestampColumn)

instance Data.FromJSON TimestampColumn where
  parseJSON =
    Data.withObject
      "TimestampColumn"
      ( \x ->
          TimestampColumn'
            Prelude.<$> (x Data..:? "ColumnFormat")
            Prelude.<*> (x Data..:? "ColumnName")
      )

instance Prelude.Hashable TimestampColumn where
  hashWithSalt _salt TimestampColumn' {..} =
    _salt
      `Prelude.hashWithSalt` columnFormat
      `Prelude.hashWithSalt` columnName

instance Prelude.NFData TimestampColumn where
  rnf TimestampColumn' {..} =
    Prelude.rnf columnFormat `Prelude.seq`
      Prelude.rnf columnName

instance Data.ToJSON TimestampColumn where
  toJSON TimestampColumn' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnFormat" Data..=) Prelude.<$> columnFormat,
            ("ColumnName" Data..=) Prelude.<$> columnName
          ]
      )
