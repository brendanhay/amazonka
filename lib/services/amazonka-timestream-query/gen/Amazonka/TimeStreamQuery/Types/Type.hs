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
-- Module      : Amazonka.TimeStreamQuery.Types.Type
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.Type where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import {-# SOURCE #-} Amazonka.TimeStreamQuery.Types.ColumnInfo
import Amazonka.TimeStreamQuery.Types.ScalarType

-- | Contains the data type of a column in a query result set. The data type
-- can be scalar or complex. The supported scalar data types are integers,
-- Boolean, string, double, timestamp, date, time, and intervals. The
-- supported complex data types are arrays, rows, and timeseries.
--
-- /See:/ 'newType' smart constructor.
data Type = Type'
  { -- | Indicates if the column is an array.
    arrayColumnInfo :: Prelude.Maybe ColumnInfo,
    -- | Indicates if the column is a row.
    rowColumnInfo :: Prelude.Maybe [ColumnInfo],
    -- | Indicates if the column is of type string, integer, Boolean, double,
    -- timestamp, date, time.
    scalarType :: Prelude.Maybe ScalarType,
    -- | Indicates if the column is a timeseries data type.
    timeSeriesMeasureValueColumnInfo :: Prelude.Maybe ColumnInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Type' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arrayColumnInfo', 'type_arrayColumnInfo' - Indicates if the column is an array.
--
-- 'rowColumnInfo', 'type_rowColumnInfo' - Indicates if the column is a row.
--
-- 'scalarType', 'type_scalarType' - Indicates if the column is of type string, integer, Boolean, double,
-- timestamp, date, time.
--
-- 'timeSeriesMeasureValueColumnInfo', 'type_timeSeriesMeasureValueColumnInfo' - Indicates if the column is a timeseries data type.
newType ::
  Type
newType =
  Type'
    { arrayColumnInfo = Prelude.Nothing,
      rowColumnInfo = Prelude.Nothing,
      scalarType = Prelude.Nothing,
      timeSeriesMeasureValueColumnInfo = Prelude.Nothing
    }

-- | Indicates if the column is an array.
type_arrayColumnInfo :: Lens.Lens' Type (Prelude.Maybe ColumnInfo)
type_arrayColumnInfo = Lens.lens (\Type' {arrayColumnInfo} -> arrayColumnInfo) (\s@Type' {} a -> s {arrayColumnInfo = a} :: Type)

-- | Indicates if the column is a row.
type_rowColumnInfo :: Lens.Lens' Type (Prelude.Maybe [ColumnInfo])
type_rowColumnInfo = Lens.lens (\Type' {rowColumnInfo} -> rowColumnInfo) (\s@Type' {} a -> s {rowColumnInfo = a} :: Type) Prelude.. Lens.mapping Lens.coerced

-- | Indicates if the column is of type string, integer, Boolean, double,
-- timestamp, date, time.
type_scalarType :: Lens.Lens' Type (Prelude.Maybe ScalarType)
type_scalarType = Lens.lens (\Type' {scalarType} -> scalarType) (\s@Type' {} a -> s {scalarType = a} :: Type)

-- | Indicates if the column is a timeseries data type.
type_timeSeriesMeasureValueColumnInfo :: Lens.Lens' Type (Prelude.Maybe ColumnInfo)
type_timeSeriesMeasureValueColumnInfo = Lens.lens (\Type' {timeSeriesMeasureValueColumnInfo} -> timeSeriesMeasureValueColumnInfo) (\s@Type' {} a -> s {timeSeriesMeasureValueColumnInfo = a} :: Type)

instance Data.FromJSON Type where
  parseJSON =
    Data.withObject
      "Type"
      ( \x ->
          Type'
            Prelude.<$> (x Data..:? "ArrayColumnInfo")
            Prelude.<*> (x Data..:? "RowColumnInfo" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ScalarType")
            Prelude.<*> (x Data..:? "TimeSeriesMeasureValueColumnInfo")
      )

instance Prelude.Hashable Type where
  hashWithSalt _salt Type' {..} =
    _salt
      `Prelude.hashWithSalt` arrayColumnInfo
      `Prelude.hashWithSalt` rowColumnInfo
      `Prelude.hashWithSalt` scalarType
      `Prelude.hashWithSalt` timeSeriesMeasureValueColumnInfo

instance Prelude.NFData Type where
  rnf Type' {..} =
    Prelude.rnf arrayColumnInfo
      `Prelude.seq` Prelude.rnf rowColumnInfo
      `Prelude.seq` Prelude.rnf scalarType
      `Prelude.seq` Prelude.rnf timeSeriesMeasureValueColumnInfo
