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
-- Module      : Amazonka.TimeStreamQuery.Types.Datum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.Datum where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import {-# SOURCE #-} Amazonka.TimeStreamQuery.Types.Row
import {-# SOURCE #-} Amazonka.TimeStreamQuery.Types.TimeSeriesDataPoint

-- | Datum represents a single data point in a query result.
--
-- /See:/ 'newDatum' smart constructor.
data Datum = Datum'
  { -- | Indicates if the data point is an array.
    arrayValue :: Prelude.Maybe [Datum],
    -- | Indicates if the data point is null.
    nullValue :: Prelude.Maybe Prelude.Bool,
    -- | Indicates if the data point is a row.
    rowValue :: Prelude.Maybe Row,
    -- | Indicates if the data point is a scalar value such as integer, string,
    -- double, or Boolean.
    scalarValue :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the data point is a timeseries data type.
    timeSeriesValue :: Prelude.Maybe [TimeSeriesDataPoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Datum' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arrayValue', 'datum_arrayValue' - Indicates if the data point is an array.
--
-- 'nullValue', 'datum_nullValue' - Indicates if the data point is null.
--
-- 'rowValue', 'datum_rowValue' - Indicates if the data point is a row.
--
-- 'scalarValue', 'datum_scalarValue' - Indicates if the data point is a scalar value such as integer, string,
-- double, or Boolean.
--
-- 'timeSeriesValue', 'datum_timeSeriesValue' - Indicates if the data point is a timeseries data type.
newDatum ::
  Datum
newDatum =
  Datum'
    { arrayValue = Prelude.Nothing,
      nullValue = Prelude.Nothing,
      rowValue = Prelude.Nothing,
      scalarValue = Prelude.Nothing,
      timeSeriesValue = Prelude.Nothing
    }

-- | Indicates if the data point is an array.
datum_arrayValue :: Lens.Lens' Datum (Prelude.Maybe [Datum])
datum_arrayValue = Lens.lens (\Datum' {arrayValue} -> arrayValue) (\s@Datum' {} a -> s {arrayValue = a} :: Datum) Prelude.. Lens.mapping Lens.coerced

-- | Indicates if the data point is null.
datum_nullValue :: Lens.Lens' Datum (Prelude.Maybe Prelude.Bool)
datum_nullValue = Lens.lens (\Datum' {nullValue} -> nullValue) (\s@Datum' {} a -> s {nullValue = a} :: Datum)

-- | Indicates if the data point is a row.
datum_rowValue :: Lens.Lens' Datum (Prelude.Maybe Row)
datum_rowValue = Lens.lens (\Datum' {rowValue} -> rowValue) (\s@Datum' {} a -> s {rowValue = a} :: Datum)

-- | Indicates if the data point is a scalar value such as integer, string,
-- double, or Boolean.
datum_scalarValue :: Lens.Lens' Datum (Prelude.Maybe Prelude.Text)
datum_scalarValue = Lens.lens (\Datum' {scalarValue} -> scalarValue) (\s@Datum' {} a -> s {scalarValue = a} :: Datum)

-- | Indicates if the data point is a timeseries data type.
datum_timeSeriesValue :: Lens.Lens' Datum (Prelude.Maybe [TimeSeriesDataPoint])
datum_timeSeriesValue = Lens.lens (\Datum' {timeSeriesValue} -> timeSeriesValue) (\s@Datum' {} a -> s {timeSeriesValue = a} :: Datum) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Datum where
  parseJSON =
    Data.withObject
      "Datum"
      ( \x ->
          Datum'
            Prelude.<$> (x Data..:? "ArrayValue" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NullValue")
            Prelude.<*> (x Data..:? "RowValue")
            Prelude.<*> (x Data..:? "ScalarValue")
            Prelude.<*> ( x
                            Data..:? "TimeSeriesValue"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Datum where
  hashWithSalt _salt Datum' {..} =
    _salt
      `Prelude.hashWithSalt` arrayValue
      `Prelude.hashWithSalt` nullValue
      `Prelude.hashWithSalt` rowValue
      `Prelude.hashWithSalt` scalarValue
      `Prelude.hashWithSalt` timeSeriesValue

instance Prelude.NFData Datum where
  rnf Datum' {..} =
    Prelude.rnf arrayValue
      `Prelude.seq` Prelude.rnf nullValue
      `Prelude.seq` Prelude.rnf rowValue
      `Prelude.seq` Prelude.rnf scalarValue
      `Prelude.seq` Prelude.rnf timeSeriesValue
