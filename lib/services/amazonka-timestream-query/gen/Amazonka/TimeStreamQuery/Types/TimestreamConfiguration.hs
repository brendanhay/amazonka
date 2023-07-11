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
-- Module      : Amazonka.TimeStreamQuery.Types.TimestreamConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.TimestreamConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.DimensionMapping
import Amazonka.TimeStreamQuery.Types.MixedMeasureMapping
import Amazonka.TimeStreamQuery.Types.MultiMeasureMappings

-- | Configuration to write data into Timestream database and table. This
-- configuration allows the user to map the query result select columns
-- into the destination table columns.
--
-- /See:/ 'newTimestreamConfiguration' smart constructor.
data TimestreamConfiguration = TimestreamConfiguration'
  { -- | Name of the measure column.
    measureNameColumn :: Prelude.Maybe Prelude.Text,
    -- | Specifies how to map measures to multi-measure records.
    mixedMeasureMappings :: Prelude.Maybe (Prelude.NonEmpty MixedMeasureMapping),
    -- | Multi-measure mappings.
    multiMeasureMappings :: Prelude.Maybe MultiMeasureMappings,
    -- | Name of Timestream database to which the query result will be written.
    databaseName :: Prelude.Text,
    -- | Name of Timestream table that the query result will be written to. The
    -- table should be within the same database that is provided in Timestream
    -- configuration.
    tableName :: Prelude.Text,
    -- | Column from query result that should be used as the time column in
    -- destination table. Column type for this should be TIMESTAMP.
    timeColumn :: Prelude.Text,
    -- | This is to allow mapping column(s) from the query result to the
    -- dimension in the destination table.
    dimensionMappings :: [DimensionMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestreamConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'measureNameColumn', 'timestreamConfiguration_measureNameColumn' - Name of the measure column.
--
-- 'mixedMeasureMappings', 'timestreamConfiguration_mixedMeasureMappings' - Specifies how to map measures to multi-measure records.
--
-- 'multiMeasureMappings', 'timestreamConfiguration_multiMeasureMappings' - Multi-measure mappings.
--
-- 'databaseName', 'timestreamConfiguration_databaseName' - Name of Timestream database to which the query result will be written.
--
-- 'tableName', 'timestreamConfiguration_tableName' - Name of Timestream table that the query result will be written to. The
-- table should be within the same database that is provided in Timestream
-- configuration.
--
-- 'timeColumn', 'timestreamConfiguration_timeColumn' - Column from query result that should be used as the time column in
-- destination table. Column type for this should be TIMESTAMP.
--
-- 'dimensionMappings', 'timestreamConfiguration_dimensionMappings' - This is to allow mapping column(s) from the query result to the
-- dimension in the destination table.
newTimestreamConfiguration ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'timeColumn'
  Prelude.Text ->
  TimestreamConfiguration
newTimestreamConfiguration
  pDatabaseName_
  pTableName_
  pTimeColumn_ =
    TimestreamConfiguration'
      { measureNameColumn =
          Prelude.Nothing,
        mixedMeasureMappings = Prelude.Nothing,
        multiMeasureMappings = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        timeColumn = pTimeColumn_,
        dimensionMappings = Prelude.mempty
      }

-- | Name of the measure column.
timestreamConfiguration_measureNameColumn :: Lens.Lens' TimestreamConfiguration (Prelude.Maybe Prelude.Text)
timestreamConfiguration_measureNameColumn = Lens.lens (\TimestreamConfiguration' {measureNameColumn} -> measureNameColumn) (\s@TimestreamConfiguration' {} a -> s {measureNameColumn = a} :: TimestreamConfiguration)

-- | Specifies how to map measures to multi-measure records.
timestreamConfiguration_mixedMeasureMappings :: Lens.Lens' TimestreamConfiguration (Prelude.Maybe (Prelude.NonEmpty MixedMeasureMapping))
timestreamConfiguration_mixedMeasureMappings = Lens.lens (\TimestreamConfiguration' {mixedMeasureMappings} -> mixedMeasureMappings) (\s@TimestreamConfiguration' {} a -> s {mixedMeasureMappings = a} :: TimestreamConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Multi-measure mappings.
timestreamConfiguration_multiMeasureMappings :: Lens.Lens' TimestreamConfiguration (Prelude.Maybe MultiMeasureMappings)
timestreamConfiguration_multiMeasureMappings = Lens.lens (\TimestreamConfiguration' {multiMeasureMappings} -> multiMeasureMappings) (\s@TimestreamConfiguration' {} a -> s {multiMeasureMappings = a} :: TimestreamConfiguration)

-- | Name of Timestream database to which the query result will be written.
timestreamConfiguration_databaseName :: Lens.Lens' TimestreamConfiguration Prelude.Text
timestreamConfiguration_databaseName = Lens.lens (\TimestreamConfiguration' {databaseName} -> databaseName) (\s@TimestreamConfiguration' {} a -> s {databaseName = a} :: TimestreamConfiguration)

-- | Name of Timestream table that the query result will be written to. The
-- table should be within the same database that is provided in Timestream
-- configuration.
timestreamConfiguration_tableName :: Lens.Lens' TimestreamConfiguration Prelude.Text
timestreamConfiguration_tableName = Lens.lens (\TimestreamConfiguration' {tableName} -> tableName) (\s@TimestreamConfiguration' {} a -> s {tableName = a} :: TimestreamConfiguration)

-- | Column from query result that should be used as the time column in
-- destination table. Column type for this should be TIMESTAMP.
timestreamConfiguration_timeColumn :: Lens.Lens' TimestreamConfiguration Prelude.Text
timestreamConfiguration_timeColumn = Lens.lens (\TimestreamConfiguration' {timeColumn} -> timeColumn) (\s@TimestreamConfiguration' {} a -> s {timeColumn = a} :: TimestreamConfiguration)

-- | This is to allow mapping column(s) from the query result to the
-- dimension in the destination table.
timestreamConfiguration_dimensionMappings :: Lens.Lens' TimestreamConfiguration [DimensionMapping]
timestreamConfiguration_dimensionMappings = Lens.lens (\TimestreamConfiguration' {dimensionMappings} -> dimensionMappings) (\s@TimestreamConfiguration' {} a -> s {dimensionMappings = a} :: TimestreamConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON TimestreamConfiguration where
  parseJSON =
    Data.withObject
      "TimestreamConfiguration"
      ( \x ->
          TimestreamConfiguration'
            Prelude.<$> (x Data..:? "MeasureNameColumn")
            Prelude.<*> (x Data..:? "MixedMeasureMappings")
            Prelude.<*> (x Data..:? "MultiMeasureMappings")
            Prelude.<*> (x Data..: "DatabaseName")
            Prelude.<*> (x Data..: "TableName")
            Prelude.<*> (x Data..: "TimeColumn")
            Prelude.<*> ( x
                            Data..:? "DimensionMappings"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TimestreamConfiguration where
  hashWithSalt _salt TimestreamConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` measureNameColumn
      `Prelude.hashWithSalt` mixedMeasureMappings
      `Prelude.hashWithSalt` multiMeasureMappings
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` timeColumn
      `Prelude.hashWithSalt` dimensionMappings

instance Prelude.NFData TimestreamConfiguration where
  rnf TimestreamConfiguration' {..} =
    Prelude.rnf measureNameColumn
      `Prelude.seq` Prelude.rnf mixedMeasureMappings
      `Prelude.seq` Prelude.rnf multiMeasureMappings
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf timeColumn
      `Prelude.seq` Prelude.rnf dimensionMappings

instance Data.ToJSON TimestreamConfiguration where
  toJSON TimestreamConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MeasureNameColumn" Data..=)
              Prelude.<$> measureNameColumn,
            ("MixedMeasureMappings" Data..=)
              Prelude.<$> mixedMeasureMappings,
            ("MultiMeasureMappings" Data..=)
              Prelude.<$> multiMeasureMappings,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("TimeColumn" Data..= timeColumn),
            Prelude.Just
              ("DimensionMappings" Data..= dimensionMappings)
          ]
      )
