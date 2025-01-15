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
-- Module      : Amazonka.Forecast.Types.TimeSeriesReplacementsDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.TimeSeriesReplacementsDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.S3Config
import Amazonka.Forecast.Types.Schema
import qualified Amazonka.Prelude as Prelude

-- | A replacement dataset is a modified version of the baseline related time
-- series that contains only the values that you want to include in a
-- what-if forecast. The replacement dataset must contain the forecast
-- dimensions and item identifiers in the baseline related time series as
-- well as at least 1 changed time series. This dataset is merged with the
-- baseline related time series to create a transformed dataset that is
-- used for the what-if forecast.
--
-- /See:/ 'newTimeSeriesReplacementsDataSource' smart constructor.
data TimeSeriesReplacementsDataSource = TimeSeriesReplacementsDataSource'
  { -- | The format of the replacement data, CSV or PARQUET.
    format :: Prelude.Maybe Prelude.Text,
    -- | The timestamp format of the replacement data.
    timestampFormat :: Prelude.Maybe Prelude.Text,
    s3Config :: S3Config,
    schema :: Schema
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSeriesReplacementsDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'timeSeriesReplacementsDataSource_format' - The format of the replacement data, CSV or PARQUET.
--
-- 'timestampFormat', 'timeSeriesReplacementsDataSource_timestampFormat' - The timestamp format of the replacement data.
--
-- 's3Config', 'timeSeriesReplacementsDataSource_s3Config' - Undocumented member.
--
-- 'schema', 'timeSeriesReplacementsDataSource_schema' - Undocumented member.
newTimeSeriesReplacementsDataSource ::
  -- | 's3Config'
  S3Config ->
  -- | 'schema'
  Schema ->
  TimeSeriesReplacementsDataSource
newTimeSeriesReplacementsDataSource
  pS3Config_
  pSchema_ =
    TimeSeriesReplacementsDataSource'
      { format =
          Prelude.Nothing,
        timestampFormat = Prelude.Nothing,
        s3Config = pS3Config_,
        schema = pSchema_
      }

-- | The format of the replacement data, CSV or PARQUET.
timeSeriesReplacementsDataSource_format :: Lens.Lens' TimeSeriesReplacementsDataSource (Prelude.Maybe Prelude.Text)
timeSeriesReplacementsDataSource_format = Lens.lens (\TimeSeriesReplacementsDataSource' {format} -> format) (\s@TimeSeriesReplacementsDataSource' {} a -> s {format = a} :: TimeSeriesReplacementsDataSource)

-- | The timestamp format of the replacement data.
timeSeriesReplacementsDataSource_timestampFormat :: Lens.Lens' TimeSeriesReplacementsDataSource (Prelude.Maybe Prelude.Text)
timeSeriesReplacementsDataSource_timestampFormat = Lens.lens (\TimeSeriesReplacementsDataSource' {timestampFormat} -> timestampFormat) (\s@TimeSeriesReplacementsDataSource' {} a -> s {timestampFormat = a} :: TimeSeriesReplacementsDataSource)

-- | Undocumented member.
timeSeriesReplacementsDataSource_s3Config :: Lens.Lens' TimeSeriesReplacementsDataSource S3Config
timeSeriesReplacementsDataSource_s3Config = Lens.lens (\TimeSeriesReplacementsDataSource' {s3Config} -> s3Config) (\s@TimeSeriesReplacementsDataSource' {} a -> s {s3Config = a} :: TimeSeriesReplacementsDataSource)

-- | Undocumented member.
timeSeriesReplacementsDataSource_schema :: Lens.Lens' TimeSeriesReplacementsDataSource Schema
timeSeriesReplacementsDataSource_schema = Lens.lens (\TimeSeriesReplacementsDataSource' {schema} -> schema) (\s@TimeSeriesReplacementsDataSource' {} a -> s {schema = a} :: TimeSeriesReplacementsDataSource)

instance
  Data.FromJSON
    TimeSeriesReplacementsDataSource
  where
  parseJSON =
    Data.withObject
      "TimeSeriesReplacementsDataSource"
      ( \x ->
          TimeSeriesReplacementsDataSource'
            Prelude.<$> (x Data..:? "Format")
            Prelude.<*> (x Data..:? "TimestampFormat")
            Prelude.<*> (x Data..: "S3Config")
            Prelude.<*> (x Data..: "Schema")
      )

instance
  Prelude.Hashable
    TimeSeriesReplacementsDataSource
  where
  hashWithSalt
    _salt
    TimeSeriesReplacementsDataSource' {..} =
      _salt
        `Prelude.hashWithSalt` format
        `Prelude.hashWithSalt` timestampFormat
        `Prelude.hashWithSalt` s3Config
        `Prelude.hashWithSalt` schema

instance
  Prelude.NFData
    TimeSeriesReplacementsDataSource
  where
  rnf TimeSeriesReplacementsDataSource' {..} =
    Prelude.rnf format `Prelude.seq`
      Prelude.rnf timestampFormat `Prelude.seq`
        Prelude.rnf s3Config `Prelude.seq`
          Prelude.rnf schema

instance Data.ToJSON TimeSeriesReplacementsDataSource where
  toJSON TimeSeriesReplacementsDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Format" Data..=) Prelude.<$> format,
            ("TimestampFormat" Data..=)
              Prelude.<$> timestampFormat,
            Prelude.Just ("S3Config" Data..= s3Config),
            Prelude.Just ("Schema" Data..= schema)
          ]
      )
