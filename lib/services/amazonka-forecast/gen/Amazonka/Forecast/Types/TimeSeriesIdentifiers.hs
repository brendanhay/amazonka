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
-- Module      : Amazonka.Forecast.Types.TimeSeriesIdentifiers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.TimeSeriesIdentifiers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.DataSource
import Amazonka.Forecast.Types.Schema
import qualified Amazonka.Prelude as Prelude

-- | Details about the import file that contains the time series for which
-- you want to create forecasts.
--
-- /See:/ 'newTimeSeriesIdentifiers' smart constructor.
data TimeSeriesIdentifiers = TimeSeriesIdentifiers'
  { dataSource :: Prelude.Maybe DataSource,
    -- | The format of the data, either CSV or PARQUET.
    format :: Prelude.Maybe Prelude.Text,
    schema :: Prelude.Maybe Schema
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSeriesIdentifiers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'timeSeriesIdentifiers_dataSource' - Undocumented member.
--
-- 'format', 'timeSeriesIdentifiers_format' - The format of the data, either CSV or PARQUET.
--
-- 'schema', 'timeSeriesIdentifiers_schema' - Undocumented member.
newTimeSeriesIdentifiers ::
  TimeSeriesIdentifiers
newTimeSeriesIdentifiers =
  TimeSeriesIdentifiers'
    { dataSource =
        Prelude.Nothing,
      format = Prelude.Nothing,
      schema = Prelude.Nothing
    }

-- | Undocumented member.
timeSeriesIdentifiers_dataSource :: Lens.Lens' TimeSeriesIdentifiers (Prelude.Maybe DataSource)
timeSeriesIdentifiers_dataSource = Lens.lens (\TimeSeriesIdentifiers' {dataSource} -> dataSource) (\s@TimeSeriesIdentifiers' {} a -> s {dataSource = a} :: TimeSeriesIdentifiers)

-- | The format of the data, either CSV or PARQUET.
timeSeriesIdentifiers_format :: Lens.Lens' TimeSeriesIdentifiers (Prelude.Maybe Prelude.Text)
timeSeriesIdentifiers_format = Lens.lens (\TimeSeriesIdentifiers' {format} -> format) (\s@TimeSeriesIdentifiers' {} a -> s {format = a} :: TimeSeriesIdentifiers)

-- | Undocumented member.
timeSeriesIdentifiers_schema :: Lens.Lens' TimeSeriesIdentifiers (Prelude.Maybe Schema)
timeSeriesIdentifiers_schema = Lens.lens (\TimeSeriesIdentifiers' {schema} -> schema) (\s@TimeSeriesIdentifiers' {} a -> s {schema = a} :: TimeSeriesIdentifiers)

instance Data.FromJSON TimeSeriesIdentifiers where
  parseJSON =
    Data.withObject
      "TimeSeriesIdentifiers"
      ( \x ->
          TimeSeriesIdentifiers'
            Prelude.<$> (x Data..:? "DataSource")
            Prelude.<*> (x Data..:? "Format")
            Prelude.<*> (x Data..:? "Schema")
      )

instance Prelude.Hashable TimeSeriesIdentifiers where
  hashWithSalt _salt TimeSeriesIdentifiers' {..} =
    _salt
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` schema

instance Prelude.NFData TimeSeriesIdentifiers where
  rnf TimeSeriesIdentifiers' {..} =
    Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf schema

instance Data.ToJSON TimeSeriesIdentifiers where
  toJSON TimeSeriesIdentifiers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSource" Data..=) Prelude.<$> dataSource,
            ("Format" Data..=) Prelude.<$> format,
            ("Schema" Data..=) Prelude.<$> schema
          ]
      )
