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
-- Module      : Amazonka.Forecast.Types.MonitorDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.MonitorDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The source of the data the monitor used during the evaluation.
--
-- /See:/ 'newMonitorDataSource' smart constructor.
data MonitorDataSource = MonitorDataSource'
  { -- | The Amazon Resource Name (ARN) of the dataset import job used to import
    -- the data that initiated the monitor evaluation.
    datasetImportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the forecast the monitor used during
    -- the evaluation.
    forecastArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the predictor resource you are
    -- monitoring.
    predictorArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitorDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetImportJobArn', 'monitorDataSource_datasetImportJobArn' - The Amazon Resource Name (ARN) of the dataset import job used to import
-- the data that initiated the monitor evaluation.
--
-- 'forecastArn', 'monitorDataSource_forecastArn' - The Amazon Resource Name (ARN) of the forecast the monitor used during
-- the evaluation.
--
-- 'predictorArn', 'monitorDataSource_predictorArn' - The Amazon Resource Name (ARN) of the predictor resource you are
-- monitoring.
newMonitorDataSource ::
  MonitorDataSource
newMonitorDataSource =
  MonitorDataSource'
    { datasetImportJobArn =
        Prelude.Nothing,
      forecastArn = Prelude.Nothing,
      predictorArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the dataset import job used to import
-- the data that initiated the monitor evaluation.
monitorDataSource_datasetImportJobArn :: Lens.Lens' MonitorDataSource (Prelude.Maybe Prelude.Text)
monitorDataSource_datasetImportJobArn = Lens.lens (\MonitorDataSource' {datasetImportJobArn} -> datasetImportJobArn) (\s@MonitorDataSource' {} a -> s {datasetImportJobArn = a} :: MonitorDataSource)

-- | The Amazon Resource Name (ARN) of the forecast the monitor used during
-- the evaluation.
monitorDataSource_forecastArn :: Lens.Lens' MonitorDataSource (Prelude.Maybe Prelude.Text)
monitorDataSource_forecastArn = Lens.lens (\MonitorDataSource' {forecastArn} -> forecastArn) (\s@MonitorDataSource' {} a -> s {forecastArn = a} :: MonitorDataSource)

-- | The Amazon Resource Name (ARN) of the predictor resource you are
-- monitoring.
monitorDataSource_predictorArn :: Lens.Lens' MonitorDataSource (Prelude.Maybe Prelude.Text)
monitorDataSource_predictorArn = Lens.lens (\MonitorDataSource' {predictorArn} -> predictorArn) (\s@MonitorDataSource' {} a -> s {predictorArn = a} :: MonitorDataSource)

instance Data.FromJSON MonitorDataSource where
  parseJSON =
    Data.withObject
      "MonitorDataSource"
      ( \x ->
          MonitorDataSource'
            Prelude.<$> (x Data..:? "DatasetImportJobArn")
            Prelude.<*> (x Data..:? "ForecastArn")
            Prelude.<*> (x Data..:? "PredictorArn")
      )

instance Prelude.Hashable MonitorDataSource where
  hashWithSalt _salt MonitorDataSource' {..} =
    _salt `Prelude.hashWithSalt` datasetImportJobArn
      `Prelude.hashWithSalt` forecastArn
      `Prelude.hashWithSalt` predictorArn

instance Prelude.NFData MonitorDataSource where
  rnf MonitorDataSource' {..} =
    Prelude.rnf datasetImportJobArn
      `Prelude.seq` Prelude.rnf forecastArn
      `Prelude.seq` Prelude.rnf predictorArn
