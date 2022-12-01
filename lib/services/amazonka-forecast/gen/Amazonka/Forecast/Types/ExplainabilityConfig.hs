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
-- Module      : Amazonka.Forecast.Types.ExplainabilityConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.ExplainabilityConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types.TimePointGranularity
import Amazonka.Forecast.Types.TimeSeriesGranularity
import qualified Amazonka.Prelude as Prelude

-- | The ExplainabilityConfig data type defines the number of time series and
-- time points included in CreateExplainability.
--
-- If you provide a predictor ARN for @ResourceArn@, you must set both
-- @TimePointGranularity@ and @TimeSeriesGranularity@ to “ALL”. When
-- creating Predictor Explainability, Amazon Forecast considers all time
-- series and time points.
--
-- If you provide a forecast ARN for @ResourceArn@, you can set
-- @TimePointGranularity@ and @TimeSeriesGranularity@ to either “ALL” or
-- “Specific”.
--
-- /See:/ 'newExplainabilityConfig' smart constructor.
data ExplainabilityConfig = ExplainabilityConfig'
  { -- | To create an Explainability for all time series in your datasets, use
    -- @ALL@. To create an Explainability for specific time series in your
    -- datasets, use @SPECIFIC@.
    --
    -- Specify time series by uploading a CSV or Parquet file to an Amazon S3
    -- bucket and set the location within the DataDestination data type.
    timeSeriesGranularity :: TimeSeriesGranularity,
    -- | To create an Explainability for all time points in your forecast
    -- horizon, use @ALL@. To create an Explainability for specific time points
    -- in your forecast horizon, use @SPECIFIC@.
    --
    -- Specify time points with the @StartDateTime@ and @EndDateTime@
    -- parameters within the CreateExplainability operation.
    timePointGranularity :: TimePointGranularity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExplainabilityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeSeriesGranularity', 'explainabilityConfig_timeSeriesGranularity' - To create an Explainability for all time series in your datasets, use
-- @ALL@. To create an Explainability for specific time series in your
-- datasets, use @SPECIFIC@.
--
-- Specify time series by uploading a CSV or Parquet file to an Amazon S3
-- bucket and set the location within the DataDestination data type.
--
-- 'timePointGranularity', 'explainabilityConfig_timePointGranularity' - To create an Explainability for all time points in your forecast
-- horizon, use @ALL@. To create an Explainability for specific time points
-- in your forecast horizon, use @SPECIFIC@.
--
-- Specify time points with the @StartDateTime@ and @EndDateTime@
-- parameters within the CreateExplainability operation.
newExplainabilityConfig ::
  -- | 'timeSeriesGranularity'
  TimeSeriesGranularity ->
  -- | 'timePointGranularity'
  TimePointGranularity ->
  ExplainabilityConfig
newExplainabilityConfig
  pTimeSeriesGranularity_
  pTimePointGranularity_ =
    ExplainabilityConfig'
      { timeSeriesGranularity =
          pTimeSeriesGranularity_,
        timePointGranularity = pTimePointGranularity_
      }

-- | To create an Explainability for all time series in your datasets, use
-- @ALL@. To create an Explainability for specific time series in your
-- datasets, use @SPECIFIC@.
--
-- Specify time series by uploading a CSV or Parquet file to an Amazon S3
-- bucket and set the location within the DataDestination data type.
explainabilityConfig_timeSeriesGranularity :: Lens.Lens' ExplainabilityConfig TimeSeriesGranularity
explainabilityConfig_timeSeriesGranularity = Lens.lens (\ExplainabilityConfig' {timeSeriesGranularity} -> timeSeriesGranularity) (\s@ExplainabilityConfig' {} a -> s {timeSeriesGranularity = a} :: ExplainabilityConfig)

-- | To create an Explainability for all time points in your forecast
-- horizon, use @ALL@. To create an Explainability for specific time points
-- in your forecast horizon, use @SPECIFIC@.
--
-- Specify time points with the @StartDateTime@ and @EndDateTime@
-- parameters within the CreateExplainability operation.
explainabilityConfig_timePointGranularity :: Lens.Lens' ExplainabilityConfig TimePointGranularity
explainabilityConfig_timePointGranularity = Lens.lens (\ExplainabilityConfig' {timePointGranularity} -> timePointGranularity) (\s@ExplainabilityConfig' {} a -> s {timePointGranularity = a} :: ExplainabilityConfig)

instance Core.FromJSON ExplainabilityConfig where
  parseJSON =
    Core.withObject
      "ExplainabilityConfig"
      ( \x ->
          ExplainabilityConfig'
            Prelude.<$> (x Core..: "TimeSeriesGranularity")
            Prelude.<*> (x Core..: "TimePointGranularity")
      )

instance Prelude.Hashable ExplainabilityConfig where
  hashWithSalt _salt ExplainabilityConfig' {..} =
    _salt `Prelude.hashWithSalt` timeSeriesGranularity
      `Prelude.hashWithSalt` timePointGranularity

instance Prelude.NFData ExplainabilityConfig where
  rnf ExplainabilityConfig' {..} =
    Prelude.rnf timeSeriesGranularity
      `Prelude.seq` Prelude.rnf timePointGranularity

instance Core.ToJSON ExplainabilityConfig where
  toJSON ExplainabilityConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TimeSeriesGranularity"
                  Core..= timeSeriesGranularity
              ),
            Prelude.Just
              ( "TimePointGranularity"
                  Core..= timePointGranularity
              )
          ]
      )
