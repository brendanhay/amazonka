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
-- Module      : Amazonka.SageMaker.Types.TimeSeriesForecastingSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TimeSeriesForecastingSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FeatureStatus

-- | Time series forecast settings for the SageMaker Canvas app.
--
-- /See:/ 'newTimeSeriesForecastingSettings' smart constructor.
data TimeSeriesForecastingSettings = TimeSeriesForecastingSettings'
  { -- | Describes whether time series forecasting is enabled or disabled in the
    -- Canvas app.
    status :: Prelude.Maybe FeatureStatus,
    -- | The IAM role that Canvas passes to Amazon Forecast for time series
    -- forecasting. By default, Canvas uses the execution role specified in the
    -- @UserProfile@ that launches the Canvas app. If an execution role is not
    -- specified in the @UserProfile@, Canvas uses the execution role specified
    -- in the Domain that owns the @UserProfile@. To allow time series
    -- forecasting, this IAM role should have the
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/security-iam-awsmanpol-canvas.html#security-iam-awsmanpol-AmazonSageMakerCanvasForecastAccess AmazonSageMakerCanvasForecastAccess>
    -- policy attached and @forecast.amazonaws.com@ added in the trust
    -- relationship as a service principal.
    amazonForecastRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSeriesForecastingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'timeSeriesForecastingSettings_status' - Describes whether time series forecasting is enabled or disabled in the
-- Canvas app.
--
-- 'amazonForecastRoleArn', 'timeSeriesForecastingSettings_amazonForecastRoleArn' - The IAM role that Canvas passes to Amazon Forecast for time series
-- forecasting. By default, Canvas uses the execution role specified in the
-- @UserProfile@ that launches the Canvas app. If an execution role is not
-- specified in the @UserProfile@, Canvas uses the execution role specified
-- in the Domain that owns the @UserProfile@. To allow time series
-- forecasting, this IAM role should have the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/security-iam-awsmanpol-canvas.html#security-iam-awsmanpol-AmazonSageMakerCanvasForecastAccess AmazonSageMakerCanvasForecastAccess>
-- policy attached and @forecast.amazonaws.com@ added in the trust
-- relationship as a service principal.
newTimeSeriesForecastingSettings ::
  TimeSeriesForecastingSettings
newTimeSeriesForecastingSettings =
  TimeSeriesForecastingSettings'
    { status =
        Prelude.Nothing,
      amazonForecastRoleArn = Prelude.Nothing
    }

-- | Describes whether time series forecasting is enabled or disabled in the
-- Canvas app.
timeSeriesForecastingSettings_status :: Lens.Lens' TimeSeriesForecastingSettings (Prelude.Maybe FeatureStatus)
timeSeriesForecastingSettings_status = Lens.lens (\TimeSeriesForecastingSettings' {status} -> status) (\s@TimeSeriesForecastingSettings' {} a -> s {status = a} :: TimeSeriesForecastingSettings)

-- | The IAM role that Canvas passes to Amazon Forecast for time series
-- forecasting. By default, Canvas uses the execution role specified in the
-- @UserProfile@ that launches the Canvas app. If an execution role is not
-- specified in the @UserProfile@, Canvas uses the execution role specified
-- in the Domain that owns the @UserProfile@. To allow time series
-- forecasting, this IAM role should have the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/security-iam-awsmanpol-canvas.html#security-iam-awsmanpol-AmazonSageMakerCanvasForecastAccess AmazonSageMakerCanvasForecastAccess>
-- policy attached and @forecast.amazonaws.com@ added in the trust
-- relationship as a service principal.
timeSeriesForecastingSettings_amazonForecastRoleArn :: Lens.Lens' TimeSeriesForecastingSettings (Prelude.Maybe Prelude.Text)
timeSeriesForecastingSettings_amazonForecastRoleArn = Lens.lens (\TimeSeriesForecastingSettings' {amazonForecastRoleArn} -> amazonForecastRoleArn) (\s@TimeSeriesForecastingSettings' {} a -> s {amazonForecastRoleArn = a} :: TimeSeriesForecastingSettings)

instance Data.FromJSON TimeSeriesForecastingSettings where
  parseJSON =
    Data.withObject
      "TimeSeriesForecastingSettings"
      ( \x ->
          TimeSeriesForecastingSettings'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "AmazonForecastRoleArn")
      )

instance
  Prelude.Hashable
    TimeSeriesForecastingSettings
  where
  hashWithSalt _salt TimeSeriesForecastingSettings' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` amazonForecastRoleArn

instance Prelude.NFData TimeSeriesForecastingSettings where
  rnf TimeSeriesForecastingSettings' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf amazonForecastRoleArn

instance Data.ToJSON TimeSeriesForecastingSettings where
  toJSON TimeSeriesForecastingSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Status" Data..=) Prelude.<$> status,
            ("AmazonForecastRoleArn" Data..=)
              Prelude.<$> amazonForecastRoleArn
          ]
      )
