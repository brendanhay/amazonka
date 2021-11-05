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
-- Module      : Network.AWS.Forecast.Types.ForecastSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Forecast.Types.ForecastSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a summary of the forecast properties used in the ListForecasts
-- operation. To get the complete set of properties, call the
-- DescribeForecast operation, and provide the @ForecastArn@ that is listed
-- in the summary.
--
-- /See:/ 'newForecastSummary' smart constructor.
data ForecastSummary = ForecastSummary'
  { -- | When the forecast creation task was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the forecast. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- The @Status@ of the forecast must be @ACTIVE@ before you can query or
    -- export the forecast.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the predictor used to generate the forecast.
    predictorArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the forecast.
    forecastArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the forecast.
    forecastName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset group that provided the
    -- data used to train the predictor.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The last time the resource was modified. The timestamp depends on the
    -- status of the job:
    --
    -- -   @CREATE_PENDING@ - The @CreationTime@.
    --
    -- -   @CREATE_IN_PROGRESS@ - The current timestamp.
    --
    -- -   @CREATE_STOPPING@ - The current timestamp.
    --
    -- -   @CREATE_STOPPED@ - When the job stopped.
    --
    -- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
    lastModificationTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForecastSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'forecastSummary_creationTime' - When the forecast creation task was created.
--
-- 'status', 'forecastSummary_status' - The status of the forecast. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the forecast must be @ACTIVE@ before you can query or
-- export the forecast.
--
-- 'predictorArn', 'forecastSummary_predictorArn' - The ARN of the predictor used to generate the forecast.
--
-- 'forecastArn', 'forecastSummary_forecastArn' - The ARN of the forecast.
--
-- 'forecastName', 'forecastSummary_forecastName' - The name of the forecast.
--
-- 'datasetGroupArn', 'forecastSummary_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group that provided the
-- data used to train the predictor.
--
-- 'message', 'forecastSummary_message' - If an error occurred, an informational message about the error.
--
-- 'lastModificationTime', 'forecastSummary_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
newForecastSummary ::
  ForecastSummary
newForecastSummary =
  ForecastSummary'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      predictorArn = Prelude.Nothing,
      forecastArn = Prelude.Nothing,
      forecastName = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      message = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing
    }

-- | When the forecast creation task was created.
forecastSummary_creationTime :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.UTCTime)
forecastSummary_creationTime = Lens.lens (\ForecastSummary' {creationTime} -> creationTime) (\s@ForecastSummary' {} a -> s {creationTime = a} :: ForecastSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the forecast. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the forecast must be @ACTIVE@ before you can query or
-- export the forecast.
forecastSummary_status :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_status = Lens.lens (\ForecastSummary' {status} -> status) (\s@ForecastSummary' {} a -> s {status = a} :: ForecastSummary)

-- | The ARN of the predictor used to generate the forecast.
forecastSummary_predictorArn :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_predictorArn = Lens.lens (\ForecastSummary' {predictorArn} -> predictorArn) (\s@ForecastSummary' {} a -> s {predictorArn = a} :: ForecastSummary)

-- | The ARN of the forecast.
forecastSummary_forecastArn :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_forecastArn = Lens.lens (\ForecastSummary' {forecastArn} -> forecastArn) (\s@ForecastSummary' {} a -> s {forecastArn = a} :: ForecastSummary)

-- | The name of the forecast.
forecastSummary_forecastName :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_forecastName = Lens.lens (\ForecastSummary' {forecastName} -> forecastName) (\s@ForecastSummary' {} a -> s {forecastName = a} :: ForecastSummary)

-- | The Amazon Resource Name (ARN) of the dataset group that provided the
-- data used to train the predictor.
forecastSummary_datasetGroupArn :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_datasetGroupArn = Lens.lens (\ForecastSummary' {datasetGroupArn} -> datasetGroupArn) (\s@ForecastSummary' {} a -> s {datasetGroupArn = a} :: ForecastSummary)

-- | If an error occurred, an informational message about the error.
forecastSummary_message :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_message = Lens.lens (\ForecastSummary' {message} -> message) (\s@ForecastSummary' {} a -> s {message = a} :: ForecastSummary)

-- | The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
forecastSummary_lastModificationTime :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.UTCTime)
forecastSummary_lastModificationTime = Lens.lens (\ForecastSummary' {lastModificationTime} -> lastModificationTime) (\s@ForecastSummary' {} a -> s {lastModificationTime = a} :: ForecastSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ForecastSummary where
  parseJSON =
    Core.withObject
      "ForecastSummary"
      ( \x ->
          ForecastSummary'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "PredictorArn")
            Prelude.<*> (x Core..:? "ForecastArn")
            Prelude.<*> (x Core..:? "ForecastName")
            Prelude.<*> (x Core..:? "DatasetGroupArn")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "LastModificationTime")
      )

instance Prelude.Hashable ForecastSummary

instance Prelude.NFData ForecastSummary
