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
-- Module      : Amazonka.Forecast.Types.ForecastSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.ForecastSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the forecast properties used in the ListForecasts
-- operation. To get the complete set of properties, call the
-- DescribeForecast operation, and provide the @ForecastArn@ that is listed
-- in the summary.
--
-- /See:/ 'newForecastSummary' smart constructor.
data ForecastSummary = ForecastSummary'
  { -- | The last time the resource was modified. The timestamp depends on the
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
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
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
    -- | When the forecast creation task was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset group that provided the
    -- data used to train the predictor.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the Forecast was created from an AutoPredictor.
    createdUsingAutoPredictor :: Prelude.Maybe Prelude.Bool,
    -- | The name of the forecast.
    forecastName :: Prelude.Maybe Prelude.Text
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
--
-- 'message', 'forecastSummary_message' - If an error occurred, an informational message about the error.
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
-- 'creationTime', 'forecastSummary_creationTime' - When the forecast creation task was created.
--
-- 'datasetGroupArn', 'forecastSummary_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group that provided the
-- data used to train the predictor.
--
-- 'createdUsingAutoPredictor', 'forecastSummary_createdUsingAutoPredictor' - Whether the Forecast was created from an AutoPredictor.
--
-- 'forecastName', 'forecastSummary_forecastName' - The name of the forecast.
newForecastSummary ::
  ForecastSummary
newForecastSummary =
  ForecastSummary'
    { lastModificationTime =
        Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing,
      predictorArn = Prelude.Nothing,
      forecastArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      createdUsingAutoPredictor = Prelude.Nothing,
      forecastName = Prelude.Nothing
    }

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

-- | If an error occurred, an informational message about the error.
forecastSummary_message :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_message = Lens.lens (\ForecastSummary' {message} -> message) (\s@ForecastSummary' {} a -> s {message = a} :: ForecastSummary)

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

-- | When the forecast creation task was created.
forecastSummary_creationTime :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.UTCTime)
forecastSummary_creationTime = Lens.lens (\ForecastSummary' {creationTime} -> creationTime) (\s@ForecastSummary' {} a -> s {creationTime = a} :: ForecastSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the dataset group that provided the
-- data used to train the predictor.
forecastSummary_datasetGroupArn :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_datasetGroupArn = Lens.lens (\ForecastSummary' {datasetGroupArn} -> datasetGroupArn) (\s@ForecastSummary' {} a -> s {datasetGroupArn = a} :: ForecastSummary)

-- | Whether the Forecast was created from an AutoPredictor.
forecastSummary_createdUsingAutoPredictor :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Bool)
forecastSummary_createdUsingAutoPredictor = Lens.lens (\ForecastSummary' {createdUsingAutoPredictor} -> createdUsingAutoPredictor) (\s@ForecastSummary' {} a -> s {createdUsingAutoPredictor = a} :: ForecastSummary)

-- | The name of the forecast.
forecastSummary_forecastName :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_forecastName = Lens.lens (\ForecastSummary' {forecastName} -> forecastName) (\s@ForecastSummary' {} a -> s {forecastName = a} :: ForecastSummary)

instance Core.FromJSON ForecastSummary where
  parseJSON =
    Core.withObject
      "ForecastSummary"
      ( \x ->
          ForecastSummary'
            Prelude.<$> (x Core..:? "LastModificationTime")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "PredictorArn")
            Prelude.<*> (x Core..:? "ForecastArn")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "DatasetGroupArn")
            Prelude.<*> (x Core..:? "CreatedUsingAutoPredictor")
            Prelude.<*> (x Core..:? "ForecastName")
      )

instance Prelude.Hashable ForecastSummary where
  hashWithSalt _salt ForecastSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` predictorArn
      `Prelude.hashWithSalt` forecastArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` createdUsingAutoPredictor
      `Prelude.hashWithSalt` forecastName

instance Prelude.NFData ForecastSummary where
  rnf ForecastSummary' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf predictorArn
      `Prelude.seq` Prelude.rnf forecastArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf createdUsingAutoPredictor
      `Prelude.seq` Prelude.rnf forecastName
