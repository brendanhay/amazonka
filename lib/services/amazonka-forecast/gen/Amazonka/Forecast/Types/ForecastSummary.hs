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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the forecast properties used in the ListForecasts
-- operation. To get the complete set of properties, call the
-- DescribeForecast operation, and provide the @ForecastArn@ that is listed
-- in the summary.
--
-- /See:/ 'newForecastSummary' smart constructor.
data ForecastSummary = ForecastSummary'
  { -- | Whether the Forecast was created from an AutoPredictor.
    createdUsingAutoPredictor :: Prelude.Maybe Prelude.Bool,
    -- | When the forecast creation task was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset group that provided the
    -- data used to train the predictor.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the forecast.
    forecastArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the forecast.
    forecastName :: Prelude.Maybe Prelude.Text,
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
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the predictor used to generate the forecast.
    predictorArn :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe Prelude.Text
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
-- 'createdUsingAutoPredictor', 'forecastSummary_createdUsingAutoPredictor' - Whether the Forecast was created from an AutoPredictor.
--
-- 'creationTime', 'forecastSummary_creationTime' - When the forecast creation task was created.
--
-- 'datasetGroupArn', 'forecastSummary_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group that provided the
-- data used to train the predictor.
--
-- 'forecastArn', 'forecastSummary_forecastArn' - The ARN of the forecast.
--
-- 'forecastName', 'forecastSummary_forecastName' - The name of the forecast.
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
-- 'predictorArn', 'forecastSummary_predictorArn' - The ARN of the predictor used to generate the forecast.
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
newForecastSummary ::
  ForecastSummary
newForecastSummary =
  ForecastSummary'
    { createdUsingAutoPredictor =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      forecastArn = Prelude.Nothing,
      forecastName = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      message = Prelude.Nothing,
      predictorArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Whether the Forecast was created from an AutoPredictor.
forecastSummary_createdUsingAutoPredictor :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Bool)
forecastSummary_createdUsingAutoPredictor = Lens.lens (\ForecastSummary' {createdUsingAutoPredictor} -> createdUsingAutoPredictor) (\s@ForecastSummary' {} a -> s {createdUsingAutoPredictor = a} :: ForecastSummary)

-- | When the forecast creation task was created.
forecastSummary_creationTime :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.UTCTime)
forecastSummary_creationTime = Lens.lens (\ForecastSummary' {creationTime} -> creationTime) (\s@ForecastSummary' {} a -> s {creationTime = a} :: ForecastSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the dataset group that provided the
-- data used to train the predictor.
forecastSummary_datasetGroupArn :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_datasetGroupArn = Lens.lens (\ForecastSummary' {datasetGroupArn} -> datasetGroupArn) (\s@ForecastSummary' {} a -> s {datasetGroupArn = a} :: ForecastSummary)

-- | The ARN of the forecast.
forecastSummary_forecastArn :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_forecastArn = Lens.lens (\ForecastSummary' {forecastArn} -> forecastArn) (\s@ForecastSummary' {} a -> s {forecastArn = a} :: ForecastSummary)

-- | The name of the forecast.
forecastSummary_forecastName :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_forecastName = Lens.lens (\ForecastSummary' {forecastName} -> forecastName) (\s@ForecastSummary' {} a -> s {forecastName = a} :: ForecastSummary)

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
forecastSummary_lastModificationTime = Lens.lens (\ForecastSummary' {lastModificationTime} -> lastModificationTime) (\s@ForecastSummary' {} a -> s {lastModificationTime = a} :: ForecastSummary) Prelude.. Lens.mapping Data._Time

-- | If an error occurred, an informational message about the error.
forecastSummary_message :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_message = Lens.lens (\ForecastSummary' {message} -> message) (\s@ForecastSummary' {} a -> s {message = a} :: ForecastSummary)

-- | The ARN of the predictor used to generate the forecast.
forecastSummary_predictorArn :: Lens.Lens' ForecastSummary (Prelude.Maybe Prelude.Text)
forecastSummary_predictorArn = Lens.lens (\ForecastSummary' {predictorArn} -> predictorArn) (\s@ForecastSummary' {} a -> s {predictorArn = a} :: ForecastSummary)

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

instance Data.FromJSON ForecastSummary where
  parseJSON =
    Data.withObject
      "ForecastSummary"
      ( \x ->
          ForecastSummary'
            Prelude.<$> (x Data..:? "CreatedUsingAutoPredictor")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DatasetGroupArn")
            Prelude.<*> (x Data..:? "ForecastArn")
            Prelude.<*> (x Data..:? "ForecastName")
            Prelude.<*> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "PredictorArn")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ForecastSummary where
  hashWithSalt _salt ForecastSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdUsingAutoPredictor
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` forecastArn
      `Prelude.hashWithSalt` forecastName
      `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` predictorArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData ForecastSummary where
  rnf ForecastSummary' {..} =
    Prelude.rnf createdUsingAutoPredictor
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf forecastArn
      `Prelude.seq` Prelude.rnf forecastName
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf predictorArn
      `Prelude.seq` Prelude.rnf status
