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
-- Module      : Amazonka.Forecast.Types.ForecastExportJobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.ForecastExportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.DataDestination
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the forecast export job properties used in the
-- ListForecastExportJobs operation. To get the complete set of properties,
-- call the DescribeForecastExportJob operation, and provide the listed
-- @ForecastExportJobArn@.
--
-- /See:/ 'newForecastExportJobSummary' smart constructor.
data ForecastExportJobSummary = ForecastExportJobSummary'
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
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | The path to the Amazon Simple Storage Service (Amazon S3) bucket where
    -- the forecast is exported.
    destination :: Prelude.Maybe DataDestination,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the forecast export job.
    forecastExportJobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the forecast export job.
    forecastExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the forecast export job. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- The @Status@ of the forecast export job must be @ACTIVE@ before you can
    -- access the forecast in your S3 bucket.
    status :: Prelude.Maybe Prelude.Text,
    -- | When the forecast export job was created.
    creationTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForecastExportJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'forecastExportJobSummary_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'destination', 'forecastExportJobSummary_destination' - The path to the Amazon Simple Storage Service (Amazon S3) bucket where
-- the forecast is exported.
--
-- 'message', 'forecastExportJobSummary_message' - If an error occurred, an informational message about the error.
--
-- 'forecastExportJobName', 'forecastExportJobSummary_forecastExportJobName' - The name of the forecast export job.
--
-- 'forecastExportJobArn', 'forecastExportJobSummary_forecastExportJobArn' - The Amazon Resource Name (ARN) of the forecast export job.
--
-- 'status', 'forecastExportJobSummary_status' - The status of the forecast export job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the forecast export job must be @ACTIVE@ before you can
-- access the forecast in your S3 bucket.
--
-- 'creationTime', 'forecastExportJobSummary_creationTime' - When the forecast export job was created.
newForecastExportJobSummary ::
  ForecastExportJobSummary
newForecastExportJobSummary =
  ForecastExportJobSummary'
    { lastModificationTime =
        Prelude.Nothing,
      destination = Prelude.Nothing,
      message = Prelude.Nothing,
      forecastExportJobName = Prelude.Nothing,
      forecastExportJobArn = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing
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
forecastExportJobSummary_lastModificationTime :: Lens.Lens' ForecastExportJobSummary (Prelude.Maybe Prelude.UTCTime)
forecastExportJobSummary_lastModificationTime = Lens.lens (\ForecastExportJobSummary' {lastModificationTime} -> lastModificationTime) (\s@ForecastExportJobSummary' {} a -> s {lastModificationTime = a} :: ForecastExportJobSummary) Prelude.. Lens.mapping Data._Time

-- | The path to the Amazon Simple Storage Service (Amazon S3) bucket where
-- the forecast is exported.
forecastExportJobSummary_destination :: Lens.Lens' ForecastExportJobSummary (Prelude.Maybe DataDestination)
forecastExportJobSummary_destination = Lens.lens (\ForecastExportJobSummary' {destination} -> destination) (\s@ForecastExportJobSummary' {} a -> s {destination = a} :: ForecastExportJobSummary)

-- | If an error occurred, an informational message about the error.
forecastExportJobSummary_message :: Lens.Lens' ForecastExportJobSummary (Prelude.Maybe Prelude.Text)
forecastExportJobSummary_message = Lens.lens (\ForecastExportJobSummary' {message} -> message) (\s@ForecastExportJobSummary' {} a -> s {message = a} :: ForecastExportJobSummary)

-- | The name of the forecast export job.
forecastExportJobSummary_forecastExportJobName :: Lens.Lens' ForecastExportJobSummary (Prelude.Maybe Prelude.Text)
forecastExportJobSummary_forecastExportJobName = Lens.lens (\ForecastExportJobSummary' {forecastExportJobName} -> forecastExportJobName) (\s@ForecastExportJobSummary' {} a -> s {forecastExportJobName = a} :: ForecastExportJobSummary)

-- | The Amazon Resource Name (ARN) of the forecast export job.
forecastExportJobSummary_forecastExportJobArn :: Lens.Lens' ForecastExportJobSummary (Prelude.Maybe Prelude.Text)
forecastExportJobSummary_forecastExportJobArn = Lens.lens (\ForecastExportJobSummary' {forecastExportJobArn} -> forecastExportJobArn) (\s@ForecastExportJobSummary' {} a -> s {forecastExportJobArn = a} :: ForecastExportJobSummary)

-- | The status of the forecast export job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the forecast export job must be @ACTIVE@ before you can
-- access the forecast in your S3 bucket.
forecastExportJobSummary_status :: Lens.Lens' ForecastExportJobSummary (Prelude.Maybe Prelude.Text)
forecastExportJobSummary_status = Lens.lens (\ForecastExportJobSummary' {status} -> status) (\s@ForecastExportJobSummary' {} a -> s {status = a} :: ForecastExportJobSummary)

-- | When the forecast export job was created.
forecastExportJobSummary_creationTime :: Lens.Lens' ForecastExportJobSummary (Prelude.Maybe Prelude.UTCTime)
forecastExportJobSummary_creationTime = Lens.lens (\ForecastExportJobSummary' {creationTime} -> creationTime) (\s@ForecastExportJobSummary' {} a -> s {creationTime = a} :: ForecastExportJobSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ForecastExportJobSummary where
  parseJSON =
    Data.withObject
      "ForecastExportJobSummary"
      ( \x ->
          ForecastExportJobSummary'
            Prelude.<$> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "Destination")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "ForecastExportJobName")
            Prelude.<*> (x Data..:? "ForecastExportJobArn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "CreationTime")
      )

instance Prelude.Hashable ForecastExportJobSummary where
  hashWithSalt _salt ForecastExportJobSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` forecastExportJobName
      `Prelude.hashWithSalt` forecastExportJobArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ForecastExportJobSummary where
  rnf ForecastExportJobSummary' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf forecastExportJobName
      `Prelude.seq` Prelude.rnf forecastExportJobArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime
