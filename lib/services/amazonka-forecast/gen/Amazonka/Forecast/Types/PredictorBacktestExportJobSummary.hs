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
-- Module      : Amazonka.Forecast.Types.PredictorBacktestExportJobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.PredictorBacktestExportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.DataDestination
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the predictor backtest export job properties used
-- in the ListPredictorBacktestExportJobs operation. To get a complete set
-- of properties, call the DescribePredictorBacktestExportJob operation,
-- and provide the listed @PredictorBacktestExportJobArn@.
--
-- /See:/ 'newPredictorBacktestExportJobSummary' smart constructor.
data PredictorBacktestExportJobSummary = PredictorBacktestExportJobSummary'
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
    destination :: Prelude.Maybe DataDestination,
    -- | Information about any errors that may have occurred during the backtest
    -- export.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the predictor backtest export job. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the predictor backtest export job.
    predictorBacktestExportJobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the predictor backtest export job.
    predictorBacktestExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | When the predictor backtest export job was created.
    creationTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictorBacktestExportJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'predictorBacktestExportJobSummary_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'destination', 'predictorBacktestExportJobSummary_destination' - Undocumented member.
--
-- 'message', 'predictorBacktestExportJobSummary_message' - Information about any errors that may have occurred during the backtest
-- export.
--
-- 'status', 'predictorBacktestExportJobSummary_status' - The status of the predictor backtest export job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- 'predictorBacktestExportJobName', 'predictorBacktestExportJobSummary_predictorBacktestExportJobName' - The name of the predictor backtest export job.
--
-- 'predictorBacktestExportJobArn', 'predictorBacktestExportJobSummary_predictorBacktestExportJobArn' - The Amazon Resource Name (ARN) of the predictor backtest export job.
--
-- 'creationTime', 'predictorBacktestExportJobSummary_creationTime' - When the predictor backtest export job was created.
newPredictorBacktestExportJobSummary ::
  PredictorBacktestExportJobSummary
newPredictorBacktestExportJobSummary =
  PredictorBacktestExportJobSummary'
    { lastModificationTime =
        Prelude.Nothing,
      destination = Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing,
      predictorBacktestExportJobName =
        Prelude.Nothing,
      predictorBacktestExportJobArn =
        Prelude.Nothing,
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
predictorBacktestExportJobSummary_lastModificationTime :: Lens.Lens' PredictorBacktestExportJobSummary (Prelude.Maybe Prelude.UTCTime)
predictorBacktestExportJobSummary_lastModificationTime = Lens.lens (\PredictorBacktestExportJobSummary' {lastModificationTime} -> lastModificationTime) (\s@PredictorBacktestExportJobSummary' {} a -> s {lastModificationTime = a} :: PredictorBacktestExportJobSummary) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
predictorBacktestExportJobSummary_destination :: Lens.Lens' PredictorBacktestExportJobSummary (Prelude.Maybe DataDestination)
predictorBacktestExportJobSummary_destination = Lens.lens (\PredictorBacktestExportJobSummary' {destination} -> destination) (\s@PredictorBacktestExportJobSummary' {} a -> s {destination = a} :: PredictorBacktestExportJobSummary)

-- | Information about any errors that may have occurred during the backtest
-- export.
predictorBacktestExportJobSummary_message :: Lens.Lens' PredictorBacktestExportJobSummary (Prelude.Maybe Prelude.Text)
predictorBacktestExportJobSummary_message = Lens.lens (\PredictorBacktestExportJobSummary' {message} -> message) (\s@PredictorBacktestExportJobSummary' {} a -> s {message = a} :: PredictorBacktestExportJobSummary)

-- | The status of the predictor backtest export job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
predictorBacktestExportJobSummary_status :: Lens.Lens' PredictorBacktestExportJobSummary (Prelude.Maybe Prelude.Text)
predictorBacktestExportJobSummary_status = Lens.lens (\PredictorBacktestExportJobSummary' {status} -> status) (\s@PredictorBacktestExportJobSummary' {} a -> s {status = a} :: PredictorBacktestExportJobSummary)

-- | The name of the predictor backtest export job.
predictorBacktestExportJobSummary_predictorBacktestExportJobName :: Lens.Lens' PredictorBacktestExportJobSummary (Prelude.Maybe Prelude.Text)
predictorBacktestExportJobSummary_predictorBacktestExportJobName = Lens.lens (\PredictorBacktestExportJobSummary' {predictorBacktestExportJobName} -> predictorBacktestExportJobName) (\s@PredictorBacktestExportJobSummary' {} a -> s {predictorBacktestExportJobName = a} :: PredictorBacktestExportJobSummary)

-- | The Amazon Resource Name (ARN) of the predictor backtest export job.
predictorBacktestExportJobSummary_predictorBacktestExportJobArn :: Lens.Lens' PredictorBacktestExportJobSummary (Prelude.Maybe Prelude.Text)
predictorBacktestExportJobSummary_predictorBacktestExportJobArn = Lens.lens (\PredictorBacktestExportJobSummary' {predictorBacktestExportJobArn} -> predictorBacktestExportJobArn) (\s@PredictorBacktestExportJobSummary' {} a -> s {predictorBacktestExportJobArn = a} :: PredictorBacktestExportJobSummary)

-- | When the predictor backtest export job was created.
predictorBacktestExportJobSummary_creationTime :: Lens.Lens' PredictorBacktestExportJobSummary (Prelude.Maybe Prelude.UTCTime)
predictorBacktestExportJobSummary_creationTime = Lens.lens (\PredictorBacktestExportJobSummary' {creationTime} -> creationTime) (\s@PredictorBacktestExportJobSummary' {} a -> s {creationTime = a} :: PredictorBacktestExportJobSummary) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    PredictorBacktestExportJobSummary
  where
  parseJSON =
    Data.withObject
      "PredictorBacktestExportJobSummary"
      ( \x ->
          PredictorBacktestExportJobSummary'
            Prelude.<$> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "Destination")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "PredictorBacktestExportJobName")
            Prelude.<*> (x Data..:? "PredictorBacktestExportJobArn")
            Prelude.<*> (x Data..:? "CreationTime")
      )

instance
  Prelude.Hashable
    PredictorBacktestExportJobSummary
  where
  hashWithSalt
    _salt
    PredictorBacktestExportJobSummary' {..} =
      _salt `Prelude.hashWithSalt` lastModificationTime
        `Prelude.hashWithSalt` destination
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` predictorBacktestExportJobName
        `Prelude.hashWithSalt` predictorBacktestExportJobArn
        `Prelude.hashWithSalt` creationTime

instance
  Prelude.NFData
    PredictorBacktestExportJobSummary
  where
  rnf PredictorBacktestExportJobSummary' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf predictorBacktestExportJobName
      `Prelude.seq` Prelude.rnf predictorBacktestExportJobArn
      `Prelude.seq` Prelude.rnf creationTime
