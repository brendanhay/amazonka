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
-- Module      : Amazonka.Forecast.Types.PredictorSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.PredictorSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the predictor properties that are used in the
-- ListPredictors operation. To get the complete set of properties, call
-- the DescribePredictor operation, and provide the listed @PredictorArn@.
--
-- /See:/ 'newPredictorSummary' smart constructor.
data PredictorSummary = PredictorSummary'
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
    -- | The name of the predictor.
    predictorName :: Prelude.Maybe Prelude.Text,
    -- | The status of the predictor. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- The @Status@ of the predictor must be @ACTIVE@ before you can use the
    -- predictor to create a forecast.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the predictor.
    predictorArn :: Prelude.Maybe Prelude.Text,
    -- | When the model training task was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset group that contains the
    -- data used to train the predictor.
    datasetGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictorSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'predictorSummary_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'message', 'predictorSummary_message' - If an error occurred, an informational message about the error.
--
-- 'predictorName', 'predictorSummary_predictorName' - The name of the predictor.
--
-- 'status', 'predictorSummary_status' - The status of the predictor. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- The @Status@ of the predictor must be @ACTIVE@ before you can use the
-- predictor to create a forecast.
--
-- 'predictorArn', 'predictorSummary_predictorArn' - The ARN of the predictor.
--
-- 'creationTime', 'predictorSummary_creationTime' - When the model training task was created.
--
-- 'datasetGroupArn', 'predictorSummary_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group that contains the
-- data used to train the predictor.
newPredictorSummary ::
  PredictorSummary
newPredictorSummary =
  PredictorSummary'
    { lastModificationTime =
        Prelude.Nothing,
      message = Prelude.Nothing,
      predictorName = Prelude.Nothing,
      status = Prelude.Nothing,
      predictorArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing
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
predictorSummary_lastModificationTime :: Lens.Lens' PredictorSummary (Prelude.Maybe Prelude.UTCTime)
predictorSummary_lastModificationTime = Lens.lens (\PredictorSummary' {lastModificationTime} -> lastModificationTime) (\s@PredictorSummary' {} a -> s {lastModificationTime = a} :: PredictorSummary) Prelude.. Lens.mapping Core._Time

-- | If an error occurred, an informational message about the error.
predictorSummary_message :: Lens.Lens' PredictorSummary (Prelude.Maybe Prelude.Text)
predictorSummary_message = Lens.lens (\PredictorSummary' {message} -> message) (\s@PredictorSummary' {} a -> s {message = a} :: PredictorSummary)

-- | The name of the predictor.
predictorSummary_predictorName :: Lens.Lens' PredictorSummary (Prelude.Maybe Prelude.Text)
predictorSummary_predictorName = Lens.lens (\PredictorSummary' {predictorName} -> predictorName) (\s@PredictorSummary' {} a -> s {predictorName = a} :: PredictorSummary)

-- | The status of the predictor. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- The @Status@ of the predictor must be @ACTIVE@ before you can use the
-- predictor to create a forecast.
predictorSummary_status :: Lens.Lens' PredictorSummary (Prelude.Maybe Prelude.Text)
predictorSummary_status = Lens.lens (\PredictorSummary' {status} -> status) (\s@PredictorSummary' {} a -> s {status = a} :: PredictorSummary)

-- | The ARN of the predictor.
predictorSummary_predictorArn :: Lens.Lens' PredictorSummary (Prelude.Maybe Prelude.Text)
predictorSummary_predictorArn = Lens.lens (\PredictorSummary' {predictorArn} -> predictorArn) (\s@PredictorSummary' {} a -> s {predictorArn = a} :: PredictorSummary)

-- | When the model training task was created.
predictorSummary_creationTime :: Lens.Lens' PredictorSummary (Prelude.Maybe Prelude.UTCTime)
predictorSummary_creationTime = Lens.lens (\PredictorSummary' {creationTime} -> creationTime) (\s@PredictorSummary' {} a -> s {creationTime = a} :: PredictorSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the dataset group that contains the
-- data used to train the predictor.
predictorSummary_datasetGroupArn :: Lens.Lens' PredictorSummary (Prelude.Maybe Prelude.Text)
predictorSummary_datasetGroupArn = Lens.lens (\PredictorSummary' {datasetGroupArn} -> datasetGroupArn) (\s@PredictorSummary' {} a -> s {datasetGroupArn = a} :: PredictorSummary)

instance Core.FromJSON PredictorSummary where
  parseJSON =
    Core.withObject
      "PredictorSummary"
      ( \x ->
          PredictorSummary'
            Prelude.<$> (x Core..:? "LastModificationTime")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "PredictorName")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "PredictorArn")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "DatasetGroupArn")
      )

instance Prelude.Hashable PredictorSummary where
  hashWithSalt _salt PredictorSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` predictorName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` predictorArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` datasetGroupArn

instance Prelude.NFData PredictorSummary where
  rnf PredictorSummary' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf predictorName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf predictorArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf datasetGroupArn
