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
-- Module      : Amazonka.Comprehend.Types.FlywheelSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.FlywheelSummary where

import Amazonka.Comprehend.Types.FlywheelStatus
import Amazonka.Comprehend.Types.ModelType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Flywheel summary information.
--
-- /See:/ 'newFlywheelSummary' smart constructor.
data FlywheelSummary = FlywheelSummary'
  { -- | ARN of the active model version for the flywheel.
    activeModelArn :: Prelude.Maybe Prelude.Text,
    -- | Creation time of the flywheel.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Amazon S3 URI of the data lake location.
    dataLakeS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the flywheel
    flywheelArn :: Prelude.Maybe Prelude.Text,
    -- | Last modified time for the flywheel.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The most recent flywheel iteration.
    latestFlywheelIteration :: Prelude.Maybe Prelude.Text,
    -- | A description of the status of the flywheel.
    message :: Prelude.Maybe Prelude.Text,
    -- | Model type of the flywheel\'s model.
    modelType :: Prelude.Maybe ModelType,
    -- | The status of the flywheel.
    status :: Prelude.Maybe FlywheelStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlywheelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeModelArn', 'flywheelSummary_activeModelArn' - ARN of the active model version for the flywheel.
--
-- 'creationTime', 'flywheelSummary_creationTime' - Creation time of the flywheel.
--
-- 'dataLakeS3Uri', 'flywheelSummary_dataLakeS3Uri' - Amazon S3 URI of the data lake location.
--
-- 'flywheelArn', 'flywheelSummary_flywheelArn' - The Amazon Resource Number (ARN) of the flywheel
--
-- 'lastModifiedTime', 'flywheelSummary_lastModifiedTime' - Last modified time for the flywheel.
--
-- 'latestFlywheelIteration', 'flywheelSummary_latestFlywheelIteration' - The most recent flywheel iteration.
--
-- 'message', 'flywheelSummary_message' - A description of the status of the flywheel.
--
-- 'modelType', 'flywheelSummary_modelType' - Model type of the flywheel\'s model.
--
-- 'status', 'flywheelSummary_status' - The status of the flywheel.
newFlywheelSummary ::
  FlywheelSummary
newFlywheelSummary =
  FlywheelSummary'
    { activeModelArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dataLakeS3Uri = Prelude.Nothing,
      flywheelArn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      latestFlywheelIteration = Prelude.Nothing,
      message = Prelude.Nothing,
      modelType = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | ARN of the active model version for the flywheel.
flywheelSummary_activeModelArn :: Lens.Lens' FlywheelSummary (Prelude.Maybe Prelude.Text)
flywheelSummary_activeModelArn = Lens.lens (\FlywheelSummary' {activeModelArn} -> activeModelArn) (\s@FlywheelSummary' {} a -> s {activeModelArn = a} :: FlywheelSummary)

-- | Creation time of the flywheel.
flywheelSummary_creationTime :: Lens.Lens' FlywheelSummary (Prelude.Maybe Prelude.UTCTime)
flywheelSummary_creationTime = Lens.lens (\FlywheelSummary' {creationTime} -> creationTime) (\s@FlywheelSummary' {} a -> s {creationTime = a} :: FlywheelSummary) Prelude.. Lens.mapping Data._Time

-- | Amazon S3 URI of the data lake location.
flywheelSummary_dataLakeS3Uri :: Lens.Lens' FlywheelSummary (Prelude.Maybe Prelude.Text)
flywheelSummary_dataLakeS3Uri = Lens.lens (\FlywheelSummary' {dataLakeS3Uri} -> dataLakeS3Uri) (\s@FlywheelSummary' {} a -> s {dataLakeS3Uri = a} :: FlywheelSummary)

-- | The Amazon Resource Number (ARN) of the flywheel
flywheelSummary_flywheelArn :: Lens.Lens' FlywheelSummary (Prelude.Maybe Prelude.Text)
flywheelSummary_flywheelArn = Lens.lens (\FlywheelSummary' {flywheelArn} -> flywheelArn) (\s@FlywheelSummary' {} a -> s {flywheelArn = a} :: FlywheelSummary)

-- | Last modified time for the flywheel.
flywheelSummary_lastModifiedTime :: Lens.Lens' FlywheelSummary (Prelude.Maybe Prelude.UTCTime)
flywheelSummary_lastModifiedTime = Lens.lens (\FlywheelSummary' {lastModifiedTime} -> lastModifiedTime) (\s@FlywheelSummary' {} a -> s {lastModifiedTime = a} :: FlywheelSummary) Prelude.. Lens.mapping Data._Time

-- | The most recent flywheel iteration.
flywheelSummary_latestFlywheelIteration :: Lens.Lens' FlywheelSummary (Prelude.Maybe Prelude.Text)
flywheelSummary_latestFlywheelIteration = Lens.lens (\FlywheelSummary' {latestFlywheelIteration} -> latestFlywheelIteration) (\s@FlywheelSummary' {} a -> s {latestFlywheelIteration = a} :: FlywheelSummary)

-- | A description of the status of the flywheel.
flywheelSummary_message :: Lens.Lens' FlywheelSummary (Prelude.Maybe Prelude.Text)
flywheelSummary_message = Lens.lens (\FlywheelSummary' {message} -> message) (\s@FlywheelSummary' {} a -> s {message = a} :: FlywheelSummary)

-- | Model type of the flywheel\'s model.
flywheelSummary_modelType :: Lens.Lens' FlywheelSummary (Prelude.Maybe ModelType)
flywheelSummary_modelType = Lens.lens (\FlywheelSummary' {modelType} -> modelType) (\s@FlywheelSummary' {} a -> s {modelType = a} :: FlywheelSummary)

-- | The status of the flywheel.
flywheelSummary_status :: Lens.Lens' FlywheelSummary (Prelude.Maybe FlywheelStatus)
flywheelSummary_status = Lens.lens (\FlywheelSummary' {status} -> status) (\s@FlywheelSummary' {} a -> s {status = a} :: FlywheelSummary)

instance Data.FromJSON FlywheelSummary where
  parseJSON =
    Data.withObject
      "FlywheelSummary"
      ( \x ->
          FlywheelSummary'
            Prelude.<$> (x Data..:? "ActiveModelArn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DataLakeS3Uri")
            Prelude.<*> (x Data..:? "FlywheelArn")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "LatestFlywheelIteration")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "ModelType")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable FlywheelSummary where
  hashWithSalt _salt FlywheelSummary' {..} =
    _salt
      `Prelude.hashWithSalt` activeModelArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataLakeS3Uri
      `Prelude.hashWithSalt` flywheelArn
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` latestFlywheelIteration
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` status

instance Prelude.NFData FlywheelSummary where
  rnf FlywheelSummary' {..} =
    Prelude.rnf activeModelArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataLakeS3Uri
      `Prelude.seq` Prelude.rnf flywheelArn
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf latestFlywheelIteration
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf status
