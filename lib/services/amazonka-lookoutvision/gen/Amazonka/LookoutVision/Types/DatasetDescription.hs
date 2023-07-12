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
-- Module      : Amazonka.LookoutVision.Types.DatasetDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.DatasetDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types.DatasetImageStats
import Amazonka.LookoutVision.Types.DatasetStatus
import qualified Amazonka.Prelude as Prelude

-- | The description for a dataset. For more information, see
-- DescribeDataset.
--
-- /See:/ 'newDatasetDescription' smart constructor.
data DatasetDescription = DatasetDescription'
  { -- | The Unix timestamp for the time and date that the dataset was created.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The type of the dataset. The value @train@ represents a training dataset
    -- or single dataset project. The value @test@ represents a test dataset.
    datasetType :: Prelude.Maybe Prelude.Text,
    -- | Statistics about the images in a dataset.
    imageStats :: Prelude.Maybe DatasetImageStats,
    -- | The Unix timestamp for the date and time that the dataset was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the project that contains the dataset.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset.
    status :: Prelude.Maybe DatasetStatus,
    -- | The status message for the dataset.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'datasetDescription_creationTimestamp' - The Unix timestamp for the time and date that the dataset was created.
--
-- 'datasetType', 'datasetDescription_datasetType' - The type of the dataset. The value @train@ represents a training dataset
-- or single dataset project. The value @test@ represents a test dataset.
--
-- 'imageStats', 'datasetDescription_imageStats' - Statistics about the images in a dataset.
--
-- 'lastUpdatedTimestamp', 'datasetDescription_lastUpdatedTimestamp' - The Unix timestamp for the date and time that the dataset was last
-- updated.
--
-- 'projectName', 'datasetDescription_projectName' - The name of the project that contains the dataset.
--
-- 'status', 'datasetDescription_status' - The status of the dataset.
--
-- 'statusMessage', 'datasetDescription_statusMessage' - The status message for the dataset.
newDatasetDescription ::
  DatasetDescription
newDatasetDescription =
  DatasetDescription'
    { creationTimestamp =
        Prelude.Nothing,
      datasetType = Prelude.Nothing,
      imageStats = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      projectName = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The Unix timestamp for the time and date that the dataset was created.
datasetDescription_creationTimestamp :: Lens.Lens' DatasetDescription (Prelude.Maybe Prelude.UTCTime)
datasetDescription_creationTimestamp = Lens.lens (\DatasetDescription' {creationTimestamp} -> creationTimestamp) (\s@DatasetDescription' {} a -> s {creationTimestamp = a} :: DatasetDescription) Prelude.. Lens.mapping Data._Time

-- | The type of the dataset. The value @train@ represents a training dataset
-- or single dataset project. The value @test@ represents a test dataset.
datasetDescription_datasetType :: Lens.Lens' DatasetDescription (Prelude.Maybe Prelude.Text)
datasetDescription_datasetType = Lens.lens (\DatasetDescription' {datasetType} -> datasetType) (\s@DatasetDescription' {} a -> s {datasetType = a} :: DatasetDescription)

-- | Statistics about the images in a dataset.
datasetDescription_imageStats :: Lens.Lens' DatasetDescription (Prelude.Maybe DatasetImageStats)
datasetDescription_imageStats = Lens.lens (\DatasetDescription' {imageStats} -> imageStats) (\s@DatasetDescription' {} a -> s {imageStats = a} :: DatasetDescription)

-- | The Unix timestamp for the date and time that the dataset was last
-- updated.
datasetDescription_lastUpdatedTimestamp :: Lens.Lens' DatasetDescription (Prelude.Maybe Prelude.UTCTime)
datasetDescription_lastUpdatedTimestamp = Lens.lens (\DatasetDescription' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@DatasetDescription' {} a -> s {lastUpdatedTimestamp = a} :: DatasetDescription) Prelude.. Lens.mapping Data._Time

-- | The name of the project that contains the dataset.
datasetDescription_projectName :: Lens.Lens' DatasetDescription (Prelude.Maybe Prelude.Text)
datasetDescription_projectName = Lens.lens (\DatasetDescription' {projectName} -> projectName) (\s@DatasetDescription' {} a -> s {projectName = a} :: DatasetDescription)

-- | The status of the dataset.
datasetDescription_status :: Lens.Lens' DatasetDescription (Prelude.Maybe DatasetStatus)
datasetDescription_status = Lens.lens (\DatasetDescription' {status} -> status) (\s@DatasetDescription' {} a -> s {status = a} :: DatasetDescription)

-- | The status message for the dataset.
datasetDescription_statusMessage :: Lens.Lens' DatasetDescription (Prelude.Maybe Prelude.Text)
datasetDescription_statusMessage = Lens.lens (\DatasetDescription' {statusMessage} -> statusMessage) (\s@DatasetDescription' {} a -> s {statusMessage = a} :: DatasetDescription)

instance Data.FromJSON DatasetDescription where
  parseJSON =
    Data.withObject
      "DatasetDescription"
      ( \x ->
          DatasetDescription'
            Prelude.<$> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "DatasetType")
            Prelude.<*> (x Data..:? "ImageStats")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "ProjectName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable DatasetDescription where
  hashWithSalt _salt DatasetDescription' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` imageStats
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData DatasetDescription where
  rnf DatasetDescription' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf imageStats
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
