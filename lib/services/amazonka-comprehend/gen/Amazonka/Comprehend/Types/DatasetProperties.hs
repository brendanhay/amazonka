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
-- Module      : Amazonka.Comprehend.Types.DatasetProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetProperties where

import Amazonka.Comprehend.Types.DatasetStatus
import Amazonka.Comprehend.Types.DatasetType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Properties associated with the dataset.
--
-- /See:/ 'newDatasetProperties' smart constructor.
data DatasetProperties = DatasetProperties'
  { -- | Creation time of the dataset.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The S3 URI where the dataset is stored.
    datasetS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The dataset type (training data or test data).
    datasetType :: Prelude.Maybe DatasetType,
    -- | Description of the dataset.
    description :: Prelude.Maybe Prelude.Text,
    -- | Time when the data from the dataset becomes available in the data lake.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | A description of the status of the dataset.
    message :: Prelude.Maybe Prelude.Text,
    -- | The number of documents in the dataset.
    numberOfDocuments :: Prelude.Maybe Prelude.Integer,
    -- | The dataset status. While the system creates the dataset, the status is
    -- @CREATING@. When the dataset is ready to use, the status changes to
    -- @COMPLETED@.
    status :: Prelude.Maybe DatasetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'datasetProperties_creationTime' - Creation time of the dataset.
--
-- 'datasetArn', 'datasetProperties_datasetArn' - The ARN of the dataset.
--
-- 'datasetName', 'datasetProperties_datasetName' - The name of the dataset.
--
-- 'datasetS3Uri', 'datasetProperties_datasetS3Uri' - The S3 URI where the dataset is stored.
--
-- 'datasetType', 'datasetProperties_datasetType' - The dataset type (training data or test data).
--
-- 'description', 'datasetProperties_description' - Description of the dataset.
--
-- 'endTime', 'datasetProperties_endTime' - Time when the data from the dataset becomes available in the data lake.
--
-- 'message', 'datasetProperties_message' - A description of the status of the dataset.
--
-- 'numberOfDocuments', 'datasetProperties_numberOfDocuments' - The number of documents in the dataset.
--
-- 'status', 'datasetProperties_status' - The dataset status. While the system creates the dataset, the status is
-- @CREATING@. When the dataset is ready to use, the status changes to
-- @COMPLETED@.
newDatasetProperties ::
  DatasetProperties
newDatasetProperties =
  DatasetProperties'
    { creationTime = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      datasetS3Uri = Prelude.Nothing,
      datasetType = Prelude.Nothing,
      description = Prelude.Nothing,
      endTime = Prelude.Nothing,
      message = Prelude.Nothing,
      numberOfDocuments = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Creation time of the dataset.
datasetProperties_creationTime :: Lens.Lens' DatasetProperties (Prelude.Maybe Prelude.UTCTime)
datasetProperties_creationTime = Lens.lens (\DatasetProperties' {creationTime} -> creationTime) (\s@DatasetProperties' {} a -> s {creationTime = a} :: DatasetProperties) Prelude.. Lens.mapping Data._Time

-- | The ARN of the dataset.
datasetProperties_datasetArn :: Lens.Lens' DatasetProperties (Prelude.Maybe Prelude.Text)
datasetProperties_datasetArn = Lens.lens (\DatasetProperties' {datasetArn} -> datasetArn) (\s@DatasetProperties' {} a -> s {datasetArn = a} :: DatasetProperties)

-- | The name of the dataset.
datasetProperties_datasetName :: Lens.Lens' DatasetProperties (Prelude.Maybe Prelude.Text)
datasetProperties_datasetName = Lens.lens (\DatasetProperties' {datasetName} -> datasetName) (\s@DatasetProperties' {} a -> s {datasetName = a} :: DatasetProperties)

-- | The S3 URI where the dataset is stored.
datasetProperties_datasetS3Uri :: Lens.Lens' DatasetProperties (Prelude.Maybe Prelude.Text)
datasetProperties_datasetS3Uri = Lens.lens (\DatasetProperties' {datasetS3Uri} -> datasetS3Uri) (\s@DatasetProperties' {} a -> s {datasetS3Uri = a} :: DatasetProperties)

-- | The dataset type (training data or test data).
datasetProperties_datasetType :: Lens.Lens' DatasetProperties (Prelude.Maybe DatasetType)
datasetProperties_datasetType = Lens.lens (\DatasetProperties' {datasetType} -> datasetType) (\s@DatasetProperties' {} a -> s {datasetType = a} :: DatasetProperties)

-- | Description of the dataset.
datasetProperties_description :: Lens.Lens' DatasetProperties (Prelude.Maybe Prelude.Text)
datasetProperties_description = Lens.lens (\DatasetProperties' {description} -> description) (\s@DatasetProperties' {} a -> s {description = a} :: DatasetProperties)

-- | Time when the data from the dataset becomes available in the data lake.
datasetProperties_endTime :: Lens.Lens' DatasetProperties (Prelude.Maybe Prelude.UTCTime)
datasetProperties_endTime = Lens.lens (\DatasetProperties' {endTime} -> endTime) (\s@DatasetProperties' {} a -> s {endTime = a} :: DatasetProperties) Prelude.. Lens.mapping Data._Time

-- | A description of the status of the dataset.
datasetProperties_message :: Lens.Lens' DatasetProperties (Prelude.Maybe Prelude.Text)
datasetProperties_message = Lens.lens (\DatasetProperties' {message} -> message) (\s@DatasetProperties' {} a -> s {message = a} :: DatasetProperties)

-- | The number of documents in the dataset.
datasetProperties_numberOfDocuments :: Lens.Lens' DatasetProperties (Prelude.Maybe Prelude.Integer)
datasetProperties_numberOfDocuments = Lens.lens (\DatasetProperties' {numberOfDocuments} -> numberOfDocuments) (\s@DatasetProperties' {} a -> s {numberOfDocuments = a} :: DatasetProperties)

-- | The dataset status. While the system creates the dataset, the status is
-- @CREATING@. When the dataset is ready to use, the status changes to
-- @COMPLETED@.
datasetProperties_status :: Lens.Lens' DatasetProperties (Prelude.Maybe DatasetStatus)
datasetProperties_status = Lens.lens (\DatasetProperties' {status} -> status) (\s@DatasetProperties' {} a -> s {status = a} :: DatasetProperties)

instance Data.FromJSON DatasetProperties where
  parseJSON =
    Data.withObject
      "DatasetProperties"
      ( \x ->
          DatasetProperties'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DatasetArn")
            Prelude.<*> (x Data..:? "DatasetName")
            Prelude.<*> (x Data..:? "DatasetS3Uri")
            Prelude.<*> (x Data..:? "DatasetType")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "NumberOfDocuments")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable DatasetProperties where
  hashWithSalt _salt DatasetProperties' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` datasetS3Uri
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` numberOfDocuments
      `Prelude.hashWithSalt` status

instance Prelude.NFData DatasetProperties where
  rnf DatasetProperties' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf datasetS3Uri
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf numberOfDocuments
      `Prelude.seq` Prelude.rnf status
