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
-- Module      : Amazonka.Personalize.Types.Dataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.Dataset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides metadata for a dataset.
--
-- /See:/ 'newDataset' smart constructor.
data Dataset = Dataset'
  { -- | The creation date and time (in Unix time) of the dataset.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset that you want metadata
    -- for.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset group.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | One of the following values:
    --
    -- -   Interactions
    --
    -- -   Items
    --
    -- -   Users
    datasetType :: Prelude.Maybe Prelude.Text,
    -- | A time stamp that shows when the dataset was updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the associated schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset.
    --
    -- A dataset can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Dataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'dataset_creationDateTime' - The creation date and time (in Unix time) of the dataset.
--
-- 'datasetArn', 'dataset_datasetArn' - The Amazon Resource Name (ARN) of the dataset that you want metadata
-- for.
--
-- 'datasetGroupArn', 'dataset_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group.
--
-- 'datasetType', 'dataset_datasetType' - One of the following values:
--
-- -   Interactions
--
-- -   Items
--
-- -   Users
--
-- 'lastUpdatedDateTime', 'dataset_lastUpdatedDateTime' - A time stamp that shows when the dataset was updated.
--
-- 'name', 'dataset_name' - The name of the dataset.
--
-- 'schemaArn', 'dataset_schemaArn' - The ARN of the associated schema.
--
-- 'status', 'dataset_status' - The status of the dataset.
--
-- A dataset can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
newDataset ::
  Dataset
newDataset =
  Dataset'
    { creationDateTime = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      datasetType = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The creation date and time (in Unix time) of the dataset.
dataset_creationDateTime :: Lens.Lens' Dataset (Prelude.Maybe Prelude.UTCTime)
dataset_creationDateTime = Lens.lens (\Dataset' {creationDateTime} -> creationDateTime) (\s@Dataset' {} a -> s {creationDateTime = a} :: Dataset) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the dataset that you want metadata
-- for.
dataset_datasetArn :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_datasetArn = Lens.lens (\Dataset' {datasetArn} -> datasetArn) (\s@Dataset' {} a -> s {datasetArn = a} :: Dataset)

-- | The Amazon Resource Name (ARN) of the dataset group.
dataset_datasetGroupArn :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_datasetGroupArn = Lens.lens (\Dataset' {datasetGroupArn} -> datasetGroupArn) (\s@Dataset' {} a -> s {datasetGroupArn = a} :: Dataset)

-- | One of the following values:
--
-- -   Interactions
--
-- -   Items
--
-- -   Users
dataset_datasetType :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_datasetType = Lens.lens (\Dataset' {datasetType} -> datasetType) (\s@Dataset' {} a -> s {datasetType = a} :: Dataset)

-- | A time stamp that shows when the dataset was updated.
dataset_lastUpdatedDateTime :: Lens.Lens' Dataset (Prelude.Maybe Prelude.UTCTime)
dataset_lastUpdatedDateTime = Lens.lens (\Dataset' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@Dataset' {} a -> s {lastUpdatedDateTime = a} :: Dataset) Prelude.. Lens.mapping Data._Time

-- | The name of the dataset.
dataset_name :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_name = Lens.lens (\Dataset' {name} -> name) (\s@Dataset' {} a -> s {name = a} :: Dataset)

-- | The ARN of the associated schema.
dataset_schemaArn :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_schemaArn = Lens.lens (\Dataset' {schemaArn} -> schemaArn) (\s@Dataset' {} a -> s {schemaArn = a} :: Dataset)

-- | The status of the dataset.
--
-- A dataset can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
dataset_status :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_status = Lens.lens (\Dataset' {status} -> status) (\s@Dataset' {} a -> s {status = a} :: Dataset)

instance Data.FromJSON Dataset where
  parseJSON =
    Data.withObject
      "Dataset"
      ( \x ->
          Dataset'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "datasetArn")
            Prelude.<*> (x Data..:? "datasetGroupArn")
            Prelude.<*> (x Data..:? "datasetType")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "schemaArn")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable Dataset where
  hashWithSalt _salt Dataset' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData Dataset where
  rnf Dataset' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf status
