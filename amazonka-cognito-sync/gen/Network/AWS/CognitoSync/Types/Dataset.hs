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
-- Module      : Network.AWS.CognitoSync.Types.Dataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.Dataset where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A collection of data for an identity pool. An identity pool can have
-- multiple datasets. A dataset is per identity and can be general or
-- associated with a particular entity in an application (like a saved
-- game). Datasets are automatically created if they don\'t exist. Data is
-- synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /See:/ 'newDataset' smart constructor.
data Dataset = Dataset'
  { -- | Date when the dataset was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | Number of records in this dataset.
    numRecords :: Core.Maybe Core.Integer,
    -- | Date on which the dataset was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | Total size in bytes of the records in this dataset.
    dataStorage :: Core.Maybe Core.Integer,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Core.Maybe Core.Text,
    -- | The device that made the last change to this dataset.
    lastModifiedBy :: Core.Maybe Core.Text,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
    -- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
    datasetName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Dataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'dataset_lastModifiedDate' - Date when the dataset was last modified.
--
-- 'numRecords', 'dataset_numRecords' - Number of records in this dataset.
--
-- 'creationDate', 'dataset_creationDate' - Date on which the dataset was created.
--
-- 'dataStorage', 'dataset_dataStorage' - Total size in bytes of the records in this dataset.
--
-- 'identityId', 'dataset_identityId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'lastModifiedBy', 'dataset_lastModifiedBy' - The device that made the last change to this dataset.
--
-- 'datasetName', 'dataset_datasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
newDataset ::
  Dataset
newDataset =
  Dataset'
    { lastModifiedDate = Core.Nothing,
      numRecords = Core.Nothing,
      creationDate = Core.Nothing,
      dataStorage = Core.Nothing,
      identityId = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      datasetName = Core.Nothing
    }

-- | Date when the dataset was last modified.
dataset_lastModifiedDate :: Lens.Lens' Dataset (Core.Maybe Core.UTCTime)
dataset_lastModifiedDate = Lens.lens (\Dataset' {lastModifiedDate} -> lastModifiedDate) (\s@Dataset' {} a -> s {lastModifiedDate = a} :: Dataset) Core.. Lens.mapping Core._Time

-- | Number of records in this dataset.
dataset_numRecords :: Lens.Lens' Dataset (Core.Maybe Core.Integer)
dataset_numRecords = Lens.lens (\Dataset' {numRecords} -> numRecords) (\s@Dataset' {} a -> s {numRecords = a} :: Dataset)

-- | Date on which the dataset was created.
dataset_creationDate :: Lens.Lens' Dataset (Core.Maybe Core.UTCTime)
dataset_creationDate = Lens.lens (\Dataset' {creationDate} -> creationDate) (\s@Dataset' {} a -> s {creationDate = a} :: Dataset) Core.. Lens.mapping Core._Time

-- | Total size in bytes of the records in this dataset.
dataset_dataStorage :: Lens.Lens' Dataset (Core.Maybe Core.Integer)
dataset_dataStorage = Lens.lens (\Dataset' {dataStorage} -> dataStorage) (\s@Dataset' {} a -> s {dataStorage = a} :: Dataset)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
dataset_identityId :: Lens.Lens' Dataset (Core.Maybe Core.Text)
dataset_identityId = Lens.lens (\Dataset' {identityId} -> identityId) (\s@Dataset' {} a -> s {identityId = a} :: Dataset)

-- | The device that made the last change to this dataset.
dataset_lastModifiedBy :: Lens.Lens' Dataset (Core.Maybe Core.Text)
dataset_lastModifiedBy = Lens.lens (\Dataset' {lastModifiedBy} -> lastModifiedBy) (\s@Dataset' {} a -> s {lastModifiedBy = a} :: Dataset)

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
dataset_datasetName :: Lens.Lens' Dataset (Core.Maybe Core.Text)
dataset_datasetName = Lens.lens (\Dataset' {datasetName} -> datasetName) (\s@Dataset' {} a -> s {datasetName = a} :: Dataset)

instance Core.FromJSON Dataset where
  parseJSON =
    Core.withObject
      "Dataset"
      ( \x ->
          Dataset'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "NumRecords")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "DataStorage")
            Core.<*> (x Core..:? "IdentityId")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "DatasetName")
      )

instance Core.Hashable Dataset

instance Core.NFData Dataset
