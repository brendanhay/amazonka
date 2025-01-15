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
-- Module      : Amazonka.CognitoSync.Types.Dataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types.Dataset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A collection of data for an identity pool. An identity pool can have
-- multiple datasets. A dataset is per identity and can be general or
-- associated with a particular entity in an application (like a saved
-- game). Datasets are automatically created if they don\'t exist. Data is
-- synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /See:/ 'newDataset' smart constructor.
data Dataset = Dataset'
  { -- | Date on which the dataset was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | Total size in bytes of the records in this dataset.
    dataStorage :: Prelude.Maybe Prelude.Integer,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
    -- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. GUID generation is unique within a region.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The device that made the last change to this dataset.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | Date when the dataset was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Number of records in this dataset.
    numRecords :: Prelude.Maybe Prelude.Integer
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
-- 'creationDate', 'dataset_creationDate' - Date on which the dataset was created.
--
-- 'dataStorage', 'dataset_dataStorage' - Total size in bytes of the records in this dataset.
--
-- 'datasetName', 'dataset_datasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
--
-- 'identityId', 'dataset_identityId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
--
-- 'lastModifiedBy', 'dataset_lastModifiedBy' - The device that made the last change to this dataset.
--
-- 'lastModifiedDate', 'dataset_lastModifiedDate' - Date when the dataset was last modified.
--
-- 'numRecords', 'dataset_numRecords' - Number of records in this dataset.
newDataset ::
  Dataset
newDataset =
  Dataset'
    { creationDate = Prelude.Nothing,
      dataStorage = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      identityId = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      numRecords = Prelude.Nothing
    }

-- | Date on which the dataset was created.
dataset_creationDate :: Lens.Lens' Dataset (Prelude.Maybe Prelude.UTCTime)
dataset_creationDate = Lens.lens (\Dataset' {creationDate} -> creationDate) (\s@Dataset' {} a -> s {creationDate = a} :: Dataset) Prelude.. Lens.mapping Data._Time

-- | Total size in bytes of the records in this dataset.
dataset_dataStorage :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Integer)
dataset_dataStorage = Lens.lens (\Dataset' {dataStorage} -> dataStorage) (\s@Dataset' {} a -> s {dataStorage = a} :: Dataset)

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
dataset_datasetName :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_datasetName = Lens.lens (\Dataset' {datasetName} -> datasetName) (\s@Dataset' {} a -> s {datasetName = a} :: Dataset)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
dataset_identityId :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_identityId = Lens.lens (\Dataset' {identityId} -> identityId) (\s@Dataset' {} a -> s {identityId = a} :: Dataset)

-- | The device that made the last change to this dataset.
dataset_lastModifiedBy :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Text)
dataset_lastModifiedBy = Lens.lens (\Dataset' {lastModifiedBy} -> lastModifiedBy) (\s@Dataset' {} a -> s {lastModifiedBy = a} :: Dataset)

-- | Date when the dataset was last modified.
dataset_lastModifiedDate :: Lens.Lens' Dataset (Prelude.Maybe Prelude.UTCTime)
dataset_lastModifiedDate = Lens.lens (\Dataset' {lastModifiedDate} -> lastModifiedDate) (\s@Dataset' {} a -> s {lastModifiedDate = a} :: Dataset) Prelude.. Lens.mapping Data._Time

-- | Number of records in this dataset.
dataset_numRecords :: Lens.Lens' Dataset (Prelude.Maybe Prelude.Integer)
dataset_numRecords = Lens.lens (\Dataset' {numRecords} -> numRecords) (\s@Dataset' {} a -> s {numRecords = a} :: Dataset)

instance Data.FromJSON Dataset where
  parseJSON =
    Data.withObject
      "Dataset"
      ( \x ->
          Dataset'
            Prelude.<$> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "DataStorage")
            Prelude.<*> (x Data..:? "DatasetName")
            Prelude.<*> (x Data..:? "IdentityId")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "NumRecords")
      )

instance Prelude.Hashable Dataset where
  hashWithSalt _salt Dataset' {..} =
    _salt
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` dataStorage
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` numRecords

instance Prelude.NFData Dataset where
  rnf Dataset' {..} =
    Prelude.rnf creationDate `Prelude.seq`
      Prelude.rnf dataStorage `Prelude.seq`
        Prelude.rnf datasetName `Prelude.seq`
          Prelude.rnf identityId `Prelude.seq`
            Prelude.rnf lastModifiedBy `Prelude.seq`
              Prelude.rnf lastModifiedDate `Prelude.seq`
                Prelude.rnf numRecords
