{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Dataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.Dataset
  ( Dataset (..),

    -- * Smart constructor
    mkDataset,

    -- * Lenses
    dLastModifiedDate,
    dNumRecords,
    dDataStorage,
    dDatasetName,
    dCreationDate,
    dLastModifiedBy,
    dIdentityId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /See:/ 'mkDataset' smart constructor.
data Dataset = Dataset'
  { lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    numRecords :: Lude.Maybe Lude.Integer,
    dataStorage :: Lude.Maybe Lude.Integer,
    datasetName :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    lastModifiedBy :: Lude.Maybe Lude.Text,
    identityId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Dataset' with the minimum fields required to make a request.
--
-- * 'creationDate' - Date on which the dataset was created.
-- * 'dataStorage' - Total size in bytes of the records in this dataset.
-- * 'datasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
-- * 'identityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
-- * 'lastModifiedBy' - The device that made the last change to this dataset.
-- * 'lastModifiedDate' - Date when the dataset was last modified.
-- * 'numRecords' - Number of records in this dataset.
mkDataset ::
  Dataset
mkDataset =
  Dataset'
    { lastModifiedDate = Lude.Nothing,
      numRecords = Lude.Nothing,
      dataStorage = Lude.Nothing,
      datasetName = Lude.Nothing,
      creationDate = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      identityId = Lude.Nothing
    }

-- | Date when the dataset was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLastModifiedDate :: Lens.Lens' Dataset (Lude.Maybe Lude.Timestamp)
dLastModifiedDate = Lens.lens (lastModifiedDate :: Dataset -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: Dataset)
{-# DEPRECATED dLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Number of records in this dataset.
--
-- /Note:/ Consider using 'numRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNumRecords :: Lens.Lens' Dataset (Lude.Maybe Lude.Integer)
dNumRecords = Lens.lens (numRecords :: Dataset -> Lude.Maybe Lude.Integer) (\s a -> s {numRecords = a} :: Dataset)
{-# DEPRECATED dNumRecords "Use generic-lens or generic-optics with 'numRecords' instead." #-}

-- | Total size in bytes of the records in this dataset.
--
-- /Note:/ Consider using 'dataStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDataStorage :: Lens.Lens' Dataset (Lude.Maybe Lude.Integer)
dDataStorage = Lens.lens (dataStorage :: Dataset -> Lude.Maybe Lude.Integer) (\s a -> s {dataStorage = a} :: Dataset)
{-# DEPRECATED dDataStorage "Use generic-lens or generic-optics with 'dataStorage' instead." #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDatasetName :: Lens.Lens' Dataset (Lude.Maybe Lude.Text)
dDatasetName = Lens.lens (datasetName :: Dataset -> Lude.Maybe Lude.Text) (\s a -> s {datasetName = a} :: Dataset)
{-# DEPRECATED dDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | Date on which the dataset was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreationDate :: Lens.Lens' Dataset (Lude.Maybe Lude.Timestamp)
dCreationDate = Lens.lens (creationDate :: Dataset -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: Dataset)
{-# DEPRECATED dCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The device that made the last change to this dataset.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLastModifiedBy :: Lens.Lens' Dataset (Lude.Maybe Lude.Text)
dLastModifiedBy = Lens.lens (lastModifiedBy :: Dataset -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: Dataset)
{-# DEPRECATED dLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIdentityId :: Lens.Lens' Dataset (Lude.Maybe Lude.Text)
dIdentityId = Lens.lens (identityId :: Dataset -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: Dataset)
{-# DEPRECATED dIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.FromJSON Dataset where
  parseJSON =
    Lude.withObject
      "Dataset"
      ( \x ->
          Dataset'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "NumRecords")
            Lude.<*> (x Lude..:? "DataStorage")
            Lude.<*> (x Lude..:? "DatasetName")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "IdentityId")
      )
