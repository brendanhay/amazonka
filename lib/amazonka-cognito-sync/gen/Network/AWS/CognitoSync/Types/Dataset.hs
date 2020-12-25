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
    dCreationDate,
    dDataStorage,
    dDatasetName,
    dIdentityId,
    dLastModifiedBy,
    dLastModifiedDate,
    dNumRecords,
  )
where

import qualified Network.AWS.CognitoSync.Types.DatasetName as Types
import qualified Network.AWS.CognitoSync.Types.IdentityId as Types
import qualified Network.AWS.CognitoSync.Types.LastModifiedBy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /See:/ 'mkDataset' smart constructor.
data Dataset = Dataset'
  { -- | Date on which the dataset was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | Total size in bytes of the records in this dataset.
    dataStorage :: Core.Maybe Core.Integer,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    datasetName :: Core.Maybe Types.DatasetName,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Core.Maybe Types.IdentityId,
    -- | The device that made the last change to this dataset.
    lastModifiedBy :: Core.Maybe Types.LastModifiedBy,
    -- | Date when the dataset was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | Number of records in this dataset.
    numRecords :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Dataset' value with any optional fields omitted.
mkDataset ::
  Dataset
mkDataset =
  Dataset'
    { creationDate = Core.Nothing,
      dataStorage = Core.Nothing,
      datasetName = Core.Nothing,
      identityId = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      numRecords = Core.Nothing
    }

-- | Date on which the dataset was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreationDate :: Lens.Lens' Dataset (Core.Maybe Core.NominalDiffTime)
dCreationDate = Lens.field @"creationDate"
{-# DEPRECATED dCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Total size in bytes of the records in this dataset.
--
-- /Note:/ Consider using 'dataStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDataStorage :: Lens.Lens' Dataset (Core.Maybe Core.Integer)
dDataStorage = Lens.field @"dataStorage"
{-# DEPRECATED dDataStorage "Use generic-lens or generic-optics with 'dataStorage' instead." #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDatasetName :: Lens.Lens' Dataset (Core.Maybe Types.DatasetName)
dDatasetName = Lens.field @"datasetName"
{-# DEPRECATED dDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIdentityId :: Lens.Lens' Dataset (Core.Maybe Types.IdentityId)
dIdentityId = Lens.field @"identityId"
{-# DEPRECATED dIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The device that made the last change to this dataset.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLastModifiedBy :: Lens.Lens' Dataset (Core.Maybe Types.LastModifiedBy)
dLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED dLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | Date when the dataset was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLastModifiedDate :: Lens.Lens' Dataset (Core.Maybe Core.NominalDiffTime)
dLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED dLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Number of records in this dataset.
--
-- /Note:/ Consider using 'numRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNumRecords :: Lens.Lens' Dataset (Core.Maybe Core.Integer)
dNumRecords = Lens.field @"numRecords"
{-# DEPRECATED dNumRecords "Use generic-lens or generic-optics with 'numRecords' instead." #-}

instance Core.FromJSON Dataset where
  parseJSON =
    Core.withObject "Dataset" Core.$
      \x ->
        Dataset'
          Core.<$> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "DataStorage")
          Core.<*> (x Core..:? "DatasetName")
          Core.<*> (x Core..:? "IdentityId")
          Core.<*> (x Core..:? "LastModifiedBy")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "NumRecords")
