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
-- Module      : Network.AWS.Forecast.Types.DatasetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Forecast.Types.DatasetSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.Forecast.Types.DatasetType
import Network.AWS.Forecast.Types.Domain
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a summary of the dataset properties used in the ListDatasets
-- operation. To get the complete set of properties, call the
-- DescribeDataset operation, and provide the @DatasetArn@.
--
-- /See:/ 'newDatasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'
  { -- | When the dataset was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The domain associated with the dataset.
    domain :: Prelude.Maybe Domain,
    -- | The dataset type.
    datasetType :: Prelude.Maybe DatasetType,
    -- | The name of the dataset.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | When you create a dataset, @LastModificationTime@ is the same as
    -- @CreationTime@. While data is being imported to the dataset,
    -- @LastModificationTime@ is the current time of the @ListDatasets@ call.
    -- After a CreateDatasetImportJob operation has finished,
    -- @LastModificationTime@ is when the import job completed or failed.
    lastModificationTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'datasetSummary_creationTime' - When the dataset was created.
--
-- 'datasetArn', 'datasetSummary_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'domain', 'datasetSummary_domain' - The domain associated with the dataset.
--
-- 'datasetType', 'datasetSummary_datasetType' - The dataset type.
--
-- 'datasetName', 'datasetSummary_datasetName' - The name of the dataset.
--
-- 'lastModificationTime', 'datasetSummary_lastModificationTime' - When you create a dataset, @LastModificationTime@ is the same as
-- @CreationTime@. While data is being imported to the dataset,
-- @LastModificationTime@ is the current time of the @ListDatasets@ call.
-- After a CreateDatasetImportJob operation has finished,
-- @LastModificationTime@ is when the import job completed or failed.
newDatasetSummary ::
  DatasetSummary
newDatasetSummary =
  DatasetSummary'
    { creationTime = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      domain = Prelude.Nothing,
      datasetType = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing
    }

-- | When the dataset was created.
datasetSummary_creationTime :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.UTCTime)
datasetSummary_creationTime = Lens.lens (\DatasetSummary' {creationTime} -> creationTime) (\s@DatasetSummary' {} a -> s {creationTime = a} :: DatasetSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the dataset.
datasetSummary_datasetArn :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_datasetArn = Lens.lens (\DatasetSummary' {datasetArn} -> datasetArn) (\s@DatasetSummary' {} a -> s {datasetArn = a} :: DatasetSummary)

-- | The domain associated with the dataset.
datasetSummary_domain :: Lens.Lens' DatasetSummary (Prelude.Maybe Domain)
datasetSummary_domain = Lens.lens (\DatasetSummary' {domain} -> domain) (\s@DatasetSummary' {} a -> s {domain = a} :: DatasetSummary)

-- | The dataset type.
datasetSummary_datasetType :: Lens.Lens' DatasetSummary (Prelude.Maybe DatasetType)
datasetSummary_datasetType = Lens.lens (\DatasetSummary' {datasetType} -> datasetType) (\s@DatasetSummary' {} a -> s {datasetType = a} :: DatasetSummary)

-- | The name of the dataset.
datasetSummary_datasetName :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_datasetName = Lens.lens (\DatasetSummary' {datasetName} -> datasetName) (\s@DatasetSummary' {} a -> s {datasetName = a} :: DatasetSummary)

-- | When you create a dataset, @LastModificationTime@ is the same as
-- @CreationTime@. While data is being imported to the dataset,
-- @LastModificationTime@ is the current time of the @ListDatasets@ call.
-- After a CreateDatasetImportJob operation has finished,
-- @LastModificationTime@ is when the import job completed or failed.
datasetSummary_lastModificationTime :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.UTCTime)
datasetSummary_lastModificationTime = Lens.lens (\DatasetSummary' {lastModificationTime} -> lastModificationTime) (\s@DatasetSummary' {} a -> s {lastModificationTime = a} :: DatasetSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DatasetSummary where
  parseJSON =
    Core.withObject
      "DatasetSummary"
      ( \x ->
          DatasetSummary'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "DatasetArn")
            Prelude.<*> (x Core..:? "Domain")
            Prelude.<*> (x Core..:? "DatasetType")
            Prelude.<*> (x Core..:? "DatasetName")
            Prelude.<*> (x Core..:? "LastModificationTime")
      )

instance Prelude.Hashable DatasetSummary

instance Prelude.NFData DatasetSummary
