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
-- Module      : Amazonka.Personalize.Types.DatasetSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a dataset. For a complete
-- listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDataset.html DescribeDataset>
-- API.
--
-- /See:/ 'newDatasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'
  { -- | The name of the dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the dataset was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The dataset type. One of the following values:
    --
    -- -   Interactions
    --
    -- -   Items
    --
    -- -   Users
    --
    -- -   Event-Interactions
    datasetType :: Prelude.Maybe Prelude.Text,
    -- | The status of the dataset.
    --
    -- A dataset can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the dataset was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
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
-- 'name', 'datasetSummary_name' - The name of the dataset.
--
-- 'creationDateTime', 'datasetSummary_creationDateTime' - The date and time (in Unix time) that the dataset was created.
--
-- 'datasetType', 'datasetSummary_datasetType' - The dataset type. One of the following values:
--
-- -   Interactions
--
-- -   Items
--
-- -   Users
--
-- -   Event-Interactions
--
-- 'status', 'datasetSummary_status' - The status of the dataset.
--
-- A dataset can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- 'datasetArn', 'datasetSummary_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'lastUpdatedDateTime', 'datasetSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the dataset was last updated.
newDatasetSummary ::
  DatasetSummary
newDatasetSummary =
  DatasetSummary'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      datasetType = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The name of the dataset.
datasetSummary_name :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_name = Lens.lens (\DatasetSummary' {name} -> name) (\s@DatasetSummary' {} a -> s {name = a} :: DatasetSummary)

-- | The date and time (in Unix time) that the dataset was created.
datasetSummary_creationDateTime :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.UTCTime)
datasetSummary_creationDateTime = Lens.lens (\DatasetSummary' {creationDateTime} -> creationDateTime) (\s@DatasetSummary' {} a -> s {creationDateTime = a} :: DatasetSummary) Prelude.. Lens.mapping Core._Time

-- | The dataset type. One of the following values:
--
-- -   Interactions
--
-- -   Items
--
-- -   Users
--
-- -   Event-Interactions
datasetSummary_datasetType :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_datasetType = Lens.lens (\DatasetSummary' {datasetType} -> datasetType) (\s@DatasetSummary' {} a -> s {datasetType = a} :: DatasetSummary)

-- | The status of the dataset.
--
-- A dataset can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
datasetSummary_status :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_status = Lens.lens (\DatasetSummary' {status} -> status) (\s@DatasetSummary' {} a -> s {status = a} :: DatasetSummary)

-- | The Amazon Resource Name (ARN) of the dataset.
datasetSummary_datasetArn :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_datasetArn = Lens.lens (\DatasetSummary' {datasetArn} -> datasetArn) (\s@DatasetSummary' {} a -> s {datasetArn = a} :: DatasetSummary)

-- | The date and time (in Unix time) that the dataset was last updated.
datasetSummary_lastUpdatedDateTime :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.UTCTime)
datasetSummary_lastUpdatedDateTime = Lens.lens (\DatasetSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetSummary' {} a -> s {lastUpdatedDateTime = a} :: DatasetSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DatasetSummary where
  parseJSON =
    Core.withObject
      "DatasetSummary"
      ( \x ->
          DatasetSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "datasetType")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "datasetArn")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable DatasetSummary where
  hashWithSalt _salt DatasetSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData DatasetSummary where
  rnf DatasetSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
