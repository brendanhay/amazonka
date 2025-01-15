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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DatasetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a dataset. For a complete
-- listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDataset.html DescribeDataset>
-- API.
--
-- /See:/ 'newDatasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'
  { -- | The date and time (in Unix time) that the dataset was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
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
    -- | The date and time (in Unix time) that the dataset was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the dataset.
    name :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'DatasetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'datasetSummary_creationDateTime' - The date and time (in Unix time) that the dataset was created.
--
-- 'datasetArn', 'datasetSummary_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
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
-- 'lastUpdatedDateTime', 'datasetSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the dataset was last updated.
--
-- 'name', 'datasetSummary_name' - The name of the dataset.
--
-- 'status', 'datasetSummary_status' - The status of the dataset.
--
-- A dataset can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
newDatasetSummary ::
  DatasetSummary
newDatasetSummary =
  DatasetSummary'
    { creationDateTime = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      datasetType = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time (in Unix time) that the dataset was created.
datasetSummary_creationDateTime :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.UTCTime)
datasetSummary_creationDateTime = Lens.lens (\DatasetSummary' {creationDateTime} -> creationDateTime) (\s@DatasetSummary' {} a -> s {creationDateTime = a} :: DatasetSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the dataset.
datasetSummary_datasetArn :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_datasetArn = Lens.lens (\DatasetSummary' {datasetArn} -> datasetArn) (\s@DatasetSummary' {} a -> s {datasetArn = a} :: DatasetSummary)

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

-- | The date and time (in Unix time) that the dataset was last updated.
datasetSummary_lastUpdatedDateTime :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.UTCTime)
datasetSummary_lastUpdatedDateTime = Lens.lens (\DatasetSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DatasetSummary' {} a -> s {lastUpdatedDateTime = a} :: DatasetSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the dataset.
datasetSummary_name :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_name = Lens.lens (\DatasetSummary' {name} -> name) (\s@DatasetSummary' {} a -> s {name = a} :: DatasetSummary)

-- | The status of the dataset.
--
-- A dataset can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
datasetSummary_status :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_status = Lens.lens (\DatasetSummary' {status} -> status) (\s@DatasetSummary' {} a -> s {status = a} :: DatasetSummary)

instance Data.FromJSON DatasetSummary where
  parseJSON =
    Data.withObject
      "DatasetSummary"
      ( \x ->
          DatasetSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "datasetArn")
            Prelude.<*> (x Data..:? "datasetType")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DatasetSummary where
  hashWithSalt _salt DatasetSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData DatasetSummary where
  rnf DatasetSummary' {..} =
    Prelude.rnf creationDateTime `Prelude.seq`
      Prelude.rnf datasetArn `Prelude.seq`
        Prelude.rnf datasetType `Prelude.seq`
          Prelude.rnf lastUpdatedDateTime `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf status
