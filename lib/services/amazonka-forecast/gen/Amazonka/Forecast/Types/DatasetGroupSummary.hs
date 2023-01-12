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
-- Module      : Amazonka.Forecast.Types.DatasetGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.DatasetGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the dataset group properties used in the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_ListDatasetGroups.html ListDatasetGroups>
-- operation. To get the complete set of properties, call the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_DescribeDatasetGroup.html DescribeDatasetGroup>
-- operation, and provide the @DatasetGroupArn@.
--
-- /See:/ 'newDatasetGroupSummary' smart constructor.
data DatasetGroupSummary = DatasetGroupSummary'
  { -- | When the dataset group was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset group.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset group.
    datasetGroupName :: Prelude.Maybe Prelude.Text,
    -- | When the dataset group was created or last updated from a call to the
    -- <https://docs.aws.amazon.com/forecast/latest/dg/API_UpdateDatasetGroup.html UpdateDatasetGroup>
    -- operation. While the dataset group is being updated,
    -- @LastModificationTime@ is the current time of the @ListDatasetGroups@
    -- call.
    lastModificationTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'datasetGroupSummary_creationTime' - When the dataset group was created.
--
-- 'datasetGroupArn', 'datasetGroupSummary_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group.
--
-- 'datasetGroupName', 'datasetGroupSummary_datasetGroupName' - The name of the dataset group.
--
-- 'lastModificationTime', 'datasetGroupSummary_lastModificationTime' - When the dataset group was created or last updated from a call to the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_UpdateDatasetGroup.html UpdateDatasetGroup>
-- operation. While the dataset group is being updated,
-- @LastModificationTime@ is the current time of the @ListDatasetGroups@
-- call.
newDatasetGroupSummary ::
  DatasetGroupSummary
newDatasetGroupSummary =
  DatasetGroupSummary'
    { creationTime =
        Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      datasetGroupName = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing
    }

-- | When the dataset group was created.
datasetGroupSummary_creationTime :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.UTCTime)
datasetGroupSummary_creationTime = Lens.lens (\DatasetGroupSummary' {creationTime} -> creationTime) (\s@DatasetGroupSummary' {} a -> s {creationTime = a} :: DatasetGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the dataset group.
datasetGroupSummary_datasetGroupArn :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.Text)
datasetGroupSummary_datasetGroupArn = Lens.lens (\DatasetGroupSummary' {datasetGroupArn} -> datasetGroupArn) (\s@DatasetGroupSummary' {} a -> s {datasetGroupArn = a} :: DatasetGroupSummary)

-- | The name of the dataset group.
datasetGroupSummary_datasetGroupName :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.Text)
datasetGroupSummary_datasetGroupName = Lens.lens (\DatasetGroupSummary' {datasetGroupName} -> datasetGroupName) (\s@DatasetGroupSummary' {} a -> s {datasetGroupName = a} :: DatasetGroupSummary)

-- | When the dataset group was created or last updated from a call to the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_UpdateDatasetGroup.html UpdateDatasetGroup>
-- operation. While the dataset group is being updated,
-- @LastModificationTime@ is the current time of the @ListDatasetGroups@
-- call.
datasetGroupSummary_lastModificationTime :: Lens.Lens' DatasetGroupSummary (Prelude.Maybe Prelude.UTCTime)
datasetGroupSummary_lastModificationTime = Lens.lens (\DatasetGroupSummary' {lastModificationTime} -> lastModificationTime) (\s@DatasetGroupSummary' {} a -> s {lastModificationTime = a} :: DatasetGroupSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DatasetGroupSummary where
  parseJSON =
    Data.withObject
      "DatasetGroupSummary"
      ( \x ->
          DatasetGroupSummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DatasetGroupArn")
            Prelude.<*> (x Data..:? "DatasetGroupName")
            Prelude.<*> (x Data..:? "LastModificationTime")
      )

instance Prelude.Hashable DatasetGroupSummary where
  hashWithSalt _salt DatasetGroupSummary' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` datasetGroupName
      `Prelude.hashWithSalt` lastModificationTime

instance Prelude.NFData DatasetGroupSummary where
  rnf DatasetGroupSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf datasetGroupName
      `Prelude.seq` Prelude.rnf lastModificationTime
