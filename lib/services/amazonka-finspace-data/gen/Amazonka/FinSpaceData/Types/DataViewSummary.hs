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
-- Module      : Amazonka.FinSpaceData.Types.DataViewSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.DataViewSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.DataViewDestinationTypeParams
import Amazonka.FinSpaceData.Types.DataViewErrorInfo
import Amazonka.FinSpaceData.Types.DataViewStatus
import qualified Amazonka.Prelude as Prelude

-- | Structure for the summary of a Dataview.
--
-- /See:/ 'newDataViewSummary' smart constructor.
data DataViewSummary = DataViewSummary'
  { -- | Time range to use for the Dataview. The value is determined as epoch
    -- time in milliseconds. For example, the value for Monday, November 1,
    -- 2021 12:00:00 PM UTC is specified as 1635768000000.
    asOfTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | The flag to indicate Dataview should be updated automatically.
    autoUpdate :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp at which the Dataview was created in FinSpace. The value
    -- is determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createTime :: Prelude.Maybe Prelude.Integer,
    -- | The ARN identifier of the Dataview.
    dataViewArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Dataview.
    dataViewId :: Prelude.Maybe Prelude.Text,
    -- | Th unique identifier for the Dataview Dataset.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | Information about the Dataview destination.
    destinationTypeProperties :: Prelude.Maybe DataViewDestinationTypeParams,
    -- | The structure with error messages.
    errorInfo :: Prelude.Maybe DataViewErrorInfo,
    -- | The last time that a Dataview was modified. The value is determined as
    -- epoch time in milliseconds. For example, the value for Monday, November
    -- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | Ordered set of column names used to partition data.
    partitionColumns :: Prelude.Maybe [Prelude.Text],
    -- | Columns to be used for sorting the data.
    sortColumns :: Prelude.Maybe [Prelude.Text],
    -- | The status of a Dataview creation.
    --
    -- -   @RUNNING@ – Dataview creation is running.
    --
    -- -   @STARTING@ – Dataview creation is starting.
    --
    -- -   @FAILED@ – Dataview creation has failed.
    --
    -- -   @CANCELLED@ – Dataview creation has been cancelled.
    --
    -- -   @TIMEOUT@ – Dataview creation has timed out.
    --
    -- -   @SUCCESS@ – Dataview creation has succeeded.
    --
    -- -   @PENDING@ – Dataview creation is pending.
    --
    -- -   @FAILED_CLEANUP_FAILED@ – Dataview creation failed and resource
    --     cleanup failed.
    status :: Prelude.Maybe DataViewStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataViewSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'asOfTimestamp', 'dataViewSummary_asOfTimestamp' - Time range to use for the Dataview. The value is determined as epoch
-- time in milliseconds. For example, the value for Monday, November 1,
-- 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'autoUpdate', 'dataViewSummary_autoUpdate' - The flag to indicate Dataview should be updated automatically.
--
-- 'createTime', 'dataViewSummary_createTime' - The timestamp at which the Dataview was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'dataViewArn', 'dataViewSummary_dataViewArn' - The ARN identifier of the Dataview.
--
-- 'dataViewId', 'dataViewSummary_dataViewId' - The unique identifier for the Dataview.
--
-- 'datasetId', 'dataViewSummary_datasetId' - Th unique identifier for the Dataview Dataset.
--
-- 'destinationTypeProperties', 'dataViewSummary_destinationTypeProperties' - Information about the Dataview destination.
--
-- 'errorInfo', 'dataViewSummary_errorInfo' - The structure with error messages.
--
-- 'lastModifiedTime', 'dataViewSummary_lastModifiedTime' - The last time that a Dataview was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'partitionColumns', 'dataViewSummary_partitionColumns' - Ordered set of column names used to partition data.
--
-- 'sortColumns', 'dataViewSummary_sortColumns' - Columns to be used for sorting the data.
--
-- 'status', 'dataViewSummary_status' - The status of a Dataview creation.
--
-- -   @RUNNING@ – Dataview creation is running.
--
-- -   @STARTING@ – Dataview creation is starting.
--
-- -   @FAILED@ – Dataview creation has failed.
--
-- -   @CANCELLED@ – Dataview creation has been cancelled.
--
-- -   @TIMEOUT@ – Dataview creation has timed out.
--
-- -   @SUCCESS@ – Dataview creation has succeeded.
--
-- -   @PENDING@ – Dataview creation is pending.
--
-- -   @FAILED_CLEANUP_FAILED@ – Dataview creation failed and resource
--     cleanup failed.
newDataViewSummary ::
  DataViewSummary
newDataViewSummary =
  DataViewSummary'
    { asOfTimestamp = Prelude.Nothing,
      autoUpdate = Prelude.Nothing,
      createTime = Prelude.Nothing,
      dataViewArn = Prelude.Nothing,
      dataViewId = Prelude.Nothing,
      datasetId = Prelude.Nothing,
      destinationTypeProperties = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      partitionColumns = Prelude.Nothing,
      sortColumns = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Time range to use for the Dataview. The value is determined as epoch
-- time in milliseconds. For example, the value for Monday, November 1,
-- 2021 12:00:00 PM UTC is specified as 1635768000000.
dataViewSummary_asOfTimestamp :: Lens.Lens' DataViewSummary (Prelude.Maybe Prelude.Integer)
dataViewSummary_asOfTimestamp = Lens.lens (\DataViewSummary' {asOfTimestamp} -> asOfTimestamp) (\s@DataViewSummary' {} a -> s {asOfTimestamp = a} :: DataViewSummary)

-- | The flag to indicate Dataview should be updated automatically.
dataViewSummary_autoUpdate :: Lens.Lens' DataViewSummary (Prelude.Maybe Prelude.Bool)
dataViewSummary_autoUpdate = Lens.lens (\DataViewSummary' {autoUpdate} -> autoUpdate) (\s@DataViewSummary' {} a -> s {autoUpdate = a} :: DataViewSummary)

-- | The timestamp at which the Dataview was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
dataViewSummary_createTime :: Lens.Lens' DataViewSummary (Prelude.Maybe Prelude.Integer)
dataViewSummary_createTime = Lens.lens (\DataViewSummary' {createTime} -> createTime) (\s@DataViewSummary' {} a -> s {createTime = a} :: DataViewSummary)

-- | The ARN identifier of the Dataview.
dataViewSummary_dataViewArn :: Lens.Lens' DataViewSummary (Prelude.Maybe Prelude.Text)
dataViewSummary_dataViewArn = Lens.lens (\DataViewSummary' {dataViewArn} -> dataViewArn) (\s@DataViewSummary' {} a -> s {dataViewArn = a} :: DataViewSummary)

-- | The unique identifier for the Dataview.
dataViewSummary_dataViewId :: Lens.Lens' DataViewSummary (Prelude.Maybe Prelude.Text)
dataViewSummary_dataViewId = Lens.lens (\DataViewSummary' {dataViewId} -> dataViewId) (\s@DataViewSummary' {} a -> s {dataViewId = a} :: DataViewSummary)

-- | Th unique identifier for the Dataview Dataset.
dataViewSummary_datasetId :: Lens.Lens' DataViewSummary (Prelude.Maybe Prelude.Text)
dataViewSummary_datasetId = Lens.lens (\DataViewSummary' {datasetId} -> datasetId) (\s@DataViewSummary' {} a -> s {datasetId = a} :: DataViewSummary)

-- | Information about the Dataview destination.
dataViewSummary_destinationTypeProperties :: Lens.Lens' DataViewSummary (Prelude.Maybe DataViewDestinationTypeParams)
dataViewSummary_destinationTypeProperties = Lens.lens (\DataViewSummary' {destinationTypeProperties} -> destinationTypeProperties) (\s@DataViewSummary' {} a -> s {destinationTypeProperties = a} :: DataViewSummary)

-- | The structure with error messages.
dataViewSummary_errorInfo :: Lens.Lens' DataViewSummary (Prelude.Maybe DataViewErrorInfo)
dataViewSummary_errorInfo = Lens.lens (\DataViewSummary' {errorInfo} -> errorInfo) (\s@DataViewSummary' {} a -> s {errorInfo = a} :: DataViewSummary)

-- | The last time that a Dataview was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
dataViewSummary_lastModifiedTime :: Lens.Lens' DataViewSummary (Prelude.Maybe Prelude.Integer)
dataViewSummary_lastModifiedTime = Lens.lens (\DataViewSummary' {lastModifiedTime} -> lastModifiedTime) (\s@DataViewSummary' {} a -> s {lastModifiedTime = a} :: DataViewSummary)

-- | Ordered set of column names used to partition data.
dataViewSummary_partitionColumns :: Lens.Lens' DataViewSummary (Prelude.Maybe [Prelude.Text])
dataViewSummary_partitionColumns = Lens.lens (\DataViewSummary' {partitionColumns} -> partitionColumns) (\s@DataViewSummary' {} a -> s {partitionColumns = a} :: DataViewSummary) Prelude.. Lens.mapping Lens.coerced

-- | Columns to be used for sorting the data.
dataViewSummary_sortColumns :: Lens.Lens' DataViewSummary (Prelude.Maybe [Prelude.Text])
dataViewSummary_sortColumns = Lens.lens (\DataViewSummary' {sortColumns} -> sortColumns) (\s@DataViewSummary' {} a -> s {sortColumns = a} :: DataViewSummary) Prelude.. Lens.mapping Lens.coerced

-- | The status of a Dataview creation.
--
-- -   @RUNNING@ – Dataview creation is running.
--
-- -   @STARTING@ – Dataview creation is starting.
--
-- -   @FAILED@ – Dataview creation has failed.
--
-- -   @CANCELLED@ – Dataview creation has been cancelled.
--
-- -   @TIMEOUT@ – Dataview creation has timed out.
--
-- -   @SUCCESS@ – Dataview creation has succeeded.
--
-- -   @PENDING@ – Dataview creation is pending.
--
-- -   @FAILED_CLEANUP_FAILED@ – Dataview creation failed and resource
--     cleanup failed.
dataViewSummary_status :: Lens.Lens' DataViewSummary (Prelude.Maybe DataViewStatus)
dataViewSummary_status = Lens.lens (\DataViewSummary' {status} -> status) (\s@DataViewSummary' {} a -> s {status = a} :: DataViewSummary)

instance Data.FromJSON DataViewSummary where
  parseJSON =
    Data.withObject
      "DataViewSummary"
      ( \x ->
          DataViewSummary'
            Prelude.<$> (x Data..:? "asOfTimestamp")
            Prelude.<*> (x Data..:? "autoUpdate")
            Prelude.<*> (x Data..:? "createTime")
            Prelude.<*> (x Data..:? "dataViewArn")
            Prelude.<*> (x Data..:? "dataViewId")
            Prelude.<*> (x Data..:? "datasetId")
            Prelude.<*> (x Data..:? "destinationTypeProperties")
            Prelude.<*> (x Data..:? "errorInfo")
            Prelude.<*> (x Data..:? "lastModifiedTime")
            Prelude.<*> ( x Data..:? "partitionColumns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "sortColumns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DataViewSummary where
  hashWithSalt _salt DataViewSummary' {..} =
    _salt `Prelude.hashWithSalt` asOfTimestamp
      `Prelude.hashWithSalt` autoUpdate
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` dataViewArn
      `Prelude.hashWithSalt` dataViewId
      `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` destinationTypeProperties
      `Prelude.hashWithSalt` errorInfo
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` partitionColumns
      `Prelude.hashWithSalt` sortColumns
      `Prelude.hashWithSalt` status

instance Prelude.NFData DataViewSummary where
  rnf DataViewSummary' {..} =
    Prelude.rnf asOfTimestamp
      `Prelude.seq` Prelude.rnf autoUpdate
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf dataViewArn
      `Prelude.seq` Prelude.rnf dataViewId
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf destinationTypeProperties
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf partitionColumns
      `Prelude.seq` Prelude.rnf sortColumns
      `Prelude.seq` Prelude.rnf status
