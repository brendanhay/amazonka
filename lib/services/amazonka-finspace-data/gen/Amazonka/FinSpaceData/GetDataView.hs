{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpaceData.GetDataView
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Dataview.
module Amazonka.FinSpaceData.GetDataView
  ( -- * Creating a Request
    GetDataView (..),
    newGetDataView,

    -- * Request Lenses
    getDataView_dataViewId,
    getDataView_datasetId,

    -- * Destructuring the Response
    GetDataViewResponse (..),
    newGetDataViewResponse,

    -- * Response Lenses
    getDataViewResponse_asOfTimestamp,
    getDataViewResponse_autoUpdate,
    getDataViewResponse_createTime,
    getDataViewResponse_dataViewArn,
    getDataViewResponse_dataViewId,
    getDataViewResponse_datasetId,
    getDataViewResponse_destinationTypeParams,
    getDataViewResponse_errorInfo,
    getDataViewResponse_lastModifiedTime,
    getDataViewResponse_partitionColumns,
    getDataViewResponse_sortColumns,
    getDataViewResponse_status,
    getDataViewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request for retrieving a data view detail. Grouped \/ accessible within
-- a dataset by its dataset id.
--
-- /See:/ 'newGetDataView' smart constructor.
data GetDataView = GetDataView'
  { -- | The unique identifier for the Dataview.
    dataViewId :: Prelude.Text,
    -- | The unique identifier for the Dataset used in the Dataview.
    datasetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataViewId', 'getDataView_dataViewId' - The unique identifier for the Dataview.
--
-- 'datasetId', 'getDataView_datasetId' - The unique identifier for the Dataset used in the Dataview.
newGetDataView ::
  -- | 'dataViewId'
  Prelude.Text ->
  -- | 'datasetId'
  Prelude.Text ->
  GetDataView
newGetDataView pDataViewId_ pDatasetId_ =
  GetDataView'
    { dataViewId = pDataViewId_,
      datasetId = pDatasetId_
    }

-- | The unique identifier for the Dataview.
getDataView_dataViewId :: Lens.Lens' GetDataView Prelude.Text
getDataView_dataViewId = Lens.lens (\GetDataView' {dataViewId} -> dataViewId) (\s@GetDataView' {} a -> s {dataViewId = a} :: GetDataView)

-- | The unique identifier for the Dataset used in the Dataview.
getDataView_datasetId :: Lens.Lens' GetDataView Prelude.Text
getDataView_datasetId = Lens.lens (\GetDataView' {datasetId} -> datasetId) (\s@GetDataView' {} a -> s {datasetId = a} :: GetDataView)

instance Core.AWSRequest GetDataView where
  type AWSResponse GetDataView = GetDataViewResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataViewResponse'
            Prelude.<$> (x Data..?> "asOfTimestamp")
            Prelude.<*> (x Data..?> "autoUpdate")
            Prelude.<*> (x Data..?> "createTime")
            Prelude.<*> (x Data..?> "dataViewArn")
            Prelude.<*> (x Data..?> "dataViewId")
            Prelude.<*> (x Data..?> "datasetId")
            Prelude.<*> (x Data..?> "destinationTypeParams")
            Prelude.<*> (x Data..?> "errorInfo")
            Prelude.<*> (x Data..?> "lastModifiedTime")
            Prelude.<*> ( x
                            Data..?> "partitionColumns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "sortColumns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataView where
  hashWithSalt _salt GetDataView' {..} =
    _salt
      `Prelude.hashWithSalt` dataViewId
      `Prelude.hashWithSalt` datasetId

instance Prelude.NFData GetDataView where
  rnf GetDataView' {..} =
    Prelude.rnf dataViewId
      `Prelude.seq` Prelude.rnf datasetId

instance Data.ToHeaders GetDataView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDataView where
  toPath GetDataView' {..} =
    Prelude.mconcat
      [ "/datasets/",
        Data.toBS datasetId,
        "/dataviewsv2/",
        Data.toBS dataViewId
      ]

instance Data.ToQuery GetDataView where
  toQuery = Prelude.const Prelude.mempty

-- | Response from retrieving a dataview, which includes details on the
-- target database and table name
--
-- /See:/ 'newGetDataViewResponse' smart constructor.
data GetDataViewResponse = GetDataViewResponse'
  { -- | Time range to use for the Dataview. The value is determined as epoch
    -- time in milliseconds. For example, the value for Monday, November 1,
    -- 2021 12:00:00 PM UTC is specified as 1635768000000.
    asOfTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | Flag to indicate Dataview should be updated automatically.
    autoUpdate :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp at which the Dataview was created in FinSpace. The value
    -- is determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createTime :: Prelude.Maybe Prelude.Integer,
    -- | The ARN identifier of the Dataview.
    dataViewArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Dataview.
    dataViewId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Dataset used in the Dataview.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | Options that define the destination type for the Dataview.
    destinationTypeParams :: Prelude.Maybe DataViewDestinationTypeParams,
    -- | Information about an error that occurred for the Dataview.
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
    status :: Prelude.Maybe DataViewStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'asOfTimestamp', 'getDataViewResponse_asOfTimestamp' - Time range to use for the Dataview. The value is determined as epoch
-- time in milliseconds. For example, the value for Monday, November 1,
-- 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'autoUpdate', 'getDataViewResponse_autoUpdate' - Flag to indicate Dataview should be updated automatically.
--
-- 'createTime', 'getDataViewResponse_createTime' - The timestamp at which the Dataview was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'dataViewArn', 'getDataViewResponse_dataViewArn' - The ARN identifier of the Dataview.
--
-- 'dataViewId', 'getDataViewResponse_dataViewId' - The unique identifier for the Dataview.
--
-- 'datasetId', 'getDataViewResponse_datasetId' - The unique identifier for the Dataset used in the Dataview.
--
-- 'destinationTypeParams', 'getDataViewResponse_destinationTypeParams' - Options that define the destination type for the Dataview.
--
-- 'errorInfo', 'getDataViewResponse_errorInfo' - Information about an error that occurred for the Dataview.
--
-- 'lastModifiedTime', 'getDataViewResponse_lastModifiedTime' - The last time that a Dataview was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'partitionColumns', 'getDataViewResponse_partitionColumns' - Ordered set of column names used to partition data.
--
-- 'sortColumns', 'getDataViewResponse_sortColumns' - Columns to be used for sorting the data.
--
-- 'status', 'getDataViewResponse_status' - The status of a Dataview creation.
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
--
-- 'httpStatus', 'getDataViewResponse_httpStatus' - The response's http status code.
newGetDataViewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataViewResponse
newGetDataViewResponse pHttpStatus_ =
  GetDataViewResponse'
    { asOfTimestamp =
        Prelude.Nothing,
      autoUpdate = Prelude.Nothing,
      createTime = Prelude.Nothing,
      dataViewArn = Prelude.Nothing,
      dataViewId = Prelude.Nothing,
      datasetId = Prelude.Nothing,
      destinationTypeParams = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      partitionColumns = Prelude.Nothing,
      sortColumns = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Time range to use for the Dataview. The value is determined as epoch
-- time in milliseconds. For example, the value for Monday, November 1,
-- 2021 12:00:00 PM UTC is specified as 1635768000000.
getDataViewResponse_asOfTimestamp :: Lens.Lens' GetDataViewResponse (Prelude.Maybe Prelude.Integer)
getDataViewResponse_asOfTimestamp = Lens.lens (\GetDataViewResponse' {asOfTimestamp} -> asOfTimestamp) (\s@GetDataViewResponse' {} a -> s {asOfTimestamp = a} :: GetDataViewResponse)

-- | Flag to indicate Dataview should be updated automatically.
getDataViewResponse_autoUpdate :: Lens.Lens' GetDataViewResponse (Prelude.Maybe Prelude.Bool)
getDataViewResponse_autoUpdate = Lens.lens (\GetDataViewResponse' {autoUpdate} -> autoUpdate) (\s@GetDataViewResponse' {} a -> s {autoUpdate = a} :: GetDataViewResponse)

-- | The timestamp at which the Dataview was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getDataViewResponse_createTime :: Lens.Lens' GetDataViewResponse (Prelude.Maybe Prelude.Integer)
getDataViewResponse_createTime = Lens.lens (\GetDataViewResponse' {createTime} -> createTime) (\s@GetDataViewResponse' {} a -> s {createTime = a} :: GetDataViewResponse)

-- | The ARN identifier of the Dataview.
getDataViewResponse_dataViewArn :: Lens.Lens' GetDataViewResponse (Prelude.Maybe Prelude.Text)
getDataViewResponse_dataViewArn = Lens.lens (\GetDataViewResponse' {dataViewArn} -> dataViewArn) (\s@GetDataViewResponse' {} a -> s {dataViewArn = a} :: GetDataViewResponse)

-- | The unique identifier for the Dataview.
getDataViewResponse_dataViewId :: Lens.Lens' GetDataViewResponse (Prelude.Maybe Prelude.Text)
getDataViewResponse_dataViewId = Lens.lens (\GetDataViewResponse' {dataViewId} -> dataViewId) (\s@GetDataViewResponse' {} a -> s {dataViewId = a} :: GetDataViewResponse)

-- | The unique identifier for the Dataset used in the Dataview.
getDataViewResponse_datasetId :: Lens.Lens' GetDataViewResponse (Prelude.Maybe Prelude.Text)
getDataViewResponse_datasetId = Lens.lens (\GetDataViewResponse' {datasetId} -> datasetId) (\s@GetDataViewResponse' {} a -> s {datasetId = a} :: GetDataViewResponse)

-- | Options that define the destination type for the Dataview.
getDataViewResponse_destinationTypeParams :: Lens.Lens' GetDataViewResponse (Prelude.Maybe DataViewDestinationTypeParams)
getDataViewResponse_destinationTypeParams = Lens.lens (\GetDataViewResponse' {destinationTypeParams} -> destinationTypeParams) (\s@GetDataViewResponse' {} a -> s {destinationTypeParams = a} :: GetDataViewResponse)

-- | Information about an error that occurred for the Dataview.
getDataViewResponse_errorInfo :: Lens.Lens' GetDataViewResponse (Prelude.Maybe DataViewErrorInfo)
getDataViewResponse_errorInfo = Lens.lens (\GetDataViewResponse' {errorInfo} -> errorInfo) (\s@GetDataViewResponse' {} a -> s {errorInfo = a} :: GetDataViewResponse)

-- | The last time that a Dataview was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
getDataViewResponse_lastModifiedTime :: Lens.Lens' GetDataViewResponse (Prelude.Maybe Prelude.Integer)
getDataViewResponse_lastModifiedTime = Lens.lens (\GetDataViewResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetDataViewResponse' {} a -> s {lastModifiedTime = a} :: GetDataViewResponse)

-- | Ordered set of column names used to partition data.
getDataViewResponse_partitionColumns :: Lens.Lens' GetDataViewResponse (Prelude.Maybe [Prelude.Text])
getDataViewResponse_partitionColumns = Lens.lens (\GetDataViewResponse' {partitionColumns} -> partitionColumns) (\s@GetDataViewResponse' {} a -> s {partitionColumns = a} :: GetDataViewResponse) Prelude.. Lens.mapping Lens.coerced

-- | Columns to be used for sorting the data.
getDataViewResponse_sortColumns :: Lens.Lens' GetDataViewResponse (Prelude.Maybe [Prelude.Text])
getDataViewResponse_sortColumns = Lens.lens (\GetDataViewResponse' {sortColumns} -> sortColumns) (\s@GetDataViewResponse' {} a -> s {sortColumns = a} :: GetDataViewResponse) Prelude.. Lens.mapping Lens.coerced

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
getDataViewResponse_status :: Lens.Lens' GetDataViewResponse (Prelude.Maybe DataViewStatus)
getDataViewResponse_status = Lens.lens (\GetDataViewResponse' {status} -> status) (\s@GetDataViewResponse' {} a -> s {status = a} :: GetDataViewResponse)

-- | The response's http status code.
getDataViewResponse_httpStatus :: Lens.Lens' GetDataViewResponse Prelude.Int
getDataViewResponse_httpStatus = Lens.lens (\GetDataViewResponse' {httpStatus} -> httpStatus) (\s@GetDataViewResponse' {} a -> s {httpStatus = a} :: GetDataViewResponse)

instance Prelude.NFData GetDataViewResponse where
  rnf GetDataViewResponse' {..} =
    Prelude.rnf asOfTimestamp
      `Prelude.seq` Prelude.rnf autoUpdate
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf dataViewArn
      `Prelude.seq` Prelude.rnf dataViewId
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf destinationTypeParams
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf partitionColumns
      `Prelude.seq` Prelude.rnf sortColumns
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
