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
-- Module      : Amazonka.FinSpaceData.CreateDataView
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Dataview for a Dataset.
module Amazonka.FinSpaceData.CreateDataView
  ( -- * Creating a Request
    CreateDataView (..),
    newCreateDataView,

    -- * Request Lenses
    createDataView_sortColumns,
    createDataView_clientToken,
    createDataView_autoUpdate,
    createDataView_partitionColumns,
    createDataView_asOfTimestamp,
    createDataView_datasetId,
    createDataView_destinationTypeParams,

    -- * Destructuring the Response
    CreateDataViewResponse (..),
    newCreateDataViewResponse,

    -- * Response Lenses
    createDataViewResponse_dataViewId,
    createDataViewResponse_datasetId,
    createDataViewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request for creating a data view.
--
-- /See:/ 'newCreateDataView' smart constructor.
data CreateDataView = CreateDataView'
  { -- | Columns to be used for sorting the data.
    sortColumns :: Prelude.Maybe [Prelude.Text],
    -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Flag to indicate Dataview should be updated automatically.
    autoUpdate :: Prelude.Maybe Prelude.Bool,
    -- | Ordered set of column names used to partition data.
    partitionColumns :: Prelude.Maybe [Prelude.Text],
    -- | Beginning time to use for the Dataview. The value is determined as epoch
    -- time in milliseconds. For example, the value for Monday, November 1,
    -- 2021 12:00:00 PM UTC is specified as 1635768000000.
    asOfTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | The unique Dataset identifier that is used to create a Dataview.
    datasetId :: Prelude.Text,
    -- | Options that define the destination type for the Dataview.
    destinationTypeParams :: DataViewDestinationTypeParams
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortColumns', 'createDataView_sortColumns' - Columns to be used for sorting the data.
--
-- 'clientToken', 'createDataView_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'autoUpdate', 'createDataView_autoUpdate' - Flag to indicate Dataview should be updated automatically.
--
-- 'partitionColumns', 'createDataView_partitionColumns' - Ordered set of column names used to partition data.
--
-- 'asOfTimestamp', 'createDataView_asOfTimestamp' - Beginning time to use for the Dataview. The value is determined as epoch
-- time in milliseconds. For example, the value for Monday, November 1,
-- 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'datasetId', 'createDataView_datasetId' - The unique Dataset identifier that is used to create a Dataview.
--
-- 'destinationTypeParams', 'createDataView_destinationTypeParams' - Options that define the destination type for the Dataview.
newCreateDataView ::
  -- | 'datasetId'
  Prelude.Text ->
  -- | 'destinationTypeParams'
  DataViewDestinationTypeParams ->
  CreateDataView
newCreateDataView pDatasetId_ pDestinationTypeParams_ =
  CreateDataView'
    { sortColumns = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      autoUpdate = Prelude.Nothing,
      partitionColumns = Prelude.Nothing,
      asOfTimestamp = Prelude.Nothing,
      datasetId = pDatasetId_,
      destinationTypeParams = pDestinationTypeParams_
    }

-- | Columns to be used for sorting the data.
createDataView_sortColumns :: Lens.Lens' CreateDataView (Prelude.Maybe [Prelude.Text])
createDataView_sortColumns = Lens.lens (\CreateDataView' {sortColumns} -> sortColumns) (\s@CreateDataView' {} a -> s {sortColumns = a} :: CreateDataView) Prelude.. Lens.mapping Lens.coerced

-- | A token that ensures idempotency. This token expires in 10 minutes.
createDataView_clientToken :: Lens.Lens' CreateDataView (Prelude.Maybe Prelude.Text)
createDataView_clientToken = Lens.lens (\CreateDataView' {clientToken} -> clientToken) (\s@CreateDataView' {} a -> s {clientToken = a} :: CreateDataView)

-- | Flag to indicate Dataview should be updated automatically.
createDataView_autoUpdate :: Lens.Lens' CreateDataView (Prelude.Maybe Prelude.Bool)
createDataView_autoUpdate = Lens.lens (\CreateDataView' {autoUpdate} -> autoUpdate) (\s@CreateDataView' {} a -> s {autoUpdate = a} :: CreateDataView)

-- | Ordered set of column names used to partition data.
createDataView_partitionColumns :: Lens.Lens' CreateDataView (Prelude.Maybe [Prelude.Text])
createDataView_partitionColumns = Lens.lens (\CreateDataView' {partitionColumns} -> partitionColumns) (\s@CreateDataView' {} a -> s {partitionColumns = a} :: CreateDataView) Prelude.. Lens.mapping Lens.coerced

-- | Beginning time to use for the Dataview. The value is determined as epoch
-- time in milliseconds. For example, the value for Monday, November 1,
-- 2021 12:00:00 PM UTC is specified as 1635768000000.
createDataView_asOfTimestamp :: Lens.Lens' CreateDataView (Prelude.Maybe Prelude.Integer)
createDataView_asOfTimestamp = Lens.lens (\CreateDataView' {asOfTimestamp} -> asOfTimestamp) (\s@CreateDataView' {} a -> s {asOfTimestamp = a} :: CreateDataView)

-- | The unique Dataset identifier that is used to create a Dataview.
createDataView_datasetId :: Lens.Lens' CreateDataView Prelude.Text
createDataView_datasetId = Lens.lens (\CreateDataView' {datasetId} -> datasetId) (\s@CreateDataView' {} a -> s {datasetId = a} :: CreateDataView)

-- | Options that define the destination type for the Dataview.
createDataView_destinationTypeParams :: Lens.Lens' CreateDataView DataViewDestinationTypeParams
createDataView_destinationTypeParams = Lens.lens (\CreateDataView' {destinationTypeParams} -> destinationTypeParams) (\s@CreateDataView' {} a -> s {destinationTypeParams = a} :: CreateDataView)

instance Core.AWSRequest CreateDataView where
  type
    AWSResponse CreateDataView =
      CreateDataViewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataViewResponse'
            Prelude.<$> (x Data..?> "dataViewId")
            Prelude.<*> (x Data..?> "datasetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataView where
  hashWithSalt _salt CreateDataView' {..} =
    _salt `Prelude.hashWithSalt` sortColumns
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` autoUpdate
      `Prelude.hashWithSalt` partitionColumns
      `Prelude.hashWithSalt` asOfTimestamp
      `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` destinationTypeParams

instance Prelude.NFData CreateDataView where
  rnf CreateDataView' {..} =
    Prelude.rnf sortColumns
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf autoUpdate
      `Prelude.seq` Prelude.rnf partitionColumns
      `Prelude.seq` Prelude.rnf asOfTimestamp
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf destinationTypeParams

instance Data.ToHeaders CreateDataView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataView where
  toJSON CreateDataView' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sortColumns" Data..=) Prelude.<$> sortColumns,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("autoUpdate" Data..=) Prelude.<$> autoUpdate,
            ("partitionColumns" Data..=)
              Prelude.<$> partitionColumns,
            ("asOfTimestamp" Data..=) Prelude.<$> asOfTimestamp,
            Prelude.Just
              ( "destinationTypeParams"
                  Data..= destinationTypeParams
              )
          ]
      )

instance Data.ToPath CreateDataView where
  toPath CreateDataView' {..} =
    Prelude.mconcat
      ["/datasets/", Data.toBS datasetId, "/dataviewsv2"]

instance Data.ToQuery CreateDataView where
  toQuery = Prelude.const Prelude.mempty

-- | Response for creating a data view.
--
-- /See:/ 'newCreateDataViewResponse' smart constructor.
data CreateDataViewResponse = CreateDataViewResponse'
  { -- | The unique identifier for the created Dataview.
    dataViewId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the Dataset used for the Dataview.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataViewId', 'createDataViewResponse_dataViewId' - The unique identifier for the created Dataview.
--
-- 'datasetId', 'createDataViewResponse_datasetId' - The unique identifier of the Dataset used for the Dataview.
--
-- 'httpStatus', 'createDataViewResponse_httpStatus' - The response's http status code.
newCreateDataViewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataViewResponse
newCreateDataViewResponse pHttpStatus_ =
  CreateDataViewResponse'
    { dataViewId =
        Prelude.Nothing,
      datasetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the created Dataview.
createDataViewResponse_dataViewId :: Lens.Lens' CreateDataViewResponse (Prelude.Maybe Prelude.Text)
createDataViewResponse_dataViewId = Lens.lens (\CreateDataViewResponse' {dataViewId} -> dataViewId) (\s@CreateDataViewResponse' {} a -> s {dataViewId = a} :: CreateDataViewResponse)

-- | The unique identifier of the Dataset used for the Dataview.
createDataViewResponse_datasetId :: Lens.Lens' CreateDataViewResponse (Prelude.Maybe Prelude.Text)
createDataViewResponse_datasetId = Lens.lens (\CreateDataViewResponse' {datasetId} -> datasetId) (\s@CreateDataViewResponse' {} a -> s {datasetId = a} :: CreateDataViewResponse)

-- | The response's http status code.
createDataViewResponse_httpStatus :: Lens.Lens' CreateDataViewResponse Prelude.Int
createDataViewResponse_httpStatus = Lens.lens (\CreateDataViewResponse' {httpStatus} -> httpStatus) (\s@CreateDataViewResponse' {} a -> s {httpStatus = a} :: CreateDataViewResponse)

instance Prelude.NFData CreateDataViewResponse where
  rnf CreateDataViewResponse' {..} =
    Prelude.rnf dataViewId
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf httpStatus
