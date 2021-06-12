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
-- Module      : Network.AWS.MachineLearning.UpdateDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @DataSourceName@ of a @DataSource@.
--
-- You can use the @GetDataSource@ operation to view the contents of the
-- updated data element.
module Network.AWS.MachineLearning.UpdateDataSource
  ( -- * Creating a Request
    UpdateDataSource (..),
    newUpdateDataSource,

    -- * Request Lenses
    updateDataSource_dataSourceId,
    updateDataSource_dataSourceName,

    -- * Destructuring the Response
    UpdateDataSourceResponse (..),
    newUpdateDataSourceResponse,

    -- * Response Lenses
    updateDataSourceResponse_dataSourceId,
    updateDataSourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDataSource' smart constructor.
data UpdateDataSource = UpdateDataSource'
  { -- | The ID assigned to the @DataSource@ during creation.
    dataSourceId :: Core.Text,
    -- | A new user-supplied name or description of the @DataSource@ that will
    -- replace the current description.
    dataSourceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'updateDataSource_dataSourceId' - The ID assigned to the @DataSource@ during creation.
--
-- 'dataSourceName', 'updateDataSource_dataSourceName' - A new user-supplied name or description of the @DataSource@ that will
-- replace the current description.
newUpdateDataSource ::
  -- | 'dataSourceId'
  Core.Text ->
  -- | 'dataSourceName'
  Core.Text ->
  UpdateDataSource
newUpdateDataSource pDataSourceId_ pDataSourceName_ =
  UpdateDataSource'
    { dataSourceId = pDataSourceId_,
      dataSourceName = pDataSourceName_
    }

-- | The ID assigned to the @DataSource@ during creation.
updateDataSource_dataSourceId :: Lens.Lens' UpdateDataSource Core.Text
updateDataSource_dataSourceId = Lens.lens (\UpdateDataSource' {dataSourceId} -> dataSourceId) (\s@UpdateDataSource' {} a -> s {dataSourceId = a} :: UpdateDataSource)

-- | A new user-supplied name or description of the @DataSource@ that will
-- replace the current description.
updateDataSource_dataSourceName :: Lens.Lens' UpdateDataSource Core.Text
updateDataSource_dataSourceName = Lens.lens (\UpdateDataSource' {dataSourceName} -> dataSourceName) (\s@UpdateDataSource' {} a -> s {dataSourceName = a} :: UpdateDataSource)

instance Core.AWSRequest UpdateDataSource where
  type
    AWSResponse UpdateDataSource =
      UpdateDataSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDataSourceResponse'
            Core.<$> (x Core..?> "DataSourceId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDataSource

instance Core.NFData UpdateDataSource

instance Core.ToHeaders UpdateDataSource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.UpdateDataSource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDataSource where
  toJSON UpdateDataSource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DataSourceId" Core..= dataSourceId),
            Core.Just ("DataSourceName" Core..= dataSourceName)
          ]
      )

instance Core.ToPath UpdateDataSource where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDataSource where
  toQuery = Core.const Core.mempty

-- | Represents the output of an @UpdateDataSource@ operation.
--
-- You can see the updated content by using the @GetBatchPrediction@
-- operation.
--
-- /See:/ 'newUpdateDataSourceResponse' smart constructor.
data UpdateDataSourceResponse = UpdateDataSourceResponse'
  { -- | The ID assigned to the @DataSource@ during creation. This value should
    -- be identical to the value of the @DataSourceID@ in the request.
    dataSourceId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'updateDataSourceResponse_dataSourceId' - The ID assigned to the @DataSource@ during creation. This value should
-- be identical to the value of the @DataSourceID@ in the request.
--
-- 'httpStatus', 'updateDataSourceResponse_httpStatus' - The response's http status code.
newUpdateDataSourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDataSourceResponse
newUpdateDataSourceResponse pHttpStatus_ =
  UpdateDataSourceResponse'
    { dataSourceId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID assigned to the @DataSource@ during creation. This value should
-- be identical to the value of the @DataSourceID@ in the request.
updateDataSourceResponse_dataSourceId :: Lens.Lens' UpdateDataSourceResponse (Core.Maybe Core.Text)
updateDataSourceResponse_dataSourceId = Lens.lens (\UpdateDataSourceResponse' {dataSourceId} -> dataSourceId) (\s@UpdateDataSourceResponse' {} a -> s {dataSourceId = a} :: UpdateDataSourceResponse)

-- | The response's http status code.
updateDataSourceResponse_httpStatus :: Lens.Lens' UpdateDataSourceResponse Core.Int
updateDataSourceResponse_httpStatus = Lens.lens (\UpdateDataSourceResponse' {httpStatus} -> httpStatus) (\s@UpdateDataSourceResponse' {} a -> s {httpStatus = a} :: UpdateDataSourceResponse)

instance Core.NFData UpdateDataSourceResponse
