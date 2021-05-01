{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MachineLearning.DeleteDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to a @DataSource@, rendering it unusable.
--
-- After using the @DeleteDataSource@ operation, you can use the
-- GetDataSource operation to verify that the status of the @DataSource@
-- changed to DELETED.
--
-- __Caution:__ The results of the @DeleteDataSource@ operation are
-- irreversible.
module Network.AWS.MachineLearning.DeleteDataSource
  ( -- * Creating a Request
    DeleteDataSource (..),
    newDeleteDataSource,

    -- * Request Lenses
    deleteDataSource_dataSourceId,

    -- * Destructuring the Response
    DeleteDataSourceResponse (..),
    newDeleteDataSourceResponse,

    -- * Response Lenses
    deleteDataSourceResponse_dataSourceId,
    deleteDataSourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDataSource' smart constructor.
data DeleteDataSource = DeleteDataSource'
  { -- | A user-supplied ID that uniquely identifies the @DataSource@.
    dataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'deleteDataSource_dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@.
newDeleteDataSource ::
  -- | 'dataSourceId'
  Prelude.Text ->
  DeleteDataSource
newDeleteDataSource pDataSourceId_ =
  DeleteDataSource' {dataSourceId = pDataSourceId_}

-- | A user-supplied ID that uniquely identifies the @DataSource@.
deleteDataSource_dataSourceId :: Lens.Lens' DeleteDataSource Prelude.Text
deleteDataSource_dataSourceId = Lens.lens (\DeleteDataSource' {dataSourceId} -> dataSourceId) (\s@DeleteDataSource' {} a -> s {dataSourceId = a} :: DeleteDataSource)

instance Prelude.AWSRequest DeleteDataSource where
  type Rs DeleteDataSource = DeleteDataSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDataSourceResponse'
            Prelude.<$> (x Prelude..?> "DataSourceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataSource

instance Prelude.NFData DeleteDataSource

instance Prelude.ToHeaders DeleteDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.DeleteDataSource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDataSource where
  toJSON DeleteDataSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DataSourceId" Prelude..= dataSourceId)
          ]
      )

instance Prelude.ToPath DeleteDataSource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @DeleteDataSource@ operation.
--
-- /See:/ 'newDeleteDataSourceResponse' smart constructor.
data DeleteDataSourceResponse = DeleteDataSourceResponse'
  { -- | A user-supplied ID that uniquely identifies the @DataSource@. This value
    -- should be identical to the value of the @DataSourceID@ in the request.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'deleteDataSourceResponse_dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@. This value
-- should be identical to the value of the @DataSourceID@ in the request.
--
-- 'httpStatus', 'deleteDataSourceResponse_httpStatus' - The response's http status code.
newDeleteDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDataSourceResponse
newDeleteDataSourceResponse pHttpStatus_ =
  DeleteDataSourceResponse'
    { dataSourceId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @DataSource@. This value
-- should be identical to the value of the @DataSourceID@ in the request.
deleteDataSourceResponse_dataSourceId :: Lens.Lens' DeleteDataSourceResponse (Prelude.Maybe Prelude.Text)
deleteDataSourceResponse_dataSourceId = Lens.lens (\DeleteDataSourceResponse' {dataSourceId} -> dataSourceId) (\s@DeleteDataSourceResponse' {} a -> s {dataSourceId = a} :: DeleteDataSourceResponse)

-- | The response's http status code.
deleteDataSourceResponse_httpStatus :: Lens.Lens' DeleteDataSourceResponse Prelude.Int
deleteDataSourceResponse_httpStatus = Lens.lens (\DeleteDataSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteDataSourceResponse' {} a -> s {httpStatus = a} :: DeleteDataSourceResponse)

instance Prelude.NFData DeleteDataSourceResponse
