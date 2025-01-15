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
-- Module      : Amazonka.MachineLearning.DeleteDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.MachineLearning.DeleteDataSource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataSource' smart constructor.
data DeleteDataSource = DeleteDataSource'
  { -- | A user-supplied ID that uniquely identifies the @DataSource@.
    dataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteDataSource where
  type
    AWSResponse DeleteDataSource =
      DeleteDataSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDataSourceResponse'
            Prelude.<$> (x Data..?> "DataSourceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataSource where
  hashWithSalt _salt DeleteDataSource' {..} =
    _salt `Prelude.hashWithSalt` dataSourceId

instance Prelude.NFData DeleteDataSource where
  rnf DeleteDataSource' {..} = Prelude.rnf dataSourceId

instance Data.ToHeaders DeleteDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.DeleteDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDataSource where
  toJSON DeleteDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DataSourceId" Data..= dataSourceId)]
      )

instance Data.ToPath DeleteDataSource where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDataSource where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteDataSourceResponse where
  rnf DeleteDataSourceResponse' {..} =
    Prelude.rnf dataSourceId `Prelude.seq`
      Prelude.rnf httpStatus
