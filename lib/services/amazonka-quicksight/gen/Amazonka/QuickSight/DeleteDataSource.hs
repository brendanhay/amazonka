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
-- Module      : Amazonka.QuickSight.DeleteDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the data source permanently. This operation breaks all the
-- datasets that reference the deleted data source.
module Amazonka.QuickSight.DeleteDataSource
  ( -- * Creating a Request
    DeleteDataSource (..),
    newDeleteDataSource,

    -- * Request Lenses
    deleteDataSource_awsAccountId,
    deleteDataSource_dataSourceId,

    -- * Destructuring the Response
    DeleteDataSourceResponse (..),
    newDeleteDataSourceResponse,

    -- * Response Lenses
    deleteDataSourceResponse_arn,
    deleteDataSourceResponse_dataSourceId,
    deleteDataSourceResponse_requestId,
    deleteDataSourceResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataSource' smart constructor.
data DeleteDataSource = DeleteDataSource'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
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
-- 'awsAccountId', 'deleteDataSource_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSourceId', 'deleteDataSource_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
newDeleteDataSource ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSourceId'
  Prelude.Text ->
  DeleteDataSource
newDeleteDataSource pAwsAccountId_ pDataSourceId_ =
  DeleteDataSource'
    { awsAccountId = pAwsAccountId_,
      dataSourceId = pDataSourceId_
    }

-- | The Amazon Web Services account ID.
deleteDataSource_awsAccountId :: Lens.Lens' DeleteDataSource Prelude.Text
deleteDataSource_awsAccountId = Lens.lens (\DeleteDataSource' {awsAccountId} -> awsAccountId) (\s@DeleteDataSource' {} a -> s {awsAccountId = a} :: DeleteDataSource)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
deleteDataSource_dataSourceId :: Lens.Lens' DeleteDataSource Prelude.Text
deleteDataSource_dataSourceId = Lens.lens (\DeleteDataSource' {dataSourceId} -> dataSourceId) (\s@DeleteDataSource' {} a -> s {dataSourceId = a} :: DeleteDataSource)

instance Core.AWSRequest DeleteDataSource where
  type
    AWSResponse DeleteDataSource =
      DeleteDataSourceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDataSourceResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "DataSourceId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataSource where
  hashWithSalt _salt DeleteDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSourceId

instance Prelude.NFData DeleteDataSource where
  rnf DeleteDataSource' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSourceId

instance Data.ToHeaders DeleteDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDataSource where
  toPath DeleteDataSource' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sources/",
        Data.toBS dataSourceId
      ]

instance Data.ToQuery DeleteDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataSourceResponse' smart constructor.
data DeleteDataSourceResponse = DeleteDataSourceResponse'
  { -- | The Amazon Resource Name (ARN) of the data source that you deleted.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
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
-- 'arn', 'deleteDataSourceResponse_arn' - The Amazon Resource Name (ARN) of the data source that you deleted.
--
-- 'dataSourceId', 'deleteDataSourceResponse_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
--
-- 'requestId', 'deleteDataSourceResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'deleteDataSourceResponse_status' - The HTTP status of the request.
newDeleteDataSourceResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteDataSourceResponse
newDeleteDataSourceResponse pStatus_ =
  DeleteDataSourceResponse'
    { arn = Prelude.Nothing,
      dataSourceId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the data source that you deleted.
deleteDataSourceResponse_arn :: Lens.Lens' DeleteDataSourceResponse (Prelude.Maybe Prelude.Text)
deleteDataSourceResponse_arn = Lens.lens (\DeleteDataSourceResponse' {arn} -> arn) (\s@DeleteDataSourceResponse' {} a -> s {arn = a} :: DeleteDataSourceResponse)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
deleteDataSourceResponse_dataSourceId :: Lens.Lens' DeleteDataSourceResponse (Prelude.Maybe Prelude.Text)
deleteDataSourceResponse_dataSourceId = Lens.lens (\DeleteDataSourceResponse' {dataSourceId} -> dataSourceId) (\s@DeleteDataSourceResponse' {} a -> s {dataSourceId = a} :: DeleteDataSourceResponse)

-- | The Amazon Web Services request ID for this operation.
deleteDataSourceResponse_requestId :: Lens.Lens' DeleteDataSourceResponse (Prelude.Maybe Prelude.Text)
deleteDataSourceResponse_requestId = Lens.lens (\DeleteDataSourceResponse' {requestId} -> requestId) (\s@DeleteDataSourceResponse' {} a -> s {requestId = a} :: DeleteDataSourceResponse)

-- | The HTTP status of the request.
deleteDataSourceResponse_status :: Lens.Lens' DeleteDataSourceResponse Prelude.Int
deleteDataSourceResponse_status = Lens.lens (\DeleteDataSourceResponse' {status} -> status) (\s@DeleteDataSourceResponse' {} a -> s {status = a} :: DeleteDataSourceResponse)

instance Prelude.NFData DeleteDataSourceResponse where
  rnf DeleteDataSourceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
