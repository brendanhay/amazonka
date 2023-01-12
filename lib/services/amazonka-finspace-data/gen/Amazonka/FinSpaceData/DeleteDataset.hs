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
-- Module      : Amazonka.FinSpaceData.DeleteDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a FinSpace Dataset.
module Amazonka.FinSpaceData.DeleteDataset
  ( -- * Creating a Request
    DeleteDataset (..),
    newDeleteDataset,

    -- * Request Lenses
    deleteDataset_clientToken,
    deleteDataset_datasetId,

    -- * Destructuring the Response
    DeleteDatasetResponse (..),
    newDeleteDatasetResponse,

    -- * Response Lenses
    deleteDatasetResponse_datasetId,
    deleteDatasetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request for a DeleteDataset operation.
--
-- /See:/ 'newDeleteDataset' smart constructor.
data DeleteDataset = DeleteDataset'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the Dataset to be deleted.
    datasetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteDataset_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'datasetId', 'deleteDataset_datasetId' - The unique identifier of the Dataset to be deleted.
newDeleteDataset ::
  -- | 'datasetId'
  Prelude.Text ->
  DeleteDataset
newDeleteDataset pDatasetId_ =
  DeleteDataset'
    { clientToken = Prelude.Nothing,
      datasetId = pDatasetId_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
deleteDataset_clientToken :: Lens.Lens' DeleteDataset (Prelude.Maybe Prelude.Text)
deleteDataset_clientToken = Lens.lens (\DeleteDataset' {clientToken} -> clientToken) (\s@DeleteDataset' {} a -> s {clientToken = a} :: DeleteDataset)

-- | The unique identifier of the Dataset to be deleted.
deleteDataset_datasetId :: Lens.Lens' DeleteDataset Prelude.Text
deleteDataset_datasetId = Lens.lens (\DeleteDataset' {datasetId} -> datasetId) (\s@DeleteDataset' {} a -> s {datasetId = a} :: DeleteDataset)

instance Core.AWSRequest DeleteDataset where
  type
    AWSResponse DeleteDataset =
      DeleteDatasetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDatasetResponse'
            Prelude.<$> (x Data..?> "datasetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataset where
  hashWithSalt _salt DeleteDataset' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` datasetId

instance Prelude.NFData DeleteDataset where
  rnf DeleteDataset' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf datasetId

instance Data.ToHeaders DeleteDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDataset where
  toPath DeleteDataset' {..} =
    Prelude.mconcat
      ["/datasetsv2/", Data.toBS datasetId]

instance Data.ToQuery DeleteDataset where
  toQuery DeleteDataset' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | The response from an DeleteDataset operation
--
-- /See:/ 'newDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  { -- | The unique identifier for the deleted Dataset.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetId', 'deleteDatasetResponse_datasetId' - The unique identifier for the deleted Dataset.
--
-- 'httpStatus', 'deleteDatasetResponse_httpStatus' - The response's http status code.
newDeleteDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDatasetResponse
newDeleteDatasetResponse pHttpStatus_ =
  DeleteDatasetResponse'
    { datasetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the deleted Dataset.
deleteDatasetResponse_datasetId :: Lens.Lens' DeleteDatasetResponse (Prelude.Maybe Prelude.Text)
deleteDatasetResponse_datasetId = Lens.lens (\DeleteDatasetResponse' {datasetId} -> datasetId) (\s@DeleteDatasetResponse' {} a -> s {datasetId = a} :: DeleteDatasetResponse)

-- | The response's http status code.
deleteDatasetResponse_httpStatus :: Lens.Lens' DeleteDatasetResponse Prelude.Int
deleteDatasetResponse_httpStatus = Lens.lens (\DeleteDatasetResponse' {httpStatus} -> httpStatus) (\s@DeleteDatasetResponse' {} a -> s {httpStatus = a} :: DeleteDatasetResponse)

instance Prelude.NFData DeleteDatasetResponse where
  rnf DeleteDatasetResponse' {..} =
    Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf httpStatus
