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
-- Module      : Amazonka.Glue.BatchDeleteConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of connection definitions from the Data Catalog.
module Amazonka.Glue.BatchDeleteConnection
  ( -- * Creating a Request
    BatchDeleteConnection (..),
    newBatchDeleteConnection,

    -- * Request Lenses
    batchDeleteConnection_catalogId,
    batchDeleteConnection_connectionNameList,

    -- * Destructuring the Response
    BatchDeleteConnectionResponse (..),
    newBatchDeleteConnectionResponse,

    -- * Response Lenses
    batchDeleteConnectionResponse_errors,
    batchDeleteConnectionResponse_succeeded,
    batchDeleteConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteConnection' smart constructor.
data BatchDeleteConnection = BatchDeleteConnection'
  { -- | The ID of the Data Catalog in which the connections reside. If none is
    -- provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A list of names of the connections to delete.
    connectionNameList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchDeleteConnection_catalogId' - The ID of the Data Catalog in which the connections reside. If none is
-- provided, the Amazon Web Services account ID is used by default.
--
-- 'connectionNameList', 'batchDeleteConnection_connectionNameList' - A list of names of the connections to delete.
newBatchDeleteConnection ::
  BatchDeleteConnection
newBatchDeleteConnection =
  BatchDeleteConnection'
    { catalogId = Prelude.Nothing,
      connectionNameList = Prelude.mempty
    }

-- | The ID of the Data Catalog in which the connections reside. If none is
-- provided, the Amazon Web Services account ID is used by default.
batchDeleteConnection_catalogId :: Lens.Lens' BatchDeleteConnection (Prelude.Maybe Prelude.Text)
batchDeleteConnection_catalogId = Lens.lens (\BatchDeleteConnection' {catalogId} -> catalogId) (\s@BatchDeleteConnection' {} a -> s {catalogId = a} :: BatchDeleteConnection)

-- | A list of names of the connections to delete.
batchDeleteConnection_connectionNameList :: Lens.Lens' BatchDeleteConnection [Prelude.Text]
batchDeleteConnection_connectionNameList = Lens.lens (\BatchDeleteConnection' {connectionNameList} -> connectionNameList) (\s@BatchDeleteConnection' {} a -> s {connectionNameList = a} :: BatchDeleteConnection) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteConnection where
  type
    AWSResponse BatchDeleteConnection =
      BatchDeleteConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteConnectionResponse'
            Prelude.<$> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Succeeded" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteConnection where
  hashWithSalt _salt BatchDeleteConnection' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` connectionNameList

instance Prelude.NFData BatchDeleteConnection where
  rnf BatchDeleteConnection' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf connectionNameList

instance Data.ToHeaders BatchDeleteConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.BatchDeleteConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteConnection where
  toJSON BatchDeleteConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just
              ("ConnectionNameList" Data..= connectionNameList)
          ]
      )

instance Data.ToPath BatchDeleteConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDeleteConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteConnectionResponse' smart constructor.
data BatchDeleteConnectionResponse = BatchDeleteConnectionResponse'
  { -- | A map of the names of connections that were not successfully deleted to
    -- error details.
    errors :: Prelude.Maybe (Prelude.HashMap Prelude.Text ErrorDetail),
    -- | A list of names of the connection definitions that were successfully
    -- deleted.
    succeeded :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchDeleteConnectionResponse_errors' - A map of the names of connections that were not successfully deleted to
-- error details.
--
-- 'succeeded', 'batchDeleteConnectionResponse_succeeded' - A list of names of the connection definitions that were successfully
-- deleted.
--
-- 'httpStatus', 'batchDeleteConnectionResponse_httpStatus' - The response's http status code.
newBatchDeleteConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteConnectionResponse
newBatchDeleteConnectionResponse pHttpStatus_ =
  BatchDeleteConnectionResponse'
    { errors =
        Prelude.Nothing,
      succeeded = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of the names of connections that were not successfully deleted to
-- error details.
batchDeleteConnectionResponse_errors :: Lens.Lens' BatchDeleteConnectionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ErrorDetail))
batchDeleteConnectionResponse_errors = Lens.lens (\BatchDeleteConnectionResponse' {errors} -> errors) (\s@BatchDeleteConnectionResponse' {} a -> s {errors = a} :: BatchDeleteConnectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of names of the connection definitions that were successfully
-- deleted.
batchDeleteConnectionResponse_succeeded :: Lens.Lens' BatchDeleteConnectionResponse (Prelude.Maybe [Prelude.Text])
batchDeleteConnectionResponse_succeeded = Lens.lens (\BatchDeleteConnectionResponse' {succeeded} -> succeeded) (\s@BatchDeleteConnectionResponse' {} a -> s {succeeded = a} :: BatchDeleteConnectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteConnectionResponse_httpStatus :: Lens.Lens' BatchDeleteConnectionResponse Prelude.Int
batchDeleteConnectionResponse_httpStatus = Lens.lens (\BatchDeleteConnectionResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteConnectionResponse' {} a -> s {httpStatus = a} :: BatchDeleteConnectionResponse)

instance Prelude.NFData BatchDeleteConnectionResponse where
  rnf BatchDeleteConnectionResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf succeeded
      `Prelude.seq` Prelude.rnf httpStatus
