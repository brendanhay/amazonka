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
-- Module      : Network.AWS.Glue.BatchDeleteConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of connection definitions from the Data Catalog.
module Network.AWS.Glue.BatchDeleteConnection
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
    batchDeleteConnectionResponse_succeeded,
    batchDeleteConnectionResponse_errors,
    batchDeleteConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDeleteConnection' smart constructor.
data BatchDeleteConnection = BatchDeleteConnection'
  { -- | The ID of the Data Catalog in which the connections reside. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | A list of names of the connections to delete.
    connectionNameList :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeleteConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchDeleteConnection_catalogId' - The ID of the Data Catalog in which the connections reside. If none is
-- provided, the AWS account ID is used by default.
--
-- 'connectionNameList', 'batchDeleteConnection_connectionNameList' - A list of names of the connections to delete.
newBatchDeleteConnection ::
  BatchDeleteConnection
newBatchDeleteConnection =
  BatchDeleteConnection'
    { catalogId = Core.Nothing,
      connectionNameList = Core.mempty
    }

-- | The ID of the Data Catalog in which the connections reside. If none is
-- provided, the AWS account ID is used by default.
batchDeleteConnection_catalogId :: Lens.Lens' BatchDeleteConnection (Core.Maybe Core.Text)
batchDeleteConnection_catalogId = Lens.lens (\BatchDeleteConnection' {catalogId} -> catalogId) (\s@BatchDeleteConnection' {} a -> s {catalogId = a} :: BatchDeleteConnection)

-- | A list of names of the connections to delete.
batchDeleteConnection_connectionNameList :: Lens.Lens' BatchDeleteConnection [Core.Text]
batchDeleteConnection_connectionNameList = Lens.lens (\BatchDeleteConnection' {connectionNameList} -> connectionNameList) (\s@BatchDeleteConnection' {} a -> s {connectionNameList = a} :: BatchDeleteConnection) Core.. Lens._Coerce

instance Core.AWSRequest BatchDeleteConnection where
  type
    AWSResponse BatchDeleteConnection =
      BatchDeleteConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteConnectionResponse'
            Core.<$> (x Core..?> "Succeeded" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Errors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchDeleteConnection

instance Core.NFData BatchDeleteConnection

instance Core.ToHeaders BatchDeleteConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchDeleteConnection" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchDeleteConnection where
  toJSON BatchDeleteConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just
              ("ConnectionNameList" Core..= connectionNameList)
          ]
      )

instance Core.ToPath BatchDeleteConnection where
  toPath = Core.const "/"

instance Core.ToQuery BatchDeleteConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchDeleteConnectionResponse' smart constructor.
data BatchDeleteConnectionResponse = BatchDeleteConnectionResponse'
  { -- | A list of names of the connection definitions that were successfully
    -- deleted.
    succeeded :: Core.Maybe [Core.Text],
    -- | A map of the names of connections that were not successfully deleted to
    -- error details.
    errors :: Core.Maybe (Core.HashMap Core.Text ErrorDetail),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeleteConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'succeeded', 'batchDeleteConnectionResponse_succeeded' - A list of names of the connection definitions that were successfully
-- deleted.
--
-- 'errors', 'batchDeleteConnectionResponse_errors' - A map of the names of connections that were not successfully deleted to
-- error details.
--
-- 'httpStatus', 'batchDeleteConnectionResponse_httpStatus' - The response's http status code.
newBatchDeleteConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchDeleteConnectionResponse
newBatchDeleteConnectionResponse pHttpStatus_ =
  BatchDeleteConnectionResponse'
    { succeeded =
        Core.Nothing,
      errors = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of names of the connection definitions that were successfully
-- deleted.
batchDeleteConnectionResponse_succeeded :: Lens.Lens' BatchDeleteConnectionResponse (Core.Maybe [Core.Text])
batchDeleteConnectionResponse_succeeded = Lens.lens (\BatchDeleteConnectionResponse' {succeeded} -> succeeded) (\s@BatchDeleteConnectionResponse' {} a -> s {succeeded = a} :: BatchDeleteConnectionResponse) Core.. Lens.mapping Lens._Coerce

-- | A map of the names of connections that were not successfully deleted to
-- error details.
batchDeleteConnectionResponse_errors :: Lens.Lens' BatchDeleteConnectionResponse (Core.Maybe (Core.HashMap Core.Text ErrorDetail))
batchDeleteConnectionResponse_errors = Lens.lens (\BatchDeleteConnectionResponse' {errors} -> errors) (\s@BatchDeleteConnectionResponse' {} a -> s {errors = a} :: BatchDeleteConnectionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchDeleteConnectionResponse_httpStatus :: Lens.Lens' BatchDeleteConnectionResponse Core.Int
batchDeleteConnectionResponse_httpStatus = Lens.lens (\BatchDeleteConnectionResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteConnectionResponse' {} a -> s {httpStatus = a} :: BatchDeleteConnectionResponse)

instance Core.NFData BatchDeleteConnectionResponse
