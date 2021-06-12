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
-- Module      : Network.AWS.CloudDirectory.BatchWrite
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs all the write operations in a batch. Either all the operations
-- succeed or none.
module Network.AWS.CloudDirectory.BatchWrite
  ( -- * Creating a Request
    BatchWrite (..),
    newBatchWrite,

    -- * Request Lenses
    batchWrite_directoryArn,
    batchWrite_operations,

    -- * Destructuring the Response
    BatchWriteResponse (..),
    newBatchWriteResponse,

    -- * Response Lenses
    batchWriteResponse_responses,
    batchWriteResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchWrite' smart constructor.
data BatchWrite = BatchWrite'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory.
    -- For more information, see arns.
    directoryArn :: Core.Text,
    -- | A list of operations that are part of the batch.
    operations :: [BatchWriteOperation]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchWrite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'batchWrite_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory.
-- For more information, see arns.
--
-- 'operations', 'batchWrite_operations' - A list of operations that are part of the batch.
newBatchWrite ::
  -- | 'directoryArn'
  Core.Text ->
  BatchWrite
newBatchWrite pDirectoryArn_ =
  BatchWrite'
    { directoryArn = pDirectoryArn_,
      operations = Core.mempty
    }

-- | The Amazon Resource Name (ARN) that is associated with the Directory.
-- For more information, see arns.
batchWrite_directoryArn :: Lens.Lens' BatchWrite Core.Text
batchWrite_directoryArn = Lens.lens (\BatchWrite' {directoryArn} -> directoryArn) (\s@BatchWrite' {} a -> s {directoryArn = a} :: BatchWrite)

-- | A list of operations that are part of the batch.
batchWrite_operations :: Lens.Lens' BatchWrite [BatchWriteOperation]
batchWrite_operations = Lens.lens (\BatchWrite' {operations} -> operations) (\s@BatchWrite' {} a -> s {operations = a} :: BatchWrite) Core.. Lens._Coerce

instance Core.AWSRequest BatchWrite where
  type AWSResponse BatchWrite = BatchWriteResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchWriteResponse'
            Core.<$> (x Core..?> "Responses" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchWrite

instance Core.NFData BatchWrite

instance Core.ToHeaders BatchWrite where
  toHeaders BatchWrite' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON BatchWrite where
  toJSON BatchWrite' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Operations" Core..= operations)]
      )

instance Core.ToPath BatchWrite where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/batchwrite"

instance Core.ToQuery BatchWrite where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchWriteResponse' smart constructor.
data BatchWriteResponse = BatchWriteResponse'
  { -- | A list of all the responses for each batch write.
    responses :: Core.Maybe [BatchWriteOperationResponse],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchWriteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responses', 'batchWriteResponse_responses' - A list of all the responses for each batch write.
--
-- 'httpStatus', 'batchWriteResponse_httpStatus' - The response's http status code.
newBatchWriteResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchWriteResponse
newBatchWriteResponse pHttpStatus_ =
  BatchWriteResponse'
    { responses = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of all the responses for each batch write.
batchWriteResponse_responses :: Lens.Lens' BatchWriteResponse (Core.Maybe [BatchWriteOperationResponse])
batchWriteResponse_responses = Lens.lens (\BatchWriteResponse' {responses} -> responses) (\s@BatchWriteResponse' {} a -> s {responses = a} :: BatchWriteResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchWriteResponse_httpStatus :: Lens.Lens' BatchWriteResponse Core.Int
batchWriteResponse_httpStatus = Lens.lens (\BatchWriteResponse' {httpStatus} -> httpStatus) (\s@BatchWriteResponse' {} a -> s {httpStatus = a} :: BatchWriteResponse)

instance Core.NFData BatchWriteResponse
