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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchWrite' smart constructor.
data BatchWrite = BatchWrite'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory.
    -- For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | A list of operations that are part of the batch.
    operations :: [BatchWriteOperation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  BatchWrite
newBatchWrite pDirectoryArn_ =
  BatchWrite'
    { directoryArn = pDirectoryArn_,
      operations = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) that is associated with the Directory.
-- For more information, see arns.
batchWrite_directoryArn :: Lens.Lens' BatchWrite Prelude.Text
batchWrite_directoryArn = Lens.lens (\BatchWrite' {directoryArn} -> directoryArn) (\s@BatchWrite' {} a -> s {directoryArn = a} :: BatchWrite)

-- | A list of operations that are part of the batch.
batchWrite_operations :: Lens.Lens' BatchWrite [BatchWriteOperation]
batchWrite_operations = Lens.lens (\BatchWrite' {operations} -> operations) (\s@BatchWrite' {} a -> s {operations = a} :: BatchWrite) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchWrite where
  type AWSResponse BatchWrite = BatchWriteResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchWriteResponse'
            Prelude.<$> (x Core..?> "Responses" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchWrite

instance Prelude.NFData BatchWrite

instance Core.ToHeaders BatchWrite where
  toHeaders BatchWrite' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON BatchWrite where
  toJSON BatchWrite' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Operations" Core..= operations)]
      )

instance Core.ToPath BatchWrite where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/batchwrite"

instance Core.ToQuery BatchWrite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchWriteResponse' smart constructor.
data BatchWriteResponse = BatchWriteResponse'
  { -- | A list of all the responses for each batch write.
    responses :: Prelude.Maybe [BatchWriteOperationResponse],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchWriteResponse
newBatchWriteResponse pHttpStatus_ =
  BatchWriteResponse'
    { responses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of all the responses for each batch write.
batchWriteResponse_responses :: Lens.Lens' BatchWriteResponse (Prelude.Maybe [BatchWriteOperationResponse])
batchWriteResponse_responses = Lens.lens (\BatchWriteResponse' {responses} -> responses) (\s@BatchWriteResponse' {} a -> s {responses = a} :: BatchWriteResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchWriteResponse_httpStatus :: Lens.Lens' BatchWriteResponse Prelude.Int
batchWriteResponse_httpStatus = Lens.lens (\BatchWriteResponse' {httpStatus} -> httpStatus) (\s@BatchWriteResponse' {} a -> s {httpStatus = a} :: BatchWriteResponse)

instance Prelude.NFData BatchWriteResponse
