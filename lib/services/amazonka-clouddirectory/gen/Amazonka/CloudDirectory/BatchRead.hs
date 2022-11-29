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
-- Module      : Amazonka.CloudDirectory.BatchRead
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs all the read operations in a batch.
module Amazonka.CloudDirectory.BatchRead
  ( -- * Creating a Request
    BatchRead (..),
    newBatchRead,

    -- * Request Lenses
    batchRead_consistencyLevel,
    batchRead_directoryArn,
    batchRead_operations,

    -- * Destructuring the Response
    BatchReadResponse (..),
    newBatchReadResponse,

    -- * Response Lenses
    batchReadResponse_responses,
    batchReadResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchRead' smart constructor.
data BatchRead = BatchRead'
  { -- | Represents the manner and timing in which the successful write or update
    -- of an object is reflected in a subsequent read operation of that same
    -- object.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory.
    -- For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | A list of operations that are part of the batch.
    operations :: [BatchReadOperation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchRead' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistencyLevel', 'batchRead_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'directoryArn', 'batchRead_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory.
-- For more information, see arns.
--
-- 'operations', 'batchRead_operations' - A list of operations that are part of the batch.
newBatchRead ::
  -- | 'directoryArn'
  Prelude.Text ->
  BatchRead
newBatchRead pDirectoryArn_ =
  BatchRead'
    { consistencyLevel = Prelude.Nothing,
      directoryArn = pDirectoryArn_,
      operations = Prelude.mempty
    }

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
batchRead_consistencyLevel :: Lens.Lens' BatchRead (Prelude.Maybe ConsistencyLevel)
batchRead_consistencyLevel = Lens.lens (\BatchRead' {consistencyLevel} -> consistencyLevel) (\s@BatchRead' {} a -> s {consistencyLevel = a} :: BatchRead)

-- | The Amazon Resource Name (ARN) that is associated with the Directory.
-- For more information, see arns.
batchRead_directoryArn :: Lens.Lens' BatchRead Prelude.Text
batchRead_directoryArn = Lens.lens (\BatchRead' {directoryArn} -> directoryArn) (\s@BatchRead' {} a -> s {directoryArn = a} :: BatchRead)

-- | A list of operations that are part of the batch.
batchRead_operations :: Lens.Lens' BatchRead [BatchReadOperation]
batchRead_operations = Lens.lens (\BatchRead' {operations} -> operations) (\s@BatchRead' {} a -> s {operations = a} :: BatchRead) Prelude.. Lens.coerced

instance Core.AWSRequest BatchRead where
  type AWSResponse BatchRead = BatchReadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchReadResponse'
            Prelude.<$> (x Core..?> "Responses" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchRead where
  hashWithSalt _salt BatchRead' {..} =
    _salt `Prelude.hashWithSalt` consistencyLevel
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` operations

instance Prelude.NFData BatchRead where
  rnf BatchRead' {..} =
    Prelude.rnf consistencyLevel
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf operations

instance Core.ToHeaders BatchRead where
  toHeaders BatchRead' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON BatchRead where
  toJSON BatchRead' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Operations" Core..= operations)]
      )

instance Core.ToPath BatchRead where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/batchread"

instance Core.ToQuery BatchRead where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchReadResponse' smart constructor.
data BatchReadResponse = BatchReadResponse'
  { -- | A list of all the responses for each batch read.
    responses :: Prelude.Maybe [BatchReadOperationResponse],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchReadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responses', 'batchReadResponse_responses' - A list of all the responses for each batch read.
--
-- 'httpStatus', 'batchReadResponse_httpStatus' - The response's http status code.
newBatchReadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchReadResponse
newBatchReadResponse pHttpStatus_ =
  BatchReadResponse'
    { responses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of all the responses for each batch read.
batchReadResponse_responses :: Lens.Lens' BatchReadResponse (Prelude.Maybe [BatchReadOperationResponse])
batchReadResponse_responses = Lens.lens (\BatchReadResponse' {responses} -> responses) (\s@BatchReadResponse' {} a -> s {responses = a} :: BatchReadResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchReadResponse_httpStatus :: Lens.Lens' BatchReadResponse Prelude.Int
batchReadResponse_httpStatus = Lens.lens (\BatchReadResponse' {httpStatus} -> httpStatus) (\s@BatchReadResponse' {} a -> s {httpStatus = a} :: BatchReadResponse)

instance Prelude.NFData BatchReadResponse where
  rnf BatchReadResponse' {..} =
    Prelude.rnf responses
      `Prelude.seq` Prelude.rnf httpStatus
