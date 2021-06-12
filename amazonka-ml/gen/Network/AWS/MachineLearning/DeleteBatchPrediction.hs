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
-- Module      : Network.AWS.MachineLearning.DeleteBatchPrediction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to a @BatchPrediction@, rendering it
-- unusable.
--
-- After using the @DeleteBatchPrediction@ operation, you can use the
-- GetBatchPrediction operation to verify that the status of the
-- @BatchPrediction@ changed to DELETED.
--
-- __Caution:__ The result of the @DeleteBatchPrediction@ operation is
-- irreversible.
module Network.AWS.MachineLearning.DeleteBatchPrediction
  ( -- * Creating a Request
    DeleteBatchPrediction (..),
    newDeleteBatchPrediction,

    -- * Request Lenses
    deleteBatchPrediction_batchPredictionId,

    -- * Destructuring the Response
    DeleteBatchPredictionResponse (..),
    newDeleteBatchPredictionResponse,

    -- * Response Lenses
    deleteBatchPredictionResponse_batchPredictionId,
    deleteBatchPredictionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBatchPrediction' smart constructor.
data DeleteBatchPrediction = DeleteBatchPrediction'
  { -- | A user-supplied ID that uniquely identifies the @BatchPrediction@.
    batchPredictionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBatchPrediction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchPredictionId', 'deleteBatchPrediction_batchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@.
newDeleteBatchPrediction ::
  -- | 'batchPredictionId'
  Core.Text ->
  DeleteBatchPrediction
newDeleteBatchPrediction pBatchPredictionId_ =
  DeleteBatchPrediction'
    { batchPredictionId =
        pBatchPredictionId_
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@.
deleteBatchPrediction_batchPredictionId :: Lens.Lens' DeleteBatchPrediction Core.Text
deleteBatchPrediction_batchPredictionId = Lens.lens (\DeleteBatchPrediction' {batchPredictionId} -> batchPredictionId) (\s@DeleteBatchPrediction' {} a -> s {batchPredictionId = a} :: DeleteBatchPrediction)

instance Core.AWSRequest DeleteBatchPrediction where
  type
    AWSResponse DeleteBatchPrediction =
      DeleteBatchPredictionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBatchPredictionResponse'
            Core.<$> (x Core..?> "BatchPredictionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteBatchPrediction

instance Core.NFData DeleteBatchPrediction

instance Core.ToHeaders DeleteBatchPrediction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.DeleteBatchPrediction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteBatchPrediction where
  toJSON DeleteBatchPrediction' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("BatchPredictionId" Core..= batchPredictionId)
          ]
      )

instance Core.ToPath DeleteBatchPrediction where
  toPath = Core.const "/"

instance Core.ToQuery DeleteBatchPrediction where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @DeleteBatchPrediction@ operation.
--
-- You can use the @GetBatchPrediction@ operation and check the value of
-- the @Status@ parameter to see whether a @BatchPrediction@ is marked as
-- @DELETED@.
--
-- /See:/ 'newDeleteBatchPredictionResponse' smart constructor.
data DeleteBatchPredictionResponse = DeleteBatchPredictionResponse'
  { -- | A user-supplied ID that uniquely identifies the @BatchPrediction@. This
    -- value should be identical to the value of the @BatchPredictionID@ in the
    -- request.
    batchPredictionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBatchPredictionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchPredictionId', 'deleteBatchPredictionResponse_batchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@. This
-- value should be identical to the value of the @BatchPredictionID@ in the
-- request.
--
-- 'httpStatus', 'deleteBatchPredictionResponse_httpStatus' - The response's http status code.
newDeleteBatchPredictionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteBatchPredictionResponse
newDeleteBatchPredictionResponse pHttpStatus_ =
  DeleteBatchPredictionResponse'
    { batchPredictionId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@. This
-- value should be identical to the value of the @BatchPredictionID@ in the
-- request.
deleteBatchPredictionResponse_batchPredictionId :: Lens.Lens' DeleteBatchPredictionResponse (Core.Maybe Core.Text)
deleteBatchPredictionResponse_batchPredictionId = Lens.lens (\DeleteBatchPredictionResponse' {batchPredictionId} -> batchPredictionId) (\s@DeleteBatchPredictionResponse' {} a -> s {batchPredictionId = a} :: DeleteBatchPredictionResponse)

-- | The response's http status code.
deleteBatchPredictionResponse_httpStatus :: Lens.Lens' DeleteBatchPredictionResponse Core.Int
deleteBatchPredictionResponse_httpStatus = Lens.lens (\DeleteBatchPredictionResponse' {httpStatus} -> httpStatus) (\s@DeleteBatchPredictionResponse' {} a -> s {httpStatus = a} :: DeleteBatchPredictionResponse)

instance Core.NFData DeleteBatchPredictionResponse
