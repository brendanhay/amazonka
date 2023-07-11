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
-- Module      : Amazonka.MachineLearning.DeleteBatchPrediction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.MachineLearning.DeleteBatchPrediction
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBatchPrediction' smart constructor.
data DeleteBatchPrediction = DeleteBatchPrediction'
  { -- | A user-supplied ID that uniquely identifies the @BatchPrediction@.
    batchPredictionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteBatchPrediction
newDeleteBatchPrediction pBatchPredictionId_ =
  DeleteBatchPrediction'
    { batchPredictionId =
        pBatchPredictionId_
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@.
deleteBatchPrediction_batchPredictionId :: Lens.Lens' DeleteBatchPrediction Prelude.Text
deleteBatchPrediction_batchPredictionId = Lens.lens (\DeleteBatchPrediction' {batchPredictionId} -> batchPredictionId) (\s@DeleteBatchPrediction' {} a -> s {batchPredictionId = a} :: DeleteBatchPrediction)

instance Core.AWSRequest DeleteBatchPrediction where
  type
    AWSResponse DeleteBatchPrediction =
      DeleteBatchPredictionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBatchPredictionResponse'
            Prelude.<$> (x Data..?> "BatchPredictionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBatchPrediction where
  hashWithSalt _salt DeleteBatchPrediction' {..} =
    _salt `Prelude.hashWithSalt` batchPredictionId

instance Prelude.NFData DeleteBatchPrediction where
  rnf DeleteBatchPrediction' {..} =
    Prelude.rnf batchPredictionId

instance Data.ToHeaders DeleteBatchPrediction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.DeleteBatchPrediction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBatchPrediction where
  toJSON DeleteBatchPrediction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("BatchPredictionId" Data..= batchPredictionId)
          ]
      )

instance Data.ToPath DeleteBatchPrediction where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBatchPrediction where
  toQuery = Prelude.const Prelude.mempty

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
    batchPredictionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteBatchPredictionResponse
newDeleteBatchPredictionResponse pHttpStatus_ =
  DeleteBatchPredictionResponse'
    { batchPredictionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@. This
-- value should be identical to the value of the @BatchPredictionID@ in the
-- request.
deleteBatchPredictionResponse_batchPredictionId :: Lens.Lens' DeleteBatchPredictionResponse (Prelude.Maybe Prelude.Text)
deleteBatchPredictionResponse_batchPredictionId = Lens.lens (\DeleteBatchPredictionResponse' {batchPredictionId} -> batchPredictionId) (\s@DeleteBatchPredictionResponse' {} a -> s {batchPredictionId = a} :: DeleteBatchPredictionResponse)

-- | The response's http status code.
deleteBatchPredictionResponse_httpStatus :: Lens.Lens' DeleteBatchPredictionResponse Prelude.Int
deleteBatchPredictionResponse_httpStatus = Lens.lens (\DeleteBatchPredictionResponse' {httpStatus} -> httpStatus) (\s@DeleteBatchPredictionResponse' {} a -> s {httpStatus = a} :: DeleteBatchPredictionResponse)

instance Prelude.NFData DeleteBatchPredictionResponse where
  rnf DeleteBatchPredictionResponse' {..} =
    Prelude.rnf batchPredictionId
      `Prelude.seq` Prelude.rnf httpStatus
