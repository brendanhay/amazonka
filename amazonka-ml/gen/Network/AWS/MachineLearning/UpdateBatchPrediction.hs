{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MachineLearning.UpdateBatchPrediction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @BatchPredictionName@ of a @BatchPrediction@.
--
-- You can use the @GetBatchPrediction@ operation to view the contents of
-- the updated data element.
module Network.AWS.MachineLearning.UpdateBatchPrediction
  ( -- * Creating a Request
    UpdateBatchPrediction (..),
    newUpdateBatchPrediction,

    -- * Request Lenses
    updateBatchPrediction_batchPredictionId,
    updateBatchPrediction_batchPredictionName,

    -- * Destructuring the Response
    UpdateBatchPredictionResponse (..),
    newUpdateBatchPredictionResponse,

    -- * Response Lenses
    updateBatchPredictionResponse_batchPredictionId,
    updateBatchPredictionResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateBatchPrediction' smart constructor.
data UpdateBatchPrediction = UpdateBatchPrediction'
  { -- | The ID assigned to the @BatchPrediction@ during creation.
    batchPredictionId :: Prelude.Text,
    -- | A new user-supplied name or description of the @BatchPrediction@.
    batchPredictionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateBatchPrediction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchPredictionId', 'updateBatchPrediction_batchPredictionId' - The ID assigned to the @BatchPrediction@ during creation.
--
-- 'batchPredictionName', 'updateBatchPrediction_batchPredictionName' - A new user-supplied name or description of the @BatchPrediction@.
newUpdateBatchPrediction ::
  -- | 'batchPredictionId'
  Prelude.Text ->
  -- | 'batchPredictionName'
  Prelude.Text ->
  UpdateBatchPrediction
newUpdateBatchPrediction
  pBatchPredictionId_
  pBatchPredictionName_ =
    UpdateBatchPrediction'
      { batchPredictionId =
          pBatchPredictionId_,
        batchPredictionName = pBatchPredictionName_
      }

-- | The ID assigned to the @BatchPrediction@ during creation.
updateBatchPrediction_batchPredictionId :: Lens.Lens' UpdateBatchPrediction Prelude.Text
updateBatchPrediction_batchPredictionId = Lens.lens (\UpdateBatchPrediction' {batchPredictionId} -> batchPredictionId) (\s@UpdateBatchPrediction' {} a -> s {batchPredictionId = a} :: UpdateBatchPrediction)

-- | A new user-supplied name or description of the @BatchPrediction@.
updateBatchPrediction_batchPredictionName :: Lens.Lens' UpdateBatchPrediction Prelude.Text
updateBatchPrediction_batchPredictionName = Lens.lens (\UpdateBatchPrediction' {batchPredictionName} -> batchPredictionName) (\s@UpdateBatchPrediction' {} a -> s {batchPredictionName = a} :: UpdateBatchPrediction)

instance Prelude.AWSRequest UpdateBatchPrediction where
  type
    Rs UpdateBatchPrediction =
      UpdateBatchPredictionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBatchPredictionResponse'
            Prelude.<$> (x Prelude..?> "BatchPredictionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBatchPrediction

instance Prelude.NFData UpdateBatchPrediction

instance Prelude.ToHeaders UpdateBatchPrediction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.UpdateBatchPrediction" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateBatchPrediction where
  toJSON UpdateBatchPrediction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("BatchPredictionId" Prelude..= batchPredictionId),
            Prelude.Just
              ( "BatchPredictionName"
                  Prelude..= batchPredictionName
              )
          ]
      )

instance Prelude.ToPath UpdateBatchPrediction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateBatchPrediction where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of an @UpdateBatchPrediction@ operation.
--
-- You can see the updated content by using the @GetBatchPrediction@
-- operation.
--
-- /See:/ 'newUpdateBatchPredictionResponse' smart constructor.
data UpdateBatchPredictionResponse = UpdateBatchPredictionResponse'
  { -- | The ID assigned to the @BatchPrediction@ during creation. This value
    -- should be identical to the value of the @BatchPredictionId@ in the
    -- request.
    batchPredictionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateBatchPredictionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchPredictionId', 'updateBatchPredictionResponse_batchPredictionId' - The ID assigned to the @BatchPrediction@ during creation. This value
-- should be identical to the value of the @BatchPredictionId@ in the
-- request.
--
-- 'httpStatus', 'updateBatchPredictionResponse_httpStatus' - The response's http status code.
newUpdateBatchPredictionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBatchPredictionResponse
newUpdateBatchPredictionResponse pHttpStatus_ =
  UpdateBatchPredictionResponse'
    { batchPredictionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID assigned to the @BatchPrediction@ during creation. This value
-- should be identical to the value of the @BatchPredictionId@ in the
-- request.
updateBatchPredictionResponse_batchPredictionId :: Lens.Lens' UpdateBatchPredictionResponse (Prelude.Maybe Prelude.Text)
updateBatchPredictionResponse_batchPredictionId = Lens.lens (\UpdateBatchPredictionResponse' {batchPredictionId} -> batchPredictionId) (\s@UpdateBatchPredictionResponse' {} a -> s {batchPredictionId = a} :: UpdateBatchPredictionResponse)

-- | The response's http status code.
updateBatchPredictionResponse_httpStatus :: Lens.Lens' UpdateBatchPredictionResponse Prelude.Int
updateBatchPredictionResponse_httpStatus = Lens.lens (\UpdateBatchPredictionResponse' {httpStatus} -> httpStatus) (\s@UpdateBatchPredictionResponse' {} a -> s {httpStatus = a} :: UpdateBatchPredictionResponse)

instance Prelude.NFData UpdateBatchPredictionResponse
