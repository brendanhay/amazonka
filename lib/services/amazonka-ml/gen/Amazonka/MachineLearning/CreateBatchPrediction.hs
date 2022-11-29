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
-- Module      : Amazonka.MachineLearning.CreateBatchPrediction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates predictions for a group of observations. The observations to
-- process exist in one or more data files referenced by a @DataSource@.
-- This operation creates a new @BatchPrediction@, and uses an @MLModel@
-- and the data files referenced by the @DataSource@ as information
-- sources.
--
-- @CreateBatchPrediction@ is an asynchronous operation. In response to
-- @CreateBatchPrediction@, Amazon Machine Learning (Amazon ML) immediately
-- returns and sets the @BatchPrediction@ status to @PENDING@. After the
-- @BatchPrediction@ completes, Amazon ML sets the status to @COMPLETED@.
--
-- You can poll for status updates by using the GetBatchPrediction
-- operation and checking the @Status@ parameter of the result. After the
-- @COMPLETED@ status appears, the results are available in the location
-- specified by the @OutputUri@ parameter.
module Amazonka.MachineLearning.CreateBatchPrediction
  ( -- * Creating a Request
    CreateBatchPrediction (..),
    newCreateBatchPrediction,

    -- * Request Lenses
    createBatchPrediction_batchPredictionName,
    createBatchPrediction_batchPredictionId,
    createBatchPrediction_mLModelId,
    createBatchPrediction_batchPredictionDataSourceId,
    createBatchPrediction_outputUri,

    -- * Destructuring the Response
    CreateBatchPredictionResponse (..),
    newCreateBatchPredictionResponse,

    -- * Response Lenses
    createBatchPredictionResponse_batchPredictionId,
    createBatchPredictionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBatchPrediction' smart constructor.
data CreateBatchPrediction = CreateBatchPrediction'
  { -- | A user-supplied name or description of the @BatchPrediction@.
    -- @BatchPredictionName@ can only use the UTF-8 character set.
    batchPredictionName :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied ID that uniquely identifies the @BatchPrediction@.
    batchPredictionId :: Prelude.Text,
    -- | The ID of the @MLModel@ that will generate predictions for the group of
    -- observations.
    mLModelId :: Prelude.Text,
    -- | The ID of the @DataSource@ that points to the group of observations to
    -- predict.
    batchPredictionDataSourceId :: Prelude.Text,
    -- | The location of an Amazon Simple Storage Service (Amazon S3) bucket or
    -- directory to store the batch prediction results. The following
    -- substrings are not allowed in the @s3 key@ portion of the @outputURI@
    -- field: \':\', \'\/\/\', \'\/.\/\', \'\/..\/\'.
    --
    -- Amazon ML needs permissions to store and retrieve the logs on your
    -- behalf. For information about how to set permissions, see the
    -- <https://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
    outputUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBatchPrediction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchPredictionName', 'createBatchPrediction_batchPredictionName' - A user-supplied name or description of the @BatchPrediction@.
-- @BatchPredictionName@ can only use the UTF-8 character set.
--
-- 'batchPredictionId', 'createBatchPrediction_batchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@.
--
-- 'mLModelId', 'createBatchPrediction_mLModelId' - The ID of the @MLModel@ that will generate predictions for the group of
-- observations.
--
-- 'batchPredictionDataSourceId', 'createBatchPrediction_batchPredictionDataSourceId' - The ID of the @DataSource@ that points to the group of observations to
-- predict.
--
-- 'outputUri', 'createBatchPrediction_outputUri' - The location of an Amazon Simple Storage Service (Amazon S3) bucket or
-- directory to store the batch prediction results. The following
-- substrings are not allowed in the @s3 key@ portion of the @outputURI@
-- field: \':\', \'\/\/\', \'\/.\/\', \'\/..\/\'.
--
-- Amazon ML needs permissions to store and retrieve the logs on your
-- behalf. For information about how to set permissions, see the
-- <https://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
newCreateBatchPrediction ::
  -- | 'batchPredictionId'
  Prelude.Text ->
  -- | 'mLModelId'
  Prelude.Text ->
  -- | 'batchPredictionDataSourceId'
  Prelude.Text ->
  -- | 'outputUri'
  Prelude.Text ->
  CreateBatchPrediction
newCreateBatchPrediction
  pBatchPredictionId_
  pMLModelId_
  pBatchPredictionDataSourceId_
  pOutputUri_ =
    CreateBatchPrediction'
      { batchPredictionName =
          Prelude.Nothing,
        batchPredictionId = pBatchPredictionId_,
        mLModelId = pMLModelId_,
        batchPredictionDataSourceId =
          pBatchPredictionDataSourceId_,
        outputUri = pOutputUri_
      }

-- | A user-supplied name or description of the @BatchPrediction@.
-- @BatchPredictionName@ can only use the UTF-8 character set.
createBatchPrediction_batchPredictionName :: Lens.Lens' CreateBatchPrediction (Prelude.Maybe Prelude.Text)
createBatchPrediction_batchPredictionName = Lens.lens (\CreateBatchPrediction' {batchPredictionName} -> batchPredictionName) (\s@CreateBatchPrediction' {} a -> s {batchPredictionName = a} :: CreateBatchPrediction)

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@.
createBatchPrediction_batchPredictionId :: Lens.Lens' CreateBatchPrediction Prelude.Text
createBatchPrediction_batchPredictionId = Lens.lens (\CreateBatchPrediction' {batchPredictionId} -> batchPredictionId) (\s@CreateBatchPrediction' {} a -> s {batchPredictionId = a} :: CreateBatchPrediction)

-- | The ID of the @MLModel@ that will generate predictions for the group of
-- observations.
createBatchPrediction_mLModelId :: Lens.Lens' CreateBatchPrediction Prelude.Text
createBatchPrediction_mLModelId = Lens.lens (\CreateBatchPrediction' {mLModelId} -> mLModelId) (\s@CreateBatchPrediction' {} a -> s {mLModelId = a} :: CreateBatchPrediction)

-- | The ID of the @DataSource@ that points to the group of observations to
-- predict.
createBatchPrediction_batchPredictionDataSourceId :: Lens.Lens' CreateBatchPrediction Prelude.Text
createBatchPrediction_batchPredictionDataSourceId = Lens.lens (\CreateBatchPrediction' {batchPredictionDataSourceId} -> batchPredictionDataSourceId) (\s@CreateBatchPrediction' {} a -> s {batchPredictionDataSourceId = a} :: CreateBatchPrediction)

-- | The location of an Amazon Simple Storage Service (Amazon S3) bucket or
-- directory to store the batch prediction results. The following
-- substrings are not allowed in the @s3 key@ portion of the @outputURI@
-- field: \':\', \'\/\/\', \'\/.\/\', \'\/..\/\'.
--
-- Amazon ML needs permissions to store and retrieve the logs on your
-- behalf. For information about how to set permissions, see the
-- <https://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
createBatchPrediction_outputUri :: Lens.Lens' CreateBatchPrediction Prelude.Text
createBatchPrediction_outputUri = Lens.lens (\CreateBatchPrediction' {outputUri} -> outputUri) (\s@CreateBatchPrediction' {} a -> s {outputUri = a} :: CreateBatchPrediction)

instance Core.AWSRequest CreateBatchPrediction where
  type
    AWSResponse CreateBatchPrediction =
      CreateBatchPredictionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBatchPredictionResponse'
            Prelude.<$> (x Core..?> "BatchPredictionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBatchPrediction where
  hashWithSalt _salt CreateBatchPrediction' {..} =
    _salt `Prelude.hashWithSalt` batchPredictionName
      `Prelude.hashWithSalt` batchPredictionId
      `Prelude.hashWithSalt` mLModelId
      `Prelude.hashWithSalt` batchPredictionDataSourceId
      `Prelude.hashWithSalt` outputUri

instance Prelude.NFData CreateBatchPrediction where
  rnf CreateBatchPrediction' {..} =
    Prelude.rnf batchPredictionName
      `Prelude.seq` Prelude.rnf batchPredictionId
      `Prelude.seq` Prelude.rnf mLModelId
      `Prelude.seq` Prelude.rnf batchPredictionDataSourceId
      `Prelude.seq` Prelude.rnf outputUri

instance Core.ToHeaders CreateBatchPrediction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.CreateBatchPrediction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBatchPrediction where
  toJSON CreateBatchPrediction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BatchPredictionName" Core..=)
              Prelude.<$> batchPredictionName,
            Prelude.Just
              ("BatchPredictionId" Core..= batchPredictionId),
            Prelude.Just ("MLModelId" Core..= mLModelId),
            Prelude.Just
              ( "BatchPredictionDataSourceId"
                  Core..= batchPredictionDataSourceId
              ),
            Prelude.Just ("OutputUri" Core..= outputUri)
          ]
      )

instance Core.ToPath CreateBatchPrediction where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateBatchPrediction where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateBatchPrediction@ operation, and is an
-- acknowledgement that Amazon ML received the request.
--
-- The @CreateBatchPrediction@ operation is asynchronous. You can poll for
-- status updates by using the @>GetBatchPrediction@ operation and checking
-- the @Status@ parameter of the result.
--
-- /See:/ 'newCreateBatchPredictionResponse' smart constructor.
data CreateBatchPredictionResponse = CreateBatchPredictionResponse'
  { -- | A user-supplied ID that uniquely identifies the @BatchPrediction@. This
    -- value is identical to the value of the @BatchPredictionId@ in the
    -- request.
    batchPredictionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBatchPredictionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchPredictionId', 'createBatchPredictionResponse_batchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@. This
-- value is identical to the value of the @BatchPredictionId@ in the
-- request.
--
-- 'httpStatus', 'createBatchPredictionResponse_httpStatus' - The response's http status code.
newCreateBatchPredictionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBatchPredictionResponse
newCreateBatchPredictionResponse pHttpStatus_ =
  CreateBatchPredictionResponse'
    { batchPredictionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@. This
-- value is identical to the value of the @BatchPredictionId@ in the
-- request.
createBatchPredictionResponse_batchPredictionId :: Lens.Lens' CreateBatchPredictionResponse (Prelude.Maybe Prelude.Text)
createBatchPredictionResponse_batchPredictionId = Lens.lens (\CreateBatchPredictionResponse' {batchPredictionId} -> batchPredictionId) (\s@CreateBatchPredictionResponse' {} a -> s {batchPredictionId = a} :: CreateBatchPredictionResponse)

-- | The response's http status code.
createBatchPredictionResponse_httpStatus :: Lens.Lens' CreateBatchPredictionResponse Prelude.Int
createBatchPredictionResponse_httpStatus = Lens.lens (\CreateBatchPredictionResponse' {httpStatus} -> httpStatus) (\s@CreateBatchPredictionResponse' {} a -> s {httpStatus = a} :: CreateBatchPredictionResponse)

instance Prelude.NFData CreateBatchPredictionResponse where
  rnf CreateBatchPredictionResponse' {..} =
    Prelude.rnf batchPredictionId
      `Prelude.seq` Prelude.rnf httpStatus
