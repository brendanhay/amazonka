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
-- Module      : Network.AWS.MachineLearning.CreateBatchPrediction
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.MachineLearning.CreateBatchPrediction
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBatchPrediction' smart constructor.
data CreateBatchPrediction = CreateBatchPrediction'
  { -- | A user-supplied name or description of the @BatchPrediction@.
    -- @BatchPredictionName@ can only use the UTF-8 character set.
    batchPredictionName :: Core.Maybe Core.Text,
    -- | A user-supplied ID that uniquely identifies the @BatchPrediction@.
    batchPredictionId :: Core.Text,
    -- | The ID of the @MLModel@ that will generate predictions for the group of
    -- observations.
    mLModelId :: Core.Text,
    -- | The ID of the @DataSource@ that points to the group of observations to
    -- predict.
    batchPredictionDataSourceId :: Core.Text,
    -- | The location of an Amazon Simple Storage Service (Amazon S3) bucket or
    -- directory to store the batch prediction results. The following
    -- substrings are not allowed in the @s3 key@ portion of the @outputURI@
    -- field: \':\', \'\/\/\', \'\/.\/\', \'\/..\/\'.
    --
    -- Amazon ML needs permissions to store and retrieve the logs on your
    -- behalf. For information about how to set permissions, see the
    -- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
    outputUri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
newCreateBatchPrediction ::
  -- | 'batchPredictionId'
  Core.Text ->
  -- | 'mLModelId'
  Core.Text ->
  -- | 'batchPredictionDataSourceId'
  Core.Text ->
  -- | 'outputUri'
  Core.Text ->
  CreateBatchPrediction
newCreateBatchPrediction
  pBatchPredictionId_
  pMLModelId_
  pBatchPredictionDataSourceId_
  pOutputUri_ =
    CreateBatchPrediction'
      { batchPredictionName =
          Core.Nothing,
        batchPredictionId = pBatchPredictionId_,
        mLModelId = pMLModelId_,
        batchPredictionDataSourceId =
          pBatchPredictionDataSourceId_,
        outputUri = pOutputUri_
      }

-- | A user-supplied name or description of the @BatchPrediction@.
-- @BatchPredictionName@ can only use the UTF-8 character set.
createBatchPrediction_batchPredictionName :: Lens.Lens' CreateBatchPrediction (Core.Maybe Core.Text)
createBatchPrediction_batchPredictionName = Lens.lens (\CreateBatchPrediction' {batchPredictionName} -> batchPredictionName) (\s@CreateBatchPrediction' {} a -> s {batchPredictionName = a} :: CreateBatchPrediction)

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@.
createBatchPrediction_batchPredictionId :: Lens.Lens' CreateBatchPrediction Core.Text
createBatchPrediction_batchPredictionId = Lens.lens (\CreateBatchPrediction' {batchPredictionId} -> batchPredictionId) (\s@CreateBatchPrediction' {} a -> s {batchPredictionId = a} :: CreateBatchPrediction)

-- | The ID of the @MLModel@ that will generate predictions for the group of
-- observations.
createBatchPrediction_mLModelId :: Lens.Lens' CreateBatchPrediction Core.Text
createBatchPrediction_mLModelId = Lens.lens (\CreateBatchPrediction' {mLModelId} -> mLModelId) (\s@CreateBatchPrediction' {} a -> s {mLModelId = a} :: CreateBatchPrediction)

-- | The ID of the @DataSource@ that points to the group of observations to
-- predict.
createBatchPrediction_batchPredictionDataSourceId :: Lens.Lens' CreateBatchPrediction Core.Text
createBatchPrediction_batchPredictionDataSourceId = Lens.lens (\CreateBatchPrediction' {batchPredictionDataSourceId} -> batchPredictionDataSourceId) (\s@CreateBatchPrediction' {} a -> s {batchPredictionDataSourceId = a} :: CreateBatchPrediction)

-- | The location of an Amazon Simple Storage Service (Amazon S3) bucket or
-- directory to store the batch prediction results. The following
-- substrings are not allowed in the @s3 key@ portion of the @outputURI@
-- field: \':\', \'\/\/\', \'\/.\/\', \'\/..\/\'.
--
-- Amazon ML needs permissions to store and retrieve the logs on your
-- behalf. For information about how to set permissions, see the
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
createBatchPrediction_outputUri :: Lens.Lens' CreateBatchPrediction Core.Text
createBatchPrediction_outputUri = Lens.lens (\CreateBatchPrediction' {outputUri} -> outputUri) (\s@CreateBatchPrediction' {} a -> s {outputUri = a} :: CreateBatchPrediction)

instance Core.AWSRequest CreateBatchPrediction where
  type
    AWSResponse CreateBatchPrediction =
      CreateBatchPredictionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBatchPredictionResponse'
            Core.<$> (x Core..?> "BatchPredictionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateBatchPrediction

instance Core.NFData CreateBatchPrediction

instance Core.ToHeaders CreateBatchPrediction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.CreateBatchPrediction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateBatchPrediction where
  toJSON CreateBatchPrediction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BatchPredictionName" Core..=)
              Core.<$> batchPredictionName,
            Core.Just
              ("BatchPredictionId" Core..= batchPredictionId),
            Core.Just ("MLModelId" Core..= mLModelId),
            Core.Just
              ( "BatchPredictionDataSourceId"
                  Core..= batchPredictionDataSourceId
              ),
            Core.Just ("OutputUri" Core..= outputUri)
          ]
      )

instance Core.ToPath CreateBatchPrediction where
  toPath = Core.const "/"

instance Core.ToQuery CreateBatchPrediction where
  toQuery = Core.const Core.mempty

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
    batchPredictionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateBatchPredictionResponse
newCreateBatchPredictionResponse pHttpStatus_ =
  CreateBatchPredictionResponse'
    { batchPredictionId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@. This
-- value is identical to the value of the @BatchPredictionId@ in the
-- request.
createBatchPredictionResponse_batchPredictionId :: Lens.Lens' CreateBatchPredictionResponse (Core.Maybe Core.Text)
createBatchPredictionResponse_batchPredictionId = Lens.lens (\CreateBatchPredictionResponse' {batchPredictionId} -> batchPredictionId) (\s@CreateBatchPredictionResponse' {} a -> s {batchPredictionId = a} :: CreateBatchPredictionResponse)

-- | The response's http status code.
createBatchPredictionResponse_httpStatus :: Lens.Lens' CreateBatchPredictionResponse Core.Int
createBatchPredictionResponse_httpStatus = Lens.lens (\CreateBatchPredictionResponse' {httpStatus} -> httpStatus) (\s@CreateBatchPredictionResponse' {} a -> s {httpStatus = a} :: CreateBatchPredictionResponse)

instance Core.NFData CreateBatchPredictionResponse
