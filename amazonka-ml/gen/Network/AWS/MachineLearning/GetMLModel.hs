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
-- Module      : Network.AWS.MachineLearning.GetMLModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an @MLModel@ that includes detailed metadata, data source
-- information, and the current status of the @MLModel@.
--
-- @GetMLModel@ provides results in normal or verbose format.
module Network.AWS.MachineLearning.GetMLModel
  ( -- * Creating a Request
    GetMLModel (..),
    newGetMLModel,

    -- * Request Lenses
    getMLModel_verbose,
    getMLModel_mLModelId,

    -- * Destructuring the Response
    GetMLModelResponse (..),
    newGetMLModelResponse,

    -- * Response Lenses
    getMLModelResponse_status,
    getMLModelResponse_startedAt,
    getMLModelResponse_schema,
    getMLModelResponse_message,
    getMLModelResponse_recipe,
    getMLModelResponse_endpointInfo,
    getMLModelResponse_scoreThresholdLastUpdatedAt,
    getMLModelResponse_createdAt,
    getMLModelResponse_trainingParameters,
    getMLModelResponse_finishedAt,
    getMLModelResponse_scoreThreshold,
    getMLModelResponse_createdByIamUser,
    getMLModelResponse_name,
    getMLModelResponse_mLModelType,
    getMLModelResponse_mLModelId,
    getMLModelResponse_sizeInBytes,
    getMLModelResponse_inputDataLocationS3,
    getMLModelResponse_computeTime,
    getMLModelResponse_trainingDataSourceId,
    getMLModelResponse_lastUpdatedAt,
    getMLModelResponse_logUri,
    getMLModelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMLModel' smart constructor.
data GetMLModel = GetMLModel'
  { -- | Specifies whether the @GetMLModel@ operation should return @Recipe@.
    --
    -- If true, @Recipe@ is returned.
    --
    -- If false, @Recipe@ is not returned.
    verbose :: Core.Maybe Core.Bool,
    -- | The ID assigned to the @MLModel@ at creation.
    mLModelId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMLModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verbose', 'getMLModel_verbose' - Specifies whether the @GetMLModel@ operation should return @Recipe@.
--
-- If true, @Recipe@ is returned.
--
-- If false, @Recipe@ is not returned.
--
-- 'mLModelId', 'getMLModel_mLModelId' - The ID assigned to the @MLModel@ at creation.
newGetMLModel ::
  -- | 'mLModelId'
  Core.Text ->
  GetMLModel
newGetMLModel pMLModelId_ =
  GetMLModel'
    { verbose = Core.Nothing,
      mLModelId = pMLModelId_
    }

-- | Specifies whether the @GetMLModel@ operation should return @Recipe@.
--
-- If true, @Recipe@ is returned.
--
-- If false, @Recipe@ is not returned.
getMLModel_verbose :: Lens.Lens' GetMLModel (Core.Maybe Core.Bool)
getMLModel_verbose = Lens.lens (\GetMLModel' {verbose} -> verbose) (\s@GetMLModel' {} a -> s {verbose = a} :: GetMLModel)

-- | The ID assigned to the @MLModel@ at creation.
getMLModel_mLModelId :: Lens.Lens' GetMLModel Core.Text
getMLModel_mLModelId = Lens.lens (\GetMLModel' {mLModelId} -> mLModelId) (\s@GetMLModel' {} a -> s {mLModelId = a} :: GetMLModel)

instance Core.AWSRequest GetMLModel where
  type AWSResponse GetMLModel = GetMLModelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMLModelResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "StartedAt")
            Core.<*> (x Core..?> "Schema")
            Core.<*> (x Core..?> "Message")
            Core.<*> (x Core..?> "Recipe")
            Core.<*> (x Core..?> "EndpointInfo")
            Core.<*> (x Core..?> "ScoreThresholdLastUpdatedAt")
            Core.<*> (x Core..?> "CreatedAt")
            Core.<*> ( x Core..?> "TrainingParameters"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "FinishedAt")
            Core.<*> (x Core..?> "ScoreThreshold")
            Core.<*> (x Core..?> "CreatedByIamUser")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "MLModelType")
            Core.<*> (x Core..?> "MLModelId")
            Core.<*> (x Core..?> "SizeInBytes")
            Core.<*> (x Core..?> "InputDataLocationS3")
            Core.<*> (x Core..?> "ComputeTime")
            Core.<*> (x Core..?> "TrainingDataSourceId")
            Core.<*> (x Core..?> "LastUpdatedAt")
            Core.<*> (x Core..?> "LogUri")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetMLModel

instance Core.NFData GetMLModel

instance Core.ToHeaders GetMLModel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonML_20141212.GetMLModel" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMLModel where
  toJSON GetMLModel' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Verbose" Core..=) Core.<$> verbose,
            Core.Just ("MLModelId" Core..= mLModelId)
          ]
      )

instance Core.ToPath GetMLModel where
  toPath = Core.const "/"

instance Core.ToQuery GetMLModel where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetMLModel@ operation, and provides detailed
-- information about a @MLModel@.
--
-- /See:/ 'newGetMLModelResponse' smart constructor.
data GetMLModelResponse = GetMLModelResponse'
  { -- | The current status of the @MLModel@. This element can have one of the
    -- following values:
    --
    -- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
    --     to describe a @MLModel@.
    -- -   @INPROGRESS@ - The request is processing.
    -- -   @FAILED@ - The request did not run to completion. The ML model
    --     isn\'t usable.
    -- -   @COMPLETED@ - The request completed successfully.
    -- -   @DELETED@ - The @MLModel@ is marked as deleted. It isn\'t usable.
    status :: Core.Maybe EntityStatus,
    -- | The epoch time when Amazon Machine Learning marked the @MLModel@ as
    -- @INPROGRESS@. @StartedAt@ isn\'t available if the @MLModel@ is in the
    -- @PENDING@ state.
    startedAt :: Core.Maybe Core.POSIX,
    -- | The schema used by all of the data files referenced by the @DataSource@.
    --
    -- Note
    --
    -- This parameter is provided as part of the verbose format.
    schema :: Core.Maybe Core.Text,
    -- | A description of the most recent details about accessing the @MLModel@.
    message :: Core.Maybe Core.Text,
    -- | The recipe to use when training the @MLModel@. The @Recipe@ provides
    -- detailed information about the observation data to use during training,
    -- and manipulations to perform on the observation data during training.
    --
    -- Note
    --
    -- This parameter is provided as part of the verbose format.
    recipe :: Core.Maybe Core.Text,
    -- | The current endpoint of the @MLModel@
    endpointInfo :: Core.Maybe RealtimeEndpointInfo,
    -- | The time of the most recent edit to the @ScoreThreshold@. The time is
    -- expressed in epoch time.
    scoreThresholdLastUpdatedAt :: Core.Maybe Core.POSIX,
    -- | The time that the @MLModel@ was created. The time is expressed in epoch
    -- time.
    createdAt :: Core.Maybe Core.POSIX,
    -- | A list of the training parameters in the @MLModel@. The list is
    -- implemented as a map of key-value pairs.
    --
    -- The following is the current set of training parameters:
    --
    -- -   @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model.
    --     Depending on the input data, the size of the model might affect its
    --     performance.
    --
    --     The value is an integer that ranges from @100000@ to @2147483648@.
    --     The default value is @33554432@.
    --
    -- -   @sgd.maxPasses@ - The number of times that the training process
    --     traverses the observations to build the @MLModel@. The value is an
    --     integer that ranges from @1@ to @10000@. The default value is @10@.
    --
    -- -   @sgd.shuffleType@ - Whether Amazon ML shuffles the training data.
    --     Shuffling data improves a model\'s ability to find the optimal
    --     solution for a variety of data types. The valid values are @auto@
    --     and @none@. The default value is @none@. We strongly recommend that
    --     you shuffle your data.
    --
    -- -   @sgd.l1RegularizationAmount@ - The coefficient regularization L1
    --     norm. It controls overfitting the data by penalizing large
    --     coefficients. This tends to drive coefficients to zero, resulting in
    --     a sparse feature set. If you use this parameter, start by specifying
    --     a small value, such as @1.0E-08@.
    --
    --     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
    --     default is to not use L1 normalization. This parameter can\'t be
    --     used when @L2@ is specified. Use this parameter sparingly.
    --
    -- -   @sgd.l2RegularizationAmount@ - The coefficient regularization L2
    --     norm. It controls overfitting the data by penalizing large
    --     coefficients. This tends to drive coefficients to small, nonzero
    --     values. If you use this parameter, start by specifying a small
    --     value, such as @1.0E-08@.
    --
    --     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
    --     default is to not use L2 normalization. This parameter can\'t be
    --     used when @L1@ is specified. Use this parameter sparingly.
    trainingParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The epoch time when Amazon Machine Learning marked the @MLModel@ as
    -- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
    -- @MLModel@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Core.Maybe Core.POSIX,
    -- | The scoring threshold is used in binary classification @MLModel@ models.
    -- It marks the boundary between a positive prediction and a negative
    -- prediction.
    --
    -- Output values greater than or equal to the threshold receive a positive
    -- result from the MLModel, such as @true@. Output values less than the
    -- threshold receive a negative response from the MLModel, such as @false@.
    scoreThreshold :: Core.Maybe Core.Double,
    -- | The AWS user account from which the @MLModel@ was created. The account
    -- type can be either an AWS root account or an AWS Identity and Access
    -- Management (IAM) user account.
    createdByIamUser :: Core.Maybe Core.Text,
    -- | A user-supplied name or description of the @MLModel@.
    name :: Core.Maybe Core.Text,
    -- | Identifies the @MLModel@ category. The following are the available
    -- types:
    --
    -- -   REGRESSION -- Produces a numeric result. For example, \"What price
    --     should a house be listed at?\"
    -- -   BINARY -- Produces one of two possible results. For example, \"Is
    --     this an e-commerce website?\"
    -- -   MULTICLASS -- Produces one of several possible results. For example,
    --     \"Is this a HIGH, LOW or MEDIUM risk trade?\"
    mLModelType :: Core.Maybe MLModelType,
    -- | The MLModel ID, which is same as the @MLModelId@ in the request.
    mLModelId :: Core.Maybe Core.Text,
    sizeInBytes :: Core.Maybe Core.Integer,
    -- | The location of the data file or directory in Amazon Simple Storage
    -- Service (Amazon S3).
    inputDataLocationS3 :: Core.Maybe Core.Text,
    -- | The approximate CPU time in milliseconds that Amazon Machine Learning
    -- spent processing the @MLModel@, normalized and scaled on computation
    -- resources. @ComputeTime@ is only available if the @MLModel@ is in the
    -- @COMPLETED@ state.
    computeTime :: Core.Maybe Core.Integer,
    -- | The ID of the training @DataSource@.
    trainingDataSourceId :: Core.Maybe Core.Text,
    -- | The time of the most recent edit to the @MLModel@. The time is expressed
    -- in epoch time.
    lastUpdatedAt :: Core.Maybe Core.POSIX,
    -- | A link to the file that contains logs of the @CreateMLModel@ operation.
    logUri :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMLModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getMLModelResponse_status' - The current status of the @MLModel@. This element can have one of the
-- following values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to describe a @MLModel@.
-- -   @INPROGRESS@ - The request is processing.
-- -   @FAILED@ - The request did not run to completion. The ML model
--     isn\'t usable.
-- -   @COMPLETED@ - The request completed successfully.
-- -   @DELETED@ - The @MLModel@ is marked as deleted. It isn\'t usable.
--
-- 'startedAt', 'getMLModelResponse_startedAt' - The epoch time when Amazon Machine Learning marked the @MLModel@ as
-- @INPROGRESS@. @StartedAt@ isn\'t available if the @MLModel@ is in the
-- @PENDING@ state.
--
-- 'schema', 'getMLModelResponse_schema' - The schema used by all of the data files referenced by the @DataSource@.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
--
-- 'message', 'getMLModelResponse_message' - A description of the most recent details about accessing the @MLModel@.
--
-- 'recipe', 'getMLModelResponse_recipe' - The recipe to use when training the @MLModel@. The @Recipe@ provides
-- detailed information about the observation data to use during training,
-- and manipulations to perform on the observation data during training.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
--
-- 'endpointInfo', 'getMLModelResponse_endpointInfo' - The current endpoint of the @MLModel@
--
-- 'scoreThresholdLastUpdatedAt', 'getMLModelResponse_scoreThresholdLastUpdatedAt' - The time of the most recent edit to the @ScoreThreshold@. The time is
-- expressed in epoch time.
--
-- 'createdAt', 'getMLModelResponse_createdAt' - The time that the @MLModel@ was created. The time is expressed in epoch
-- time.
--
-- 'trainingParameters', 'getMLModelResponse_trainingParameters' - A list of the training parameters in the @MLModel@. The list is
-- implemented as a map of key-value pairs.
--
-- The following is the current set of training parameters:
--
-- -   @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model.
--     Depending on the input data, the size of the model might affect its
--     performance.
--
--     The value is an integer that ranges from @100000@ to @2147483648@.
--     The default value is @33554432@.
--
-- -   @sgd.maxPasses@ - The number of times that the training process
--     traverses the observations to build the @MLModel@. The value is an
--     integer that ranges from @1@ to @10000@. The default value is @10@.
--
-- -   @sgd.shuffleType@ - Whether Amazon ML shuffles the training data.
--     Shuffling data improves a model\'s ability to find the optimal
--     solution for a variety of data types. The valid values are @auto@
--     and @none@. The default value is @none@. We strongly recommend that
--     you shuffle your data.
--
-- -   @sgd.l1RegularizationAmount@ - The coefficient regularization L1
--     norm. It controls overfitting the data by penalizing large
--     coefficients. This tends to drive coefficients to zero, resulting in
--     a sparse feature set. If you use this parameter, start by specifying
--     a small value, such as @1.0E-08@.
--
--     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
--     default is to not use L1 normalization. This parameter can\'t be
--     used when @L2@ is specified. Use this parameter sparingly.
--
-- -   @sgd.l2RegularizationAmount@ - The coefficient regularization L2
--     norm. It controls overfitting the data by penalizing large
--     coefficients. This tends to drive coefficients to small, nonzero
--     values. If you use this parameter, start by specifying a small
--     value, such as @1.0E-08@.
--
--     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
--     default is to not use L2 normalization. This parameter can\'t be
--     used when @L1@ is specified. Use this parameter sparingly.
--
-- 'finishedAt', 'getMLModelResponse_finishedAt' - The epoch time when Amazon Machine Learning marked the @MLModel@ as
-- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @MLModel@ is in the @COMPLETED@ or @FAILED@ state.
--
-- 'scoreThreshold', 'getMLModelResponse_scoreThreshold' - The scoring threshold is used in binary classification @MLModel@ models.
-- It marks the boundary between a positive prediction and a negative
-- prediction.
--
-- Output values greater than or equal to the threshold receive a positive
-- result from the MLModel, such as @true@. Output values less than the
-- threshold receive a negative response from the MLModel, such as @false@.
--
-- 'createdByIamUser', 'getMLModelResponse_createdByIamUser' - The AWS user account from which the @MLModel@ was created. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
--
-- 'name', 'getMLModelResponse_name' - A user-supplied name or description of the @MLModel@.
--
-- 'mLModelType', 'getMLModelResponse_mLModelType' - Identifies the @MLModel@ category. The following are the available
-- types:
--
-- -   REGRESSION -- Produces a numeric result. For example, \"What price
--     should a house be listed at?\"
-- -   BINARY -- Produces one of two possible results. For example, \"Is
--     this an e-commerce website?\"
-- -   MULTICLASS -- Produces one of several possible results. For example,
--     \"Is this a HIGH, LOW or MEDIUM risk trade?\"
--
-- 'mLModelId', 'getMLModelResponse_mLModelId' - The MLModel ID, which is same as the @MLModelId@ in the request.
--
-- 'sizeInBytes', 'getMLModelResponse_sizeInBytes' - Undocumented member.
--
-- 'inputDataLocationS3', 'getMLModelResponse_inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
--
-- 'computeTime', 'getMLModelResponse_computeTime' - The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @MLModel@, normalized and scaled on computation
-- resources. @ComputeTime@ is only available if the @MLModel@ is in the
-- @COMPLETED@ state.
--
-- 'trainingDataSourceId', 'getMLModelResponse_trainingDataSourceId' - The ID of the training @DataSource@.
--
-- 'lastUpdatedAt', 'getMLModelResponse_lastUpdatedAt' - The time of the most recent edit to the @MLModel@. The time is expressed
-- in epoch time.
--
-- 'logUri', 'getMLModelResponse_logUri' - A link to the file that contains logs of the @CreateMLModel@ operation.
--
-- 'httpStatus', 'getMLModelResponse_httpStatus' - The response's http status code.
newGetMLModelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetMLModelResponse
newGetMLModelResponse pHttpStatus_ =
  GetMLModelResponse'
    { status = Core.Nothing,
      startedAt = Core.Nothing,
      schema = Core.Nothing,
      message = Core.Nothing,
      recipe = Core.Nothing,
      endpointInfo = Core.Nothing,
      scoreThresholdLastUpdatedAt = Core.Nothing,
      createdAt = Core.Nothing,
      trainingParameters = Core.Nothing,
      finishedAt = Core.Nothing,
      scoreThreshold = Core.Nothing,
      createdByIamUser = Core.Nothing,
      name = Core.Nothing,
      mLModelType = Core.Nothing,
      mLModelId = Core.Nothing,
      sizeInBytes = Core.Nothing,
      inputDataLocationS3 = Core.Nothing,
      computeTime = Core.Nothing,
      trainingDataSourceId = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      logUri = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the @MLModel@. This element can have one of the
-- following values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to describe a @MLModel@.
-- -   @INPROGRESS@ - The request is processing.
-- -   @FAILED@ - The request did not run to completion. The ML model
--     isn\'t usable.
-- -   @COMPLETED@ - The request completed successfully.
-- -   @DELETED@ - The @MLModel@ is marked as deleted. It isn\'t usable.
getMLModelResponse_status :: Lens.Lens' GetMLModelResponse (Core.Maybe EntityStatus)
getMLModelResponse_status = Lens.lens (\GetMLModelResponse' {status} -> status) (\s@GetMLModelResponse' {} a -> s {status = a} :: GetMLModelResponse)

-- | The epoch time when Amazon Machine Learning marked the @MLModel@ as
-- @INPROGRESS@. @StartedAt@ isn\'t available if the @MLModel@ is in the
-- @PENDING@ state.
getMLModelResponse_startedAt :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.UTCTime)
getMLModelResponse_startedAt = Lens.lens (\GetMLModelResponse' {startedAt} -> startedAt) (\s@GetMLModelResponse' {} a -> s {startedAt = a} :: GetMLModelResponse) Core.. Lens.mapping Core._Time

-- | The schema used by all of the data files referenced by the @DataSource@.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
getMLModelResponse_schema :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Text)
getMLModelResponse_schema = Lens.lens (\GetMLModelResponse' {schema} -> schema) (\s@GetMLModelResponse' {} a -> s {schema = a} :: GetMLModelResponse)

-- | A description of the most recent details about accessing the @MLModel@.
getMLModelResponse_message :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Text)
getMLModelResponse_message = Lens.lens (\GetMLModelResponse' {message} -> message) (\s@GetMLModelResponse' {} a -> s {message = a} :: GetMLModelResponse)

-- | The recipe to use when training the @MLModel@. The @Recipe@ provides
-- detailed information about the observation data to use during training,
-- and manipulations to perform on the observation data during training.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
getMLModelResponse_recipe :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Text)
getMLModelResponse_recipe = Lens.lens (\GetMLModelResponse' {recipe} -> recipe) (\s@GetMLModelResponse' {} a -> s {recipe = a} :: GetMLModelResponse)

-- | The current endpoint of the @MLModel@
getMLModelResponse_endpointInfo :: Lens.Lens' GetMLModelResponse (Core.Maybe RealtimeEndpointInfo)
getMLModelResponse_endpointInfo = Lens.lens (\GetMLModelResponse' {endpointInfo} -> endpointInfo) (\s@GetMLModelResponse' {} a -> s {endpointInfo = a} :: GetMLModelResponse)

-- | The time of the most recent edit to the @ScoreThreshold@. The time is
-- expressed in epoch time.
getMLModelResponse_scoreThresholdLastUpdatedAt :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.UTCTime)
getMLModelResponse_scoreThresholdLastUpdatedAt = Lens.lens (\GetMLModelResponse' {scoreThresholdLastUpdatedAt} -> scoreThresholdLastUpdatedAt) (\s@GetMLModelResponse' {} a -> s {scoreThresholdLastUpdatedAt = a} :: GetMLModelResponse) Core.. Lens.mapping Core._Time

-- | The time that the @MLModel@ was created. The time is expressed in epoch
-- time.
getMLModelResponse_createdAt :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.UTCTime)
getMLModelResponse_createdAt = Lens.lens (\GetMLModelResponse' {createdAt} -> createdAt) (\s@GetMLModelResponse' {} a -> s {createdAt = a} :: GetMLModelResponse) Core.. Lens.mapping Core._Time

-- | A list of the training parameters in the @MLModel@. The list is
-- implemented as a map of key-value pairs.
--
-- The following is the current set of training parameters:
--
-- -   @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model.
--     Depending on the input data, the size of the model might affect its
--     performance.
--
--     The value is an integer that ranges from @100000@ to @2147483648@.
--     The default value is @33554432@.
--
-- -   @sgd.maxPasses@ - The number of times that the training process
--     traverses the observations to build the @MLModel@. The value is an
--     integer that ranges from @1@ to @10000@. The default value is @10@.
--
-- -   @sgd.shuffleType@ - Whether Amazon ML shuffles the training data.
--     Shuffling data improves a model\'s ability to find the optimal
--     solution for a variety of data types. The valid values are @auto@
--     and @none@. The default value is @none@. We strongly recommend that
--     you shuffle your data.
--
-- -   @sgd.l1RegularizationAmount@ - The coefficient regularization L1
--     norm. It controls overfitting the data by penalizing large
--     coefficients. This tends to drive coefficients to zero, resulting in
--     a sparse feature set. If you use this parameter, start by specifying
--     a small value, such as @1.0E-08@.
--
--     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
--     default is to not use L1 normalization. This parameter can\'t be
--     used when @L2@ is specified. Use this parameter sparingly.
--
-- -   @sgd.l2RegularizationAmount@ - The coefficient regularization L2
--     norm. It controls overfitting the data by penalizing large
--     coefficients. This tends to drive coefficients to small, nonzero
--     values. If you use this parameter, start by specifying a small
--     value, such as @1.0E-08@.
--
--     The value is a double that ranges from @0@ to @MAX_DOUBLE@. The
--     default is to not use L2 normalization. This parameter can\'t be
--     used when @L1@ is specified. Use this parameter sparingly.
getMLModelResponse_trainingParameters :: Lens.Lens' GetMLModelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getMLModelResponse_trainingParameters = Lens.lens (\GetMLModelResponse' {trainingParameters} -> trainingParameters) (\s@GetMLModelResponse' {} a -> s {trainingParameters = a} :: GetMLModelResponse) Core.. Lens.mapping Lens._Coerce

-- | The epoch time when Amazon Machine Learning marked the @MLModel@ as
-- @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @MLModel@ is in the @COMPLETED@ or @FAILED@ state.
getMLModelResponse_finishedAt :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.UTCTime)
getMLModelResponse_finishedAt = Lens.lens (\GetMLModelResponse' {finishedAt} -> finishedAt) (\s@GetMLModelResponse' {} a -> s {finishedAt = a} :: GetMLModelResponse) Core.. Lens.mapping Core._Time

-- | The scoring threshold is used in binary classification @MLModel@ models.
-- It marks the boundary between a positive prediction and a negative
-- prediction.
--
-- Output values greater than or equal to the threshold receive a positive
-- result from the MLModel, such as @true@. Output values less than the
-- threshold receive a negative response from the MLModel, such as @false@.
getMLModelResponse_scoreThreshold :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Double)
getMLModelResponse_scoreThreshold = Lens.lens (\GetMLModelResponse' {scoreThreshold} -> scoreThreshold) (\s@GetMLModelResponse' {} a -> s {scoreThreshold = a} :: GetMLModelResponse)

-- | The AWS user account from which the @MLModel@ was created. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
getMLModelResponse_createdByIamUser :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Text)
getMLModelResponse_createdByIamUser = Lens.lens (\GetMLModelResponse' {createdByIamUser} -> createdByIamUser) (\s@GetMLModelResponse' {} a -> s {createdByIamUser = a} :: GetMLModelResponse)

-- | A user-supplied name or description of the @MLModel@.
getMLModelResponse_name :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Text)
getMLModelResponse_name = Lens.lens (\GetMLModelResponse' {name} -> name) (\s@GetMLModelResponse' {} a -> s {name = a} :: GetMLModelResponse)

-- | Identifies the @MLModel@ category. The following are the available
-- types:
--
-- -   REGRESSION -- Produces a numeric result. For example, \"What price
--     should a house be listed at?\"
-- -   BINARY -- Produces one of two possible results. For example, \"Is
--     this an e-commerce website?\"
-- -   MULTICLASS -- Produces one of several possible results. For example,
--     \"Is this a HIGH, LOW or MEDIUM risk trade?\"
getMLModelResponse_mLModelType :: Lens.Lens' GetMLModelResponse (Core.Maybe MLModelType)
getMLModelResponse_mLModelType = Lens.lens (\GetMLModelResponse' {mLModelType} -> mLModelType) (\s@GetMLModelResponse' {} a -> s {mLModelType = a} :: GetMLModelResponse)

-- | The MLModel ID, which is same as the @MLModelId@ in the request.
getMLModelResponse_mLModelId :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Text)
getMLModelResponse_mLModelId = Lens.lens (\GetMLModelResponse' {mLModelId} -> mLModelId) (\s@GetMLModelResponse' {} a -> s {mLModelId = a} :: GetMLModelResponse)

-- | Undocumented member.
getMLModelResponse_sizeInBytes :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Integer)
getMLModelResponse_sizeInBytes = Lens.lens (\GetMLModelResponse' {sizeInBytes} -> sizeInBytes) (\s@GetMLModelResponse' {} a -> s {sizeInBytes = a} :: GetMLModelResponse)

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
getMLModelResponse_inputDataLocationS3 :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Text)
getMLModelResponse_inputDataLocationS3 = Lens.lens (\GetMLModelResponse' {inputDataLocationS3} -> inputDataLocationS3) (\s@GetMLModelResponse' {} a -> s {inputDataLocationS3 = a} :: GetMLModelResponse)

-- | The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @MLModel@, normalized and scaled on computation
-- resources. @ComputeTime@ is only available if the @MLModel@ is in the
-- @COMPLETED@ state.
getMLModelResponse_computeTime :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Integer)
getMLModelResponse_computeTime = Lens.lens (\GetMLModelResponse' {computeTime} -> computeTime) (\s@GetMLModelResponse' {} a -> s {computeTime = a} :: GetMLModelResponse)

-- | The ID of the training @DataSource@.
getMLModelResponse_trainingDataSourceId :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Text)
getMLModelResponse_trainingDataSourceId = Lens.lens (\GetMLModelResponse' {trainingDataSourceId} -> trainingDataSourceId) (\s@GetMLModelResponse' {} a -> s {trainingDataSourceId = a} :: GetMLModelResponse)

-- | The time of the most recent edit to the @MLModel@. The time is expressed
-- in epoch time.
getMLModelResponse_lastUpdatedAt :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.UTCTime)
getMLModelResponse_lastUpdatedAt = Lens.lens (\GetMLModelResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetMLModelResponse' {} a -> s {lastUpdatedAt = a} :: GetMLModelResponse) Core.. Lens.mapping Core._Time

-- | A link to the file that contains logs of the @CreateMLModel@ operation.
getMLModelResponse_logUri :: Lens.Lens' GetMLModelResponse (Core.Maybe Core.Text)
getMLModelResponse_logUri = Lens.lens (\GetMLModelResponse' {logUri} -> logUri) (\s@GetMLModelResponse' {} a -> s {logUri = a} :: GetMLModelResponse)

-- | The response's http status code.
getMLModelResponse_httpStatus :: Lens.Lens' GetMLModelResponse Core.Int
getMLModelResponse_httpStatus = Lens.lens (\GetMLModelResponse' {httpStatus} -> httpStatus) (\s@GetMLModelResponse' {} a -> s {httpStatus = a} :: GetMLModelResponse)

instance Core.NFData GetMLModelResponse
