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
-- Module      : Amazonka.MachineLearning.CreateMLModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new @MLModel@ using the @DataSource@ and the recipe as
-- information sources.
--
-- An @MLModel@ is nearly immutable. Users can update only the
-- @MLModelName@ and the @ScoreThreshold@ in an @MLModel@ without creating
-- a new @MLModel@.
--
-- @CreateMLModel@ is an asynchronous operation. In response to
-- @CreateMLModel@, Amazon Machine Learning (Amazon ML) immediately returns
-- and sets the @MLModel@ status to @PENDING@. After the @MLModel@ has been
-- created and ready is for use, Amazon ML sets the status to @COMPLETED@.
--
-- You can use the @GetMLModel@ operation to check the progress of the
-- @MLModel@ during the creation operation.
--
-- @CreateMLModel@ requires a @DataSource@ with computed statistics, which
-- can be created by setting @ComputeStatistics@ to @true@ in
-- @CreateDataSourceFromRDS@, @CreateDataSourceFromS3@, or
-- @CreateDataSourceFromRedshift@ operations.
module Amazonka.MachineLearning.CreateMLModel
  ( -- * Creating a Request
    CreateMLModel (..),
    newCreateMLModel,

    -- * Request Lenses
    createMLModel_mLModelName,
    createMLModel_parameters,
    createMLModel_recipe,
    createMLModel_recipeUri,
    createMLModel_mLModelId,
    createMLModel_mLModelType,
    createMLModel_trainingDataSourceId,

    -- * Destructuring the Response
    CreateMLModelResponse (..),
    newCreateMLModelResponse,

    -- * Response Lenses
    createMLModelResponse_mLModelId,
    createMLModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMLModel' smart constructor.
data CreateMLModel = CreateMLModel'
  { -- | A user-supplied name or description of the @MLModel@.
    mLModelName :: Prelude.Maybe Prelude.Text,
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
    --     Shuffling the data improves a model\'s ability to find the optimal
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
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The data recipe for creating the @MLModel@. You must specify either the
    -- recipe or its URI. If you don\'t specify a recipe or its URI, Amazon ML
    -- creates a default.
    recipe :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Storage Service (Amazon S3) location and file name
    -- that contains the @MLModel@ recipe. You must specify either the recipe
    -- or its URI. If you don\'t specify a recipe or its URI, Amazon ML creates
    -- a default.
    recipeUri :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied ID that uniquely identifies the @MLModel@.
    mLModelId :: Prelude.Text,
    -- | The category of supervised learning that this @MLModel@ will address.
    -- Choose from the following types:
    --
    -- -   Choose @REGRESSION@ if the @MLModel@ will be used to predict a
    --     numeric value.
    --
    -- -   Choose @BINARY@ if the @MLModel@ result has two possible values.
    --
    -- -   Choose @MULTICLASS@ if the @MLModel@ result has a limited number of
    --     values.
    --
    -- For more information, see the
    -- <https://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
    mLModelType :: MLModelType,
    -- | The @DataSource@ that points to the training data.
    trainingDataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMLModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelName', 'createMLModel_mLModelName' - A user-supplied name or description of the @MLModel@.
--
-- 'parameters', 'createMLModel_parameters' - A list of the training parameters in the @MLModel@. The list is
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
--     Shuffling the data improves a model\'s ability to find the optimal
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
-- 'recipe', 'createMLModel_recipe' - The data recipe for creating the @MLModel@. You must specify either the
-- recipe or its URI. If you don\'t specify a recipe or its URI, Amazon ML
-- creates a default.
--
-- 'recipeUri', 'createMLModel_recipeUri' - The Amazon Simple Storage Service (Amazon S3) location and file name
-- that contains the @MLModel@ recipe. You must specify either the recipe
-- or its URI. If you don\'t specify a recipe or its URI, Amazon ML creates
-- a default.
--
-- 'mLModelId', 'createMLModel_mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@.
--
-- 'mLModelType', 'createMLModel_mLModelType' - The category of supervised learning that this @MLModel@ will address.
-- Choose from the following types:
--
-- -   Choose @REGRESSION@ if the @MLModel@ will be used to predict a
--     numeric value.
--
-- -   Choose @BINARY@ if the @MLModel@ result has two possible values.
--
-- -   Choose @MULTICLASS@ if the @MLModel@ result has a limited number of
--     values.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
--
-- 'trainingDataSourceId', 'createMLModel_trainingDataSourceId' - The @DataSource@ that points to the training data.
newCreateMLModel ::
  -- | 'mLModelId'
  Prelude.Text ->
  -- | 'mLModelType'
  MLModelType ->
  -- | 'trainingDataSourceId'
  Prelude.Text ->
  CreateMLModel
newCreateMLModel
  pMLModelId_
  pMLModelType_
  pTrainingDataSourceId_ =
    CreateMLModel'
      { mLModelName = Prelude.Nothing,
        parameters = Prelude.Nothing,
        recipe = Prelude.Nothing,
        recipeUri = Prelude.Nothing,
        mLModelId = pMLModelId_,
        mLModelType = pMLModelType_,
        trainingDataSourceId = pTrainingDataSourceId_
      }

-- | A user-supplied name or description of the @MLModel@.
createMLModel_mLModelName :: Lens.Lens' CreateMLModel (Prelude.Maybe Prelude.Text)
createMLModel_mLModelName = Lens.lens (\CreateMLModel' {mLModelName} -> mLModelName) (\s@CreateMLModel' {} a -> s {mLModelName = a} :: CreateMLModel)

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
--     Shuffling the data improves a model\'s ability to find the optimal
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
createMLModel_parameters :: Lens.Lens' CreateMLModel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMLModel_parameters = Lens.lens (\CreateMLModel' {parameters} -> parameters) (\s@CreateMLModel' {} a -> s {parameters = a} :: CreateMLModel) Prelude.. Lens.mapping Lens.coerced

-- | The data recipe for creating the @MLModel@. You must specify either the
-- recipe or its URI. If you don\'t specify a recipe or its URI, Amazon ML
-- creates a default.
createMLModel_recipe :: Lens.Lens' CreateMLModel (Prelude.Maybe Prelude.Text)
createMLModel_recipe = Lens.lens (\CreateMLModel' {recipe} -> recipe) (\s@CreateMLModel' {} a -> s {recipe = a} :: CreateMLModel)

-- | The Amazon Simple Storage Service (Amazon S3) location and file name
-- that contains the @MLModel@ recipe. You must specify either the recipe
-- or its URI. If you don\'t specify a recipe or its URI, Amazon ML creates
-- a default.
createMLModel_recipeUri :: Lens.Lens' CreateMLModel (Prelude.Maybe Prelude.Text)
createMLModel_recipeUri = Lens.lens (\CreateMLModel' {recipeUri} -> recipeUri) (\s@CreateMLModel' {} a -> s {recipeUri = a} :: CreateMLModel)

-- | A user-supplied ID that uniquely identifies the @MLModel@.
createMLModel_mLModelId :: Lens.Lens' CreateMLModel Prelude.Text
createMLModel_mLModelId = Lens.lens (\CreateMLModel' {mLModelId} -> mLModelId) (\s@CreateMLModel' {} a -> s {mLModelId = a} :: CreateMLModel)

-- | The category of supervised learning that this @MLModel@ will address.
-- Choose from the following types:
--
-- -   Choose @REGRESSION@ if the @MLModel@ will be used to predict a
--     numeric value.
--
-- -   Choose @BINARY@ if the @MLModel@ result has two possible values.
--
-- -   Choose @MULTICLASS@ if the @MLModel@ result has a limited number of
--     values.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
createMLModel_mLModelType :: Lens.Lens' CreateMLModel MLModelType
createMLModel_mLModelType = Lens.lens (\CreateMLModel' {mLModelType} -> mLModelType) (\s@CreateMLModel' {} a -> s {mLModelType = a} :: CreateMLModel)

-- | The @DataSource@ that points to the training data.
createMLModel_trainingDataSourceId :: Lens.Lens' CreateMLModel Prelude.Text
createMLModel_trainingDataSourceId = Lens.lens (\CreateMLModel' {trainingDataSourceId} -> trainingDataSourceId) (\s@CreateMLModel' {} a -> s {trainingDataSourceId = a} :: CreateMLModel)

instance Core.AWSRequest CreateMLModel where
  type
    AWSResponse CreateMLModel =
      CreateMLModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMLModelResponse'
            Prelude.<$> (x Data..?> "MLModelId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMLModel where
  hashWithSalt _salt CreateMLModel' {..} =
    _salt
      `Prelude.hashWithSalt` mLModelName
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` recipe
      `Prelude.hashWithSalt` recipeUri
      `Prelude.hashWithSalt` mLModelId
      `Prelude.hashWithSalt` mLModelType
      `Prelude.hashWithSalt` trainingDataSourceId

instance Prelude.NFData CreateMLModel where
  rnf CreateMLModel' {..} =
    Prelude.rnf mLModelName
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf recipe
      `Prelude.seq` Prelude.rnf recipeUri
      `Prelude.seq` Prelude.rnf mLModelId
      `Prelude.seq` Prelude.rnf mLModelType
      `Prelude.seq` Prelude.rnf trainingDataSourceId

instance Data.ToHeaders CreateMLModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.CreateMLModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMLModel where
  toJSON CreateMLModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MLModelName" Data..=) Prelude.<$> mLModelName,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("Recipe" Data..=) Prelude.<$> recipe,
            ("RecipeUri" Data..=) Prelude.<$> recipeUri,
            Prelude.Just ("MLModelId" Data..= mLModelId),
            Prelude.Just ("MLModelType" Data..= mLModelType),
            Prelude.Just
              ( "TrainingDataSourceId"
                  Data..= trainingDataSourceId
              )
          ]
      )

instance Data.ToPath CreateMLModel where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateMLModel where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateMLModel@ operation, and is an
-- acknowledgement that Amazon ML received the request.
--
-- The @CreateMLModel@ operation is asynchronous. You can poll for status
-- updates by using the @GetMLModel@ operation and checking the @Status@
-- parameter.
--
-- /See:/ 'newCreateMLModelResponse' smart constructor.
data CreateMLModelResponse = CreateMLModelResponse'
  { -- | A user-supplied ID that uniquely identifies the @MLModel@. This value
    -- should be identical to the value of the @MLModelId@ in the request.
    mLModelId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMLModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mLModelId', 'createMLModelResponse_mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
--
-- 'httpStatus', 'createMLModelResponse_httpStatus' - The response's http status code.
newCreateMLModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMLModelResponse
newCreateMLModelResponse pHttpStatus_ =
  CreateMLModelResponse'
    { mLModelId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
createMLModelResponse_mLModelId :: Lens.Lens' CreateMLModelResponse (Prelude.Maybe Prelude.Text)
createMLModelResponse_mLModelId = Lens.lens (\CreateMLModelResponse' {mLModelId} -> mLModelId) (\s@CreateMLModelResponse' {} a -> s {mLModelId = a} :: CreateMLModelResponse)

-- | The response's http status code.
createMLModelResponse_httpStatus :: Lens.Lens' CreateMLModelResponse Prelude.Int
createMLModelResponse_httpStatus = Lens.lens (\CreateMLModelResponse' {httpStatus} -> httpStatus) (\s@CreateMLModelResponse' {} a -> s {httpStatus = a} :: CreateMLModelResponse)

instance Prelude.NFData CreateMLModelResponse where
  rnf CreateMLModelResponse' {..} =
    Prelude.rnf mLModelId
      `Prelude.seq` Prelude.rnf httpStatus
