{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateMLModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new @MLModel@ using the @DataSource@ and the recipe as information sources.
--
-- An @MLModel@ is nearly immutable. Users can update only the @MLModelName@ and the @ScoreThreshold@ in an @MLModel@ without creating a new @MLModel@ .
-- @CreateMLModel@ is an asynchronous operation. In response to @CreateMLModel@ , Amazon Machine Learning (Amazon ML) immediately returns and sets the @MLModel@ status to @PENDING@ . After the @MLModel@ has been created and ready is for use, Amazon ML sets the status to @COMPLETED@ .
-- You can use the @GetMLModel@ operation to check the progress of the @MLModel@ during the creation operation.
-- @CreateMLModel@ requires a @DataSource@ with computed statistics, which can be created by setting @ComputeStatistics@ to @true@ in @CreateDataSourceFromRDS@ , @CreateDataSourceFromS3@ , or @CreateDataSourceFromRedshift@ operations.
module Network.AWS.MachineLearning.CreateMLModel
  ( -- * Creating a request
    CreateMLModel (..),
    mkCreateMLModel,

    -- ** Request lenses
    cmlmRecipe,
    cmlmRecipeURI,
    cmlmMLModelName,
    cmlmParameters,
    cmlmMLModelId,
    cmlmMLModelType,
    cmlmTrainingDataSourceId,

    -- * Destructuring the response
    CreateMLModelResponse (..),
    mkCreateMLModelResponse,

    -- ** Response lenses
    cmlmrsMLModelId,
    cmlmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateMLModel' smart constructor.
data CreateMLModel = CreateMLModel'
  { recipe :: Lude.Maybe Lude.Text,
    recipeURI :: Lude.Maybe Lude.Text,
    mLModelName :: Lude.Maybe Lude.Text,
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    mLModelId :: Lude.Text,
    mLModelType :: MLModelType,
    trainingDataSourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMLModel' with the minimum fields required to make a request.
--
-- * 'mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ .
-- * 'mLModelName' - A user-supplied name or description of the @MLModel@ .
-- * 'mLModelType' - The category of supervised learning that this @MLModel@ will address. Choose from the following types:
--
--
--     * Choose @REGRESSION@ if the @MLModel@ will be used to predict a numeric value.
--
--     * Choose @BINARY@ if the @MLModel@ result has two possible values.
--
--     * Choose @MULTICLASS@ if the @MLModel@ result has a limited number of values.
--
-- For more information, see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
-- * 'parameters' - A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs.
--
-- The following is the current set of training parameters:
--
--     * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance.
-- The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .
--
--
--     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .
--
--
--     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling the data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.
--
--
--     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.
--
--
--     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
--
--
-- * 'recipe' - The data recipe for creating the @MLModel@ . You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
-- * 'recipeURI' - The Amazon Simple Storage Service (Amazon S3) location and file name that contains the @MLModel@ recipe. You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
-- * 'trainingDataSourceId' - The @DataSource@ that points to the training data.
mkCreateMLModel ::
  -- | 'mLModelId'
  Lude.Text ->
  -- | 'mLModelType'
  MLModelType ->
  -- | 'trainingDataSourceId'
  Lude.Text ->
  CreateMLModel
mkCreateMLModel pMLModelId_ pMLModelType_ pTrainingDataSourceId_ =
  CreateMLModel'
    { recipe = Lude.Nothing,
      recipeURI = Lude.Nothing,
      mLModelName = Lude.Nothing,
      parameters = Lude.Nothing,
      mLModelId = pMLModelId_,
      mLModelType = pMLModelType_,
      trainingDataSourceId = pTrainingDataSourceId_
    }

-- | The data recipe for creating the @MLModel@ . You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
--
-- /Note:/ Consider using 'recipe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmRecipe :: Lens.Lens' CreateMLModel (Lude.Maybe Lude.Text)
cmlmRecipe = Lens.lens (recipe :: CreateMLModel -> Lude.Maybe Lude.Text) (\s a -> s {recipe = a} :: CreateMLModel)
{-# DEPRECATED cmlmRecipe "Use generic-lens or generic-optics with 'recipe' instead." #-}

-- | The Amazon Simple Storage Service (Amazon S3) location and file name that contains the @MLModel@ recipe. You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
--
-- /Note:/ Consider using 'recipeURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmRecipeURI :: Lens.Lens' CreateMLModel (Lude.Maybe Lude.Text)
cmlmRecipeURI = Lens.lens (recipeURI :: CreateMLModel -> Lude.Maybe Lude.Text) (\s a -> s {recipeURI = a} :: CreateMLModel)
{-# DEPRECATED cmlmRecipeURI "Use generic-lens or generic-optics with 'recipeURI' instead." #-}

-- | A user-supplied name or description of the @MLModel@ .
--
-- /Note:/ Consider using 'mLModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmMLModelName :: Lens.Lens' CreateMLModel (Lude.Maybe Lude.Text)
cmlmMLModelName = Lens.lens (mLModelName :: CreateMLModel -> Lude.Maybe Lude.Text) (\s a -> s {mLModelName = a} :: CreateMLModel)
{-# DEPRECATED cmlmMLModelName "Use generic-lens or generic-optics with 'mLModelName' instead." #-}

-- | A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs.
--
-- The following is the current set of training parameters:
--
--     * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance.
-- The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .
--
--
--     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .
--
--
--     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling the data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.
--
--
--     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.
--
--
--     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
--
--
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmParameters :: Lens.Lens' CreateMLModel (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cmlmParameters = Lens.lens (parameters :: CreateMLModel -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: CreateMLModel)
{-# DEPRECATED cmlmParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A user-supplied ID that uniquely identifies the @MLModel@ .
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmMLModelId :: Lens.Lens' CreateMLModel Lude.Text
cmlmMLModelId = Lens.lens (mLModelId :: CreateMLModel -> Lude.Text) (\s a -> s {mLModelId = a} :: CreateMLModel)
{-# DEPRECATED cmlmMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The category of supervised learning that this @MLModel@ will address. Choose from the following types:
--
--
--     * Choose @REGRESSION@ if the @MLModel@ will be used to predict a numeric value.
--
--     * Choose @BINARY@ if the @MLModel@ result has two possible values.
--
--     * Choose @MULTICLASS@ if the @MLModel@ result has a limited number of values.
--
-- For more information, see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
--
-- /Note:/ Consider using 'mLModelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmMLModelType :: Lens.Lens' CreateMLModel MLModelType
cmlmMLModelType = Lens.lens (mLModelType :: CreateMLModel -> MLModelType) (\s a -> s {mLModelType = a} :: CreateMLModel)
{-# DEPRECATED cmlmMLModelType "Use generic-lens or generic-optics with 'mLModelType' instead." #-}

-- | The @DataSource@ that points to the training data.
--
-- /Note:/ Consider using 'trainingDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmTrainingDataSourceId :: Lens.Lens' CreateMLModel Lude.Text
cmlmTrainingDataSourceId = Lens.lens (trainingDataSourceId :: CreateMLModel -> Lude.Text) (\s a -> s {trainingDataSourceId = a} :: CreateMLModel)
{-# DEPRECATED cmlmTrainingDataSourceId "Use generic-lens or generic-optics with 'trainingDataSourceId' instead." #-}

instance Lude.AWSRequest CreateMLModel where
  type Rs CreateMLModel = CreateMLModelResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMLModelResponse'
            Lude.<$> (x Lude..?> "MLModelId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateMLModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.CreateMLModel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMLModel where
  toJSON CreateMLModel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Recipe" Lude..=) Lude.<$> recipe,
            ("RecipeUri" Lude..=) Lude.<$> recipeURI,
            ("MLModelName" Lude..=) Lude.<$> mLModelName,
            ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("MLModelId" Lude..= mLModelId),
            Lude.Just ("MLModelType" Lude..= mLModelType),
            Lude.Just ("TrainingDataSourceId" Lude..= trainingDataSourceId)
          ]
      )

instance Lude.ToPath CreateMLModel where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateMLModel where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateMLModel@ operation, and is an acknowledgement that Amazon ML received the request.
--
-- The @CreateMLModel@ operation is asynchronous. You can poll for status updates by using the @GetMLModel@ operation and checking the @Status@ parameter.
--
-- /See:/ 'mkCreateMLModelResponse' smart constructor.
data CreateMLModelResponse = CreateMLModelResponse'
  { mLModelId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMLModelResponse' with the minimum fields required to make a request.
--
-- * 'mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
-- * 'responseStatus' - The response status code.
mkCreateMLModelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMLModelResponse
mkCreateMLModelResponse pResponseStatus_ =
  CreateMLModelResponse'
    { mLModelId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmrsMLModelId :: Lens.Lens' CreateMLModelResponse (Lude.Maybe Lude.Text)
cmlmrsMLModelId = Lens.lens (mLModelId :: CreateMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: CreateMLModelResponse)
{-# DEPRECATED cmlmrsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmrsResponseStatus :: Lens.Lens' CreateMLModelResponse Lude.Int
cmlmrsResponseStatus = Lens.lens (responseStatus :: CreateMLModelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMLModelResponse)
{-# DEPRECATED cmlmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
