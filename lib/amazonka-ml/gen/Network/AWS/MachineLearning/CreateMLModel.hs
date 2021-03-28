{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateMLModel (..)
    , mkCreateMLModel
    -- ** Request lenses
    , cmlmMLModelId
    , cmlmMLModelType
    , cmlmTrainingDataSourceId
    , cmlmMLModelName
    , cmlmParameters
    , cmlmRecipe
    , cmlmRecipeUri

    -- * Destructuring the response
    , CreateMLModelResponse (..)
    , mkCreateMLModelResponse
    -- ** Response lenses
    , cmlmrrsMLModelId
    , cmlmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateMLModel' smart constructor.
data CreateMLModel = CreateMLModel'
  { mLModelId :: Types.MLModelId
    -- ^ A user-supplied ID that uniquely identifies the @MLModel@ .
  , mLModelType :: Types.MLModelType
    -- ^ The category of supervised learning that this @MLModel@ will address. Choose from the following types:
--
--
--     * Choose @REGRESSION@ if the @MLModel@ will be used to predict a numeric value.
--
--     * Choose @BINARY@ if the @MLModel@ result has two possible values.
--
--     * Choose @MULTICLASS@ if the @MLModel@ result has a limited number of values. 
--
-- For more information, see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
  , trainingDataSourceId :: Types.TrainingDataSourceId
    -- ^ The @DataSource@ that points to the training data.
  , mLModelName :: Core.Maybe Types.EntityName
    -- ^ A user-supplied name or description of the @MLModel@ .
  , parameters :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs.
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
  , recipe :: Core.Maybe Types.Recipe
    -- ^ The data recipe for creating the @MLModel@ . You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
  , recipeUri :: Core.Maybe Types.RecipeUri
    -- ^ The Amazon Simple Storage Service (Amazon S3) location and file name that contains the @MLModel@ recipe. You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMLModel' value with any optional fields omitted.
mkCreateMLModel
    :: Types.MLModelId -- ^ 'mLModelId'
    -> Types.MLModelType -- ^ 'mLModelType'
    -> Types.TrainingDataSourceId -- ^ 'trainingDataSourceId'
    -> CreateMLModel
mkCreateMLModel mLModelId mLModelType trainingDataSourceId
  = CreateMLModel'{mLModelId, mLModelType, trainingDataSourceId,
                   mLModelName = Core.Nothing, parameters = Core.Nothing,
                   recipe = Core.Nothing, recipeUri = Core.Nothing}

-- | A user-supplied ID that uniquely identifies the @MLModel@ .
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmMLModelId :: Lens.Lens' CreateMLModel Types.MLModelId
cmlmMLModelId = Lens.field @"mLModelId"
{-# INLINEABLE cmlmMLModelId #-}
{-# DEPRECATED mLModelId "Use generic-lens or generic-optics with 'mLModelId' instead"  #-}

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
cmlmMLModelType :: Lens.Lens' CreateMLModel Types.MLModelType
cmlmMLModelType = Lens.field @"mLModelType"
{-# INLINEABLE cmlmMLModelType #-}
{-# DEPRECATED mLModelType "Use generic-lens or generic-optics with 'mLModelType' instead"  #-}

-- | The @DataSource@ that points to the training data.
--
-- /Note:/ Consider using 'trainingDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmTrainingDataSourceId :: Lens.Lens' CreateMLModel Types.TrainingDataSourceId
cmlmTrainingDataSourceId = Lens.field @"trainingDataSourceId"
{-# INLINEABLE cmlmTrainingDataSourceId #-}
{-# DEPRECATED trainingDataSourceId "Use generic-lens or generic-optics with 'trainingDataSourceId' instead"  #-}

-- | A user-supplied name or description of the @MLModel@ .
--
-- /Note:/ Consider using 'mLModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmMLModelName :: Lens.Lens' CreateMLModel (Core.Maybe Types.EntityName)
cmlmMLModelName = Lens.field @"mLModelName"
{-# INLINEABLE cmlmMLModelName #-}
{-# DEPRECATED mLModelName "Use generic-lens or generic-optics with 'mLModelName' instead"  #-}

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
cmlmParameters :: Lens.Lens' CreateMLModel (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
cmlmParameters = Lens.field @"parameters"
{-# INLINEABLE cmlmParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The data recipe for creating the @MLModel@ . You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
--
-- /Note:/ Consider using 'recipe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmRecipe :: Lens.Lens' CreateMLModel (Core.Maybe Types.Recipe)
cmlmRecipe = Lens.field @"recipe"
{-# INLINEABLE cmlmRecipe #-}
{-# DEPRECATED recipe "Use generic-lens or generic-optics with 'recipe' instead"  #-}

-- | The Amazon Simple Storage Service (Amazon S3) location and file name that contains the @MLModel@ recipe. You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
--
-- /Note:/ Consider using 'recipeUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmRecipeUri :: Lens.Lens' CreateMLModel (Core.Maybe Types.RecipeUri)
cmlmRecipeUri = Lens.field @"recipeUri"
{-# INLINEABLE cmlmRecipeUri #-}
{-# DEPRECATED recipeUri "Use generic-lens or generic-optics with 'recipeUri' instead"  #-}

instance Core.ToQuery CreateMLModel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateMLModel where
        toHeaders CreateMLModel{..}
          = Core.pure ("X-Amz-Target", "AmazonML_20141212.CreateMLModel")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateMLModel where
        toJSON CreateMLModel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MLModelId" Core..= mLModelId),
                  Core.Just ("MLModelType" Core..= mLModelType),
                  Core.Just ("TrainingDataSourceId" Core..= trainingDataSourceId),
                  ("MLModelName" Core..=) Core.<$> mLModelName,
                  ("Parameters" Core..=) Core.<$> parameters,
                  ("Recipe" Core..=) Core.<$> recipe,
                  ("RecipeUri" Core..=) Core.<$> recipeUri])

instance Core.AWSRequest CreateMLModel where
        type Rs CreateMLModel = CreateMLModelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateMLModelResponse' Core.<$>
                   (x Core..:? "MLModelId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @CreateMLModel@ operation, and is an acknowledgement that Amazon ML received the request.
--
-- The @CreateMLModel@ operation is asynchronous. You can poll for status updates by using the @GetMLModel@ operation and checking the @Status@ parameter. 
--
-- /See:/ 'mkCreateMLModelResponse' smart constructor.
data CreateMLModelResponse = CreateMLModelResponse'
  { mLModelId :: Core.Maybe Types.MLModelId
    -- ^ A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMLModelResponse' value with any optional fields omitted.
mkCreateMLModelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateMLModelResponse
mkCreateMLModelResponse responseStatus
  = CreateMLModelResponse'{mLModelId = Core.Nothing, responseStatus}

-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request. 
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmrrsMLModelId :: Lens.Lens' CreateMLModelResponse (Core.Maybe Types.MLModelId)
cmlmrrsMLModelId = Lens.field @"mLModelId"
{-# INLINEABLE cmlmrrsMLModelId #-}
{-# DEPRECATED mLModelId "Use generic-lens or generic-optics with 'mLModelId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmlmrrsResponseStatus :: Lens.Lens' CreateMLModelResponse Core.Int
cmlmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmlmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
