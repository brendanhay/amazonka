{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateMLModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new @MLModel@ using the @DataSource@ and the recipe as information sources.
--
--
-- An @MLModel@ is nearly immutable. Users can update only the @MLModelName@ and the @ScoreThreshold@ in an @MLModel@ without creating a new @MLModel@ .
--
-- @CreateMLModel@ is an asynchronous operation. In response to @CreateMLModel@ , Amazon Machine Learning (Amazon ML) immediately returns and sets the @MLModel@ status to @PENDING@ . After the @MLModel@ has been created and ready is for use, Amazon ML sets the status to @COMPLETED@ .
--
-- You can use the @GetMLModel@ operation to check the progress of the @MLModel@ during the creation operation.
--
-- @CreateMLModel@ requires a @DataSource@ with computed statistics, which can be created by setting @ComputeStatistics@ to @true@ in @CreateDataSourceFromRDS@ , @CreateDataSourceFromS3@ , or @CreateDataSourceFromRedshift@ operations.
--
module Network.AWS.MachineLearning.CreateMLModel
    (
    -- * Creating a Request
      createMLModel
    , CreateMLModel
    -- * Request Lenses
    , cmlmRecipe
    , cmlmRecipeURI
    , cmlmMLModelName
    , cmlmParameters
    , cmlmMLModelId
    , cmlmMLModelType
    , cmlmTrainingDataSourceId

    -- * Destructuring the Response
    , createMLModelResponse
    , CreateMLModelResponse
    -- * Response Lenses
    , cmlmrsMLModelId
    , cmlmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createMLModel' smart constructor.
data CreateMLModel = CreateMLModel'
  { _cmlmRecipe               :: !(Maybe Text)
  , _cmlmRecipeURI            :: !(Maybe Text)
  , _cmlmMLModelName          :: !(Maybe Text)
  , _cmlmParameters           :: !(Maybe (Map Text Text))
  , _cmlmMLModelId            :: !Text
  , _cmlmMLModelType          :: !MLModelType
  , _cmlmTrainingDataSourceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMLModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmlmRecipe' - The data recipe for creating the @MLModel@ . You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
--
-- * 'cmlmRecipeURI' - The Amazon Simple Storage Service (Amazon S3) location and file name that contains the @MLModel@ recipe. You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
--
-- * 'cmlmMLModelName' - A user-supplied name or description of the @MLModel@ .
--
-- * 'cmlmParameters' - A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs. The following is the current set of training parameters:      * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance. The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling the data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
--
-- * 'cmlmMLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ .
--
-- * 'cmlmMLModelType' - The category of supervised learning that this @MLModel@ will address. Choose from the following types:     * Choose @REGRESSION@ if the @MLModel@ will be used to predict a numeric value.    * Choose @BINARY@ if the @MLModel@ result has two possible values.    * Choose @MULTICLASS@ if the @MLModel@ result has a limited number of values.  For more information, see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
--
-- * 'cmlmTrainingDataSourceId' - The @DataSource@ that points to the training data.
createMLModel
    :: Text -- ^ 'cmlmMLModelId'
    -> MLModelType -- ^ 'cmlmMLModelType'
    -> Text -- ^ 'cmlmTrainingDataSourceId'
    -> CreateMLModel
createMLModel pMLModelId_ pMLModelType_ pTrainingDataSourceId_ =
  CreateMLModel'
    { _cmlmRecipe = Nothing
    , _cmlmRecipeURI = Nothing
    , _cmlmMLModelName = Nothing
    , _cmlmParameters = Nothing
    , _cmlmMLModelId = pMLModelId_
    , _cmlmMLModelType = pMLModelType_
    , _cmlmTrainingDataSourceId = pTrainingDataSourceId_
    }


-- | The data recipe for creating the @MLModel@ . You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
cmlmRecipe :: Lens' CreateMLModel (Maybe Text)
cmlmRecipe = lens _cmlmRecipe (\ s a -> s{_cmlmRecipe = a})

-- | The Amazon Simple Storage Service (Amazon S3) location and file name that contains the @MLModel@ recipe. You must specify either the recipe or its URI. If you don't specify a recipe or its URI, Amazon ML creates a default.
cmlmRecipeURI :: Lens' CreateMLModel (Maybe Text)
cmlmRecipeURI = lens _cmlmRecipeURI (\ s a -> s{_cmlmRecipeURI = a})

-- | A user-supplied name or description of the @MLModel@ .
cmlmMLModelName :: Lens' CreateMLModel (Maybe Text)
cmlmMLModelName = lens _cmlmMLModelName (\ s a -> s{_cmlmMLModelName = a})

-- | A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs. The following is the current set of training parameters:      * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance. The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling the data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
cmlmParameters :: Lens' CreateMLModel (HashMap Text Text)
cmlmParameters = lens _cmlmParameters (\ s a -> s{_cmlmParameters = a}) . _Default . _Map

-- | A user-supplied ID that uniquely identifies the @MLModel@ .
cmlmMLModelId :: Lens' CreateMLModel Text
cmlmMLModelId = lens _cmlmMLModelId (\ s a -> s{_cmlmMLModelId = a})

-- | The category of supervised learning that this @MLModel@ will address. Choose from the following types:     * Choose @REGRESSION@ if the @MLModel@ will be used to predict a numeric value.    * Choose @BINARY@ if the @MLModel@ result has two possible values.    * Choose @MULTICLASS@ if the @MLModel@ result has a limited number of values.  For more information, see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
cmlmMLModelType :: Lens' CreateMLModel MLModelType
cmlmMLModelType = lens _cmlmMLModelType (\ s a -> s{_cmlmMLModelType = a})

-- | The @DataSource@ that points to the training data.
cmlmTrainingDataSourceId :: Lens' CreateMLModel Text
cmlmTrainingDataSourceId = lens _cmlmTrainingDataSourceId (\ s a -> s{_cmlmTrainingDataSourceId = a})

instance AWSRequest CreateMLModel where
        type Rs CreateMLModel = CreateMLModelResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 CreateMLModelResponse' <$>
                   (x .?> "MLModelId") <*> (pure (fromEnum s)))

instance Hashable CreateMLModel where

instance NFData CreateMLModel where

instance ToHeaders CreateMLModel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.CreateMLModel" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateMLModel where
        toJSON CreateMLModel'{..}
          = object
              (catMaybes
                 [("Recipe" .=) <$> _cmlmRecipe,
                  ("RecipeUri" .=) <$> _cmlmRecipeURI,
                  ("MLModelName" .=) <$> _cmlmMLModelName,
                  ("Parameters" .=) <$> _cmlmParameters,
                  Just ("MLModelId" .= _cmlmMLModelId),
                  Just ("MLModelType" .= _cmlmMLModelType),
                  Just
                    ("TrainingDataSourceId" .=
                       _cmlmTrainingDataSourceId)])

instance ToPath CreateMLModel where
        toPath = const "/"

instance ToQuery CreateMLModel where
        toQuery = const mempty

-- | Represents the output of a @CreateMLModel@ operation, and is an acknowledgement that Amazon ML received the request.
--
--
-- The @CreateMLModel@ operation is asynchronous. You can poll for status updates by using the @GetMLModel@ operation and checking the @Status@ parameter.
--
--
-- /See:/ 'createMLModelResponse' smart constructor.
data CreateMLModelResponse = CreateMLModelResponse'
  { _cmlmrsMLModelId      :: !(Maybe Text)
  , _cmlmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMLModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmlmrsMLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
--
-- * 'cmlmrsResponseStatus' - -- | The response status code.
createMLModelResponse
    :: Int -- ^ 'cmlmrsResponseStatus'
    -> CreateMLModelResponse
createMLModelResponse pResponseStatus_ =
  CreateMLModelResponse'
    {_cmlmrsMLModelId = Nothing, _cmlmrsResponseStatus = pResponseStatus_}


-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
cmlmrsMLModelId :: Lens' CreateMLModelResponse (Maybe Text)
cmlmrsMLModelId = lens _cmlmrsMLModelId (\ s a -> s{_cmlmrsMLModelId = a})

-- | -- | The response status code.
cmlmrsResponseStatus :: Lens' CreateMLModelResponse Int
cmlmrsResponseStatus = lens _cmlmrsResponseStatus (\ s a -> s{_cmlmrsResponseStatus = a})

instance NFData CreateMLModelResponse where
