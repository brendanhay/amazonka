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
-- Module      : Network.AWS.MachineLearning.GetMLModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an @MLModel@ that includes detailed metadata, data source information, and the current status of the @MLModel@ .
--
--
-- @GetMLModel@ provides results in normal or verbose format.
--
module Network.AWS.MachineLearning.GetMLModel
    (
    -- * Creating a Request
      getMLModel
    , GetMLModel
    -- * Request Lenses
    , gmlmVerbose
    , gmlmMLModelId

    -- * Destructuring the Response
    , getMLModelResponse
    , GetMLModelResponse
    -- * Response Lenses
    , gmlmrsStatus
    , gmlmrsLastUpdatedAt
    , gmlmrsTrainingParameters
    , gmlmrsScoreThresholdLastUpdatedAt
    , gmlmrsCreatedAt
    , gmlmrsComputeTime
    , gmlmrsRecipe
    , gmlmrsInputDataLocationS3
    , gmlmrsMLModelId
    , gmlmrsSizeInBytes
    , gmlmrsSchema
    , gmlmrsStartedAt
    , gmlmrsScoreThreshold
    , gmlmrsFinishedAt
    , gmlmrsCreatedByIAMUser
    , gmlmrsName
    , gmlmrsLogURI
    , gmlmrsEndpointInfo
    , gmlmrsTrainingDataSourceId
    , gmlmrsMessage
    , gmlmrsMLModelType
    , gmlmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMLModel' smart constructor.
data GetMLModel = GetMLModel'
  { _gmlmVerbose   :: !(Maybe Bool)
  , _gmlmMLModelId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMLModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmlmVerbose' - Specifies whether the @GetMLModel@ operation should return @Recipe@ . If true, @Recipe@ is returned. If false, @Recipe@ is not returned.
--
-- * 'gmlmMLModelId' - The ID assigned to the @MLModel@ at creation.
getMLModel
    :: Text -- ^ 'gmlmMLModelId'
    -> GetMLModel
getMLModel pMLModelId_ =
  GetMLModel' {_gmlmVerbose = Nothing, _gmlmMLModelId = pMLModelId_}


-- | Specifies whether the @GetMLModel@ operation should return @Recipe@ . If true, @Recipe@ is returned. If false, @Recipe@ is not returned.
gmlmVerbose :: Lens' GetMLModel (Maybe Bool)
gmlmVerbose = lens _gmlmVerbose (\ s a -> s{_gmlmVerbose = a})

-- | The ID assigned to the @MLModel@ at creation.
gmlmMLModelId :: Lens' GetMLModel Text
gmlmMLModelId = lens _gmlmMLModelId (\ s a -> s{_gmlmMLModelId = a})

instance AWSRequest GetMLModel where
        type Rs GetMLModel = GetMLModelResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 GetMLModelResponse' <$>
                   (x .?> "Status") <*> (x .?> "LastUpdatedAt") <*>
                     (x .?> "TrainingParameters" .!@ mempty)
                     <*> (x .?> "ScoreThresholdLastUpdatedAt")
                     <*> (x .?> "CreatedAt")
                     <*> (x .?> "ComputeTime")
                     <*> (x .?> "Recipe")
                     <*> (x .?> "InputDataLocationS3")
                     <*> (x .?> "MLModelId")
                     <*> (x .?> "SizeInBytes")
                     <*> (x .?> "Schema")
                     <*> (x .?> "StartedAt")
                     <*> (x .?> "ScoreThreshold")
                     <*> (x .?> "FinishedAt")
                     <*> (x .?> "CreatedByIamUser")
                     <*> (x .?> "Name")
                     <*> (x .?> "LogUri")
                     <*> (x .?> "EndpointInfo")
                     <*> (x .?> "TrainingDataSourceId")
                     <*> (x .?> "Message")
                     <*> (x .?> "MLModelType")
                     <*> (pure (fromEnum s)))

instance Hashable GetMLModel where

instance NFData GetMLModel where

instance ToHeaders GetMLModel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.GetMLModel" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetMLModel where
        toJSON GetMLModel'{..}
          = object
              (catMaybes
                 [("Verbose" .=) <$> _gmlmVerbose,
                  Just ("MLModelId" .= _gmlmMLModelId)])

instance ToPath GetMLModel where
        toPath = const "/"

instance ToQuery GetMLModel where
        toQuery = const mempty

-- | Represents the output of a @GetMLModel@ operation, and provides detailed information about a @MLModel@ .
--
--
--
-- /See:/ 'getMLModelResponse' smart constructor.
data GetMLModelResponse = GetMLModelResponse'
  { _gmlmrsStatus                      :: !(Maybe EntityStatus)
  , _gmlmrsLastUpdatedAt               :: !(Maybe POSIX)
  , _gmlmrsTrainingParameters          :: !(Maybe (Map Text Text))
  , _gmlmrsScoreThresholdLastUpdatedAt :: !(Maybe POSIX)
  , _gmlmrsCreatedAt                   :: !(Maybe POSIX)
  , _gmlmrsComputeTime                 :: !(Maybe Integer)
  , _gmlmrsRecipe                      :: !(Maybe Text)
  , _gmlmrsInputDataLocationS3         :: !(Maybe Text)
  , _gmlmrsMLModelId                   :: !(Maybe Text)
  , _gmlmrsSizeInBytes                 :: !(Maybe Integer)
  , _gmlmrsSchema                      :: !(Maybe Text)
  , _gmlmrsStartedAt                   :: !(Maybe POSIX)
  , _gmlmrsScoreThreshold              :: !(Maybe Double)
  , _gmlmrsFinishedAt                  :: !(Maybe POSIX)
  , _gmlmrsCreatedByIAMUser            :: !(Maybe Text)
  , _gmlmrsName                        :: !(Maybe Text)
  , _gmlmrsLogURI                      :: !(Maybe Text)
  , _gmlmrsEndpointInfo                :: !(Maybe RealtimeEndpointInfo)
  , _gmlmrsTrainingDataSourceId        :: !(Maybe Text)
  , _gmlmrsMessage                     :: !(Maybe Text)
  , _gmlmrsMLModelType                 :: !(Maybe MLModelType)
  , _gmlmrsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMLModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmlmrsStatus' - The current status of the @MLModel@ . This element can have one of the following values:     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to describe a @MLModel@ .    * @INPROGRESS@ - The request is processing.    * @FAILED@ - The request did not run to completion. The ML model isn't usable.    * @COMPLETED@ - The request completed successfully.    * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
--
-- * 'gmlmrsLastUpdatedAt' - The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
--
-- * 'gmlmrsTrainingParameters' - A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs. The following is the current set of training parameters:      * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance. The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
--
-- * 'gmlmrsScoreThresholdLastUpdatedAt' - The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
--
-- * 'gmlmrsCreatedAt' - The time that the @MLModel@ was created. The time is expressed in epoch time.
--
-- * 'gmlmrsComputeTime' - The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @MLModel@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @MLModel@ is in the @COMPLETED@ state.
--
-- * 'gmlmrsRecipe' - The recipe to use when training the @MLModel@ . The @Recipe@ provides detailed information about the observation data to use during training, and manipulations to perform on the observation data during training.
--
-- * 'gmlmrsInputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- * 'gmlmrsMLModelId' - The MLModel ID, which is same as the @MLModelId@ in the request.
--
-- * 'gmlmrsSizeInBytes' - Undocumented member.
--
-- * 'gmlmrsSchema' - The schema used by all of the data files referenced by the @DataSource@ .
--
-- * 'gmlmrsStartedAt' - The epoch time when Amazon Machine Learning marked the @MLModel@ as @INPROGRESS@ . @StartedAt@ isn't available if the @MLModel@ is in the @PENDING@ state.
--
-- * 'gmlmrsScoreThreshold' - The scoring threshold is used in binary classification @MLModel@ models. It marks the boundary between a positive prediction and a negative prediction. Output values greater than or equal to the threshold receive a positive result from the MLModel, such as @true@ . Output values less than the threshold receive a negative response from the MLModel, such as @false@ .
--
-- * 'gmlmrsFinishedAt' - The epoch time when Amazon Machine Learning marked the @MLModel@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @MLModel@ is in the @COMPLETED@ or @FAILED@ state.
--
-- * 'gmlmrsCreatedByIAMUser' - The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- * 'gmlmrsName' - A user-supplied name or description of the @MLModel@ .
--
-- * 'gmlmrsLogURI' - A link to the file that contains logs of the @CreateMLModel@ operation.
--
-- * 'gmlmrsEndpointInfo' - The current endpoint of the @MLModel@
--
-- * 'gmlmrsTrainingDataSourceId' - The ID of the training @DataSource@ .
--
-- * 'gmlmrsMessage' - A description of the most recent details about accessing the @MLModel@ .
--
-- * 'gmlmrsMLModelType' - Identifies the @MLModel@ category. The following are the available types:      * REGRESSION -- Produces a numeric result. For example, "What price should a house be listed at?"    * BINARY -- Produces one of two possible results. For example, "Is this an e-commerce website?"    * MULTICLASS -- Produces one of several possible results. For example, "Is this a HIGH, LOW or MEDIUM risk trade?"
--
-- * 'gmlmrsResponseStatus' - -- | The response status code.
getMLModelResponse
    :: Int -- ^ 'gmlmrsResponseStatus'
    -> GetMLModelResponse
getMLModelResponse pResponseStatus_ =
  GetMLModelResponse'
    { _gmlmrsStatus = Nothing
    , _gmlmrsLastUpdatedAt = Nothing
    , _gmlmrsTrainingParameters = Nothing
    , _gmlmrsScoreThresholdLastUpdatedAt = Nothing
    , _gmlmrsCreatedAt = Nothing
    , _gmlmrsComputeTime = Nothing
    , _gmlmrsRecipe = Nothing
    , _gmlmrsInputDataLocationS3 = Nothing
    , _gmlmrsMLModelId = Nothing
    , _gmlmrsSizeInBytes = Nothing
    , _gmlmrsSchema = Nothing
    , _gmlmrsStartedAt = Nothing
    , _gmlmrsScoreThreshold = Nothing
    , _gmlmrsFinishedAt = Nothing
    , _gmlmrsCreatedByIAMUser = Nothing
    , _gmlmrsName = Nothing
    , _gmlmrsLogURI = Nothing
    , _gmlmrsEndpointInfo = Nothing
    , _gmlmrsTrainingDataSourceId = Nothing
    , _gmlmrsMessage = Nothing
    , _gmlmrsMLModelType = Nothing
    , _gmlmrsResponseStatus = pResponseStatus_
    }


-- | The current status of the @MLModel@ . This element can have one of the following values:     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to describe a @MLModel@ .    * @INPROGRESS@ - The request is processing.    * @FAILED@ - The request did not run to completion. The ML model isn't usable.    * @COMPLETED@ - The request completed successfully.    * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
gmlmrsStatus :: Lens' GetMLModelResponse (Maybe EntityStatus)
gmlmrsStatus = lens _gmlmrsStatus (\ s a -> s{_gmlmrsStatus = a})

-- | The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
gmlmrsLastUpdatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsLastUpdatedAt = lens _gmlmrsLastUpdatedAt (\ s a -> s{_gmlmrsLastUpdatedAt = a}) . mapping _Time

-- | A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs. The following is the current set of training parameters:      * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance. The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
gmlmrsTrainingParameters :: Lens' GetMLModelResponse (HashMap Text Text)
gmlmrsTrainingParameters = lens _gmlmrsTrainingParameters (\ s a -> s{_gmlmrsTrainingParameters = a}) . _Default . _Map

-- | The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
gmlmrsScoreThresholdLastUpdatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsScoreThresholdLastUpdatedAt = lens _gmlmrsScoreThresholdLastUpdatedAt (\ s a -> s{_gmlmrsScoreThresholdLastUpdatedAt = a}) . mapping _Time

-- | The time that the @MLModel@ was created. The time is expressed in epoch time.
gmlmrsCreatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsCreatedAt = lens _gmlmrsCreatedAt (\ s a -> s{_gmlmrsCreatedAt = a}) . mapping _Time

-- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @MLModel@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @MLModel@ is in the @COMPLETED@ state.
gmlmrsComputeTime :: Lens' GetMLModelResponse (Maybe Integer)
gmlmrsComputeTime = lens _gmlmrsComputeTime (\ s a -> s{_gmlmrsComputeTime = a})

-- | The recipe to use when training the @MLModel@ . The @Recipe@ provides detailed information about the observation data to use during training, and manipulations to perform on the observation data during training.
gmlmrsRecipe :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsRecipe = lens _gmlmrsRecipe (\ s a -> s{_gmlmrsRecipe = a})

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
gmlmrsInputDataLocationS3 :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsInputDataLocationS3 = lens _gmlmrsInputDataLocationS3 (\ s a -> s{_gmlmrsInputDataLocationS3 = a})

-- | The MLModel ID, which is same as the @MLModelId@ in the request.
gmlmrsMLModelId :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsMLModelId = lens _gmlmrsMLModelId (\ s a -> s{_gmlmrsMLModelId = a})

-- | Undocumented member.
gmlmrsSizeInBytes :: Lens' GetMLModelResponse (Maybe Integer)
gmlmrsSizeInBytes = lens _gmlmrsSizeInBytes (\ s a -> s{_gmlmrsSizeInBytes = a})

-- | The schema used by all of the data files referenced by the @DataSource@ .
gmlmrsSchema :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsSchema = lens _gmlmrsSchema (\ s a -> s{_gmlmrsSchema = a})

-- | The epoch time when Amazon Machine Learning marked the @MLModel@ as @INPROGRESS@ . @StartedAt@ isn't available if the @MLModel@ is in the @PENDING@ state.
gmlmrsStartedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsStartedAt = lens _gmlmrsStartedAt (\ s a -> s{_gmlmrsStartedAt = a}) . mapping _Time

-- | The scoring threshold is used in binary classification @MLModel@ models. It marks the boundary between a positive prediction and a negative prediction. Output values greater than or equal to the threshold receive a positive result from the MLModel, such as @true@ . Output values less than the threshold receive a negative response from the MLModel, such as @false@ .
gmlmrsScoreThreshold :: Lens' GetMLModelResponse (Maybe Double)
gmlmrsScoreThreshold = lens _gmlmrsScoreThreshold (\ s a -> s{_gmlmrsScoreThreshold = a})

-- | The epoch time when Amazon Machine Learning marked the @MLModel@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @MLModel@ is in the @COMPLETED@ or @FAILED@ state.
gmlmrsFinishedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsFinishedAt = lens _gmlmrsFinishedAt (\ s a -> s{_gmlmrsFinishedAt = a}) . mapping _Time

-- | The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
gmlmrsCreatedByIAMUser :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsCreatedByIAMUser = lens _gmlmrsCreatedByIAMUser (\ s a -> s{_gmlmrsCreatedByIAMUser = a})

-- | A user-supplied name or description of the @MLModel@ .
gmlmrsName :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsName = lens _gmlmrsName (\ s a -> s{_gmlmrsName = a})

-- | A link to the file that contains logs of the @CreateMLModel@ operation.
gmlmrsLogURI :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsLogURI = lens _gmlmrsLogURI (\ s a -> s{_gmlmrsLogURI = a})

-- | The current endpoint of the @MLModel@
gmlmrsEndpointInfo :: Lens' GetMLModelResponse (Maybe RealtimeEndpointInfo)
gmlmrsEndpointInfo = lens _gmlmrsEndpointInfo (\ s a -> s{_gmlmrsEndpointInfo = a})

-- | The ID of the training @DataSource@ .
gmlmrsTrainingDataSourceId :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsTrainingDataSourceId = lens _gmlmrsTrainingDataSourceId (\ s a -> s{_gmlmrsTrainingDataSourceId = a})

-- | A description of the most recent details about accessing the @MLModel@ .
gmlmrsMessage :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsMessage = lens _gmlmrsMessage (\ s a -> s{_gmlmrsMessage = a})

-- | Identifies the @MLModel@ category. The following are the available types:      * REGRESSION -- Produces a numeric result. For example, "What price should a house be listed at?"    * BINARY -- Produces one of two possible results. For example, "Is this an e-commerce website?"    * MULTICLASS -- Produces one of several possible results. For example, "Is this a HIGH, LOW or MEDIUM risk trade?"
gmlmrsMLModelType :: Lens' GetMLModelResponse (Maybe MLModelType)
gmlmrsMLModelType = lens _gmlmrsMLModelType (\ s a -> s{_gmlmrsMLModelType = a})

-- | -- | The response status code.
gmlmrsResponseStatus :: Lens' GetMLModelResponse Int
gmlmrsResponseStatus = lens _gmlmrsResponseStatus (\ s a -> s{_gmlmrsResponseStatus = a})

instance NFData GetMLModelResponse where
