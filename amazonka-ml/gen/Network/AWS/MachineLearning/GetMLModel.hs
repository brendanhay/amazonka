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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an 'MLModel' that includes detailed metadata, and data source
-- information as well as the current status of the 'MLModel'.
--
-- 'GetMLModel' provides results in normal or verbose format.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_GetMLModel.html AWS API Reference> for GetMLModel.
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
    , gmlmrsLastUpdatedAt
    , gmlmrsTrainingParameters
    , gmlmrsScoreThresholdLastUpdatedAt
    , gmlmrsCreatedAt
    , gmlmrsRecipe
    , gmlmrsInputDataLocationS3
    , gmlmrsMLModelId
    , gmlmrsSizeInBytes
    , gmlmrsSchema
    , gmlmrsScoreThreshold
    , gmlmrsCreatedByIAMUser
    , gmlmrsName
    , gmlmrsLogURI
    , gmlmrsEndpointInfo
    , gmlmrsTrainingDataSourceId
    , gmlmrsMessage
    , gmlmrsMLModelType
    , gmlmrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getMLModel' smart constructor.
data GetMLModel = GetMLModel'
    { _gmlmVerbose   :: !(Maybe Bool)
    , _gmlmMLModelId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetMLModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmlmVerbose'
--
-- * 'gmlmMLModelId'
getMLModel
    :: Text -- ^ 'gmlmMLModelId'
    -> GetMLModel
getMLModel pMLModelId_ =
    GetMLModel'
    { _gmlmVerbose = Nothing
    , _gmlmMLModelId = pMLModelId_
    }

-- | Specifies whether the 'GetMLModel' operation should return 'Recipe'.
--
-- If true, 'Recipe' is returned.
--
-- If false, 'Recipe' is not returned.
gmlmVerbose :: Lens' GetMLModel (Maybe Bool)
gmlmVerbose = lens _gmlmVerbose (\ s a -> s{_gmlmVerbose = a});

-- | The ID assigned to the 'MLModel' at creation.
gmlmMLModelId :: Lens' GetMLModel Text
gmlmMLModelId = lens _gmlmMLModelId (\ s a -> s{_gmlmMLModelId = a});

instance AWSRequest GetMLModel where
        type Rs GetMLModel = GetMLModelResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 GetMLModelResponse' <$>
                   (x .?> "LastUpdatedAt") <*>
                     (x .?> "TrainingParameters" .!@ mempty)
                     <*> (x .?> "ScoreThresholdLastUpdatedAt")
                     <*> (x .?> "CreatedAt")
                     <*> (x .?> "Recipe")
                     <*> (x .?> "InputDataLocationS3")
                     <*> (x .?> "MLModelId")
                     <*> (x .?> "SizeInBytes")
                     <*> (x .?> "Schema")
                     <*> (x .?> "ScoreThreshold")
                     <*> (x .?> "CreatedByIamUser")
                     <*> (x .?> "Name")
                     <*> (x .?> "LogUri")
                     <*> (x .?> "EndpointInfo")
                     <*> (x .?> "TrainingDataSourceId")
                     <*> (x .?> "Message")
                     <*> (x .?> "MLModelType")
                     <*> (pure (fromEnum s)))

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

-- | Represents the output of a GetMLModel operation, and provides detailed
-- information about a 'MLModel'.
--
-- /See:/ 'getMLModelResponse' smart constructor.
data GetMLModelResponse = GetMLModelResponse'
    { _gmlmrsLastUpdatedAt               :: !(Maybe POSIX)
    , _gmlmrsTrainingParameters          :: !(Maybe (Map Text Text))
    , _gmlmrsScoreThresholdLastUpdatedAt :: !(Maybe POSIX)
    , _gmlmrsCreatedAt                   :: !(Maybe POSIX)
    , _gmlmrsRecipe                      :: !(Maybe Text)
    , _gmlmrsInputDataLocationS3         :: !(Maybe Text)
    , _gmlmrsMLModelId                   :: !(Maybe Text)
    , _gmlmrsSizeInBytes                 :: !(Maybe Integer)
    , _gmlmrsSchema                      :: !(Maybe Text)
    , _gmlmrsScoreThreshold              :: !(Maybe Double)
    , _gmlmrsCreatedByIAMUser            :: !(Maybe Text)
    , _gmlmrsName                        :: !(Maybe Text)
    , _gmlmrsLogURI                      :: !(Maybe Text)
    , _gmlmrsEndpointInfo                :: !(Maybe RealtimeEndpointInfo)
    , _gmlmrsTrainingDataSourceId        :: !(Maybe Text)
    , _gmlmrsMessage                     :: !(Maybe Text)
    , _gmlmrsMLModelType                 :: !(Maybe MLModelType)
    , _gmlmrsStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetMLModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmlmrsLastUpdatedAt'
--
-- * 'gmlmrsTrainingParameters'
--
-- * 'gmlmrsScoreThresholdLastUpdatedAt'
--
-- * 'gmlmrsCreatedAt'
--
-- * 'gmlmrsRecipe'
--
-- * 'gmlmrsInputDataLocationS3'
--
-- * 'gmlmrsMLModelId'
--
-- * 'gmlmrsSizeInBytes'
--
-- * 'gmlmrsSchema'
--
-- * 'gmlmrsScoreThreshold'
--
-- * 'gmlmrsCreatedByIAMUser'
--
-- * 'gmlmrsName'
--
-- * 'gmlmrsLogURI'
--
-- * 'gmlmrsEndpointInfo'
--
-- * 'gmlmrsTrainingDataSourceId'
--
-- * 'gmlmrsMessage'
--
-- * 'gmlmrsMLModelType'
--
-- * 'gmlmrsStatus'
getMLModelResponse
    :: Int -- ^ 'gmlmrsStatus'
    -> GetMLModelResponse
getMLModelResponse pStatus_ =
    GetMLModelResponse'
    { _gmlmrsLastUpdatedAt = Nothing
    , _gmlmrsTrainingParameters = Nothing
    , _gmlmrsScoreThresholdLastUpdatedAt = Nothing
    , _gmlmrsCreatedAt = Nothing
    , _gmlmrsRecipe = Nothing
    , _gmlmrsInputDataLocationS3 = Nothing
    , _gmlmrsMLModelId = Nothing
    , _gmlmrsSizeInBytes = Nothing
    , _gmlmrsSchema = Nothing
    , _gmlmrsScoreThreshold = Nothing
    , _gmlmrsCreatedByIAMUser = Nothing
    , _gmlmrsName = Nothing
    , _gmlmrsLogURI = Nothing
    , _gmlmrsEndpointInfo = Nothing
    , _gmlmrsTrainingDataSourceId = Nothing
    , _gmlmrsMessage = Nothing
    , _gmlmrsMLModelType = Nothing
    , _gmlmrsStatus = pStatus_
    }

-- | The time of the most recent edit to the 'MLModel'. The time is expressed
-- in epoch time.
gmlmrsLastUpdatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsLastUpdatedAt = lens _gmlmrsLastUpdatedAt (\ s a -> s{_gmlmrsLastUpdatedAt = a}) . mapping _Time;

-- | A list of the training parameters in the 'MLModel'. The list is
-- implemented as a map of key\/value pairs.
--
-- The following is the current set of training parameters:
--
-- -   'sgd.l1RegularizationAmount' - Coefficient regularization L1 norm.
--     It controls overfitting the data by penalizing large coefficients.
--     This tends to drive coefficients to zero, resulting in a sparse
--     feature set. If you use this parameter, specify a small value, such
--     as 1.0E-04 or 1.0E-08.
--
--     The value is a double that ranges from 0 to MAX_DOUBLE. The default
--     is not to use L1 normalization. The parameter cannot be used when
--     'L2' is specified. Use this parameter sparingly.
--
-- -   'sgd.l2RegularizationAmount' - Coefficient regularization L2 norm.
--     It controls overfitting the data by penalizing large coefficients.
--     This tends to drive coefficients to small, nonzero values. If you
--     use this parameter, specify a small value, such as 1.0E-04 or
--     1.0E-08.
--
--     The value is a double that ranges from 0 to MAX_DOUBLE. The default
--     is not to use L2 normalization. This parameter cannot be used when
--     'L1' is specified. Use this parameter sparingly.
--
-- -   'sgd.maxPasses' - The number of times that the training process
--     traverses the observations to build the 'MLModel'. The value is an
--     integer that ranges from 1 to 10000. The default value is 10.
--
-- -   'sgd.maxMLModelSizeInBytes' - The maximum allowed size of the model.
--     Depending on the input data, the model size might affect
--     performance.
--
--     The value is an integer that ranges from 100000 to 2147483648. The
--     default value is 33554432.
--
gmlmrsTrainingParameters :: Lens' GetMLModelResponse (HashMap Text Text)
gmlmrsTrainingParameters = lens _gmlmrsTrainingParameters (\ s a -> s{_gmlmrsTrainingParameters = a}) . _Default . _Map;

-- | The time of the most recent edit to the 'ScoreThreshold'. The time is
-- expressed in epoch time.
gmlmrsScoreThresholdLastUpdatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsScoreThresholdLastUpdatedAt = lens _gmlmrsScoreThresholdLastUpdatedAt (\ s a -> s{_gmlmrsScoreThresholdLastUpdatedAt = a}) . mapping _Time;

-- | The time that the 'MLModel' was created. The time is expressed in epoch
-- time.
gmlmrsCreatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsCreatedAt = lens _gmlmrsCreatedAt (\ s a -> s{_gmlmrsCreatedAt = a}) . mapping _Time;

-- | The recipe to use when training the 'MLModel'. The 'Recipe' provides
-- detailed information about the observation data to use during training,
-- as well as manipulations to perform on the observation data during
-- training.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
gmlmrsRecipe :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsRecipe = lens _gmlmrsRecipe (\ s a -> s{_gmlmrsRecipe = a});

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
gmlmrsInputDataLocationS3 :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsInputDataLocationS3 = lens _gmlmrsInputDataLocationS3 (\ s a -> s{_gmlmrsInputDataLocationS3 = a});

-- | The MLModel ID which is same as the 'MLModelId' in the request.
gmlmrsMLModelId :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsMLModelId = lens _gmlmrsMLModelId (\ s a -> s{_gmlmrsMLModelId = a});

-- | Undocumented member.
gmlmrsSizeInBytes :: Lens' GetMLModelResponse (Maybe Integer)
gmlmrsSizeInBytes = lens _gmlmrsSizeInBytes (\ s a -> s{_gmlmrsSizeInBytes = a});

-- | The schema used by all of the data files referenced by the 'DataSource'.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
gmlmrsSchema :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsSchema = lens _gmlmrsSchema (\ s a -> s{_gmlmrsSchema = a});

-- | The scoring threshold is used in binary classification 'MLModel's, and
-- marks the boundary between a positive prediction and a negative
-- prediction.
--
-- Output values greater than or equal to the threshold receive a positive
-- result from the MLModel, such as 'true'. Output values less than the
-- threshold receive a negative response from the MLModel, such as 'false'.
gmlmrsScoreThreshold :: Lens' GetMLModelResponse (Maybe Double)
gmlmrsScoreThreshold = lens _gmlmrsScoreThreshold (\ s a -> s{_gmlmrsScoreThreshold = a});

-- | The AWS user account from which the 'MLModel' was created. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
gmlmrsCreatedByIAMUser :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsCreatedByIAMUser = lens _gmlmrsCreatedByIAMUser (\ s a -> s{_gmlmrsCreatedByIAMUser = a});

-- | A user-supplied name or description of the 'MLModel'.
gmlmrsName :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsName = lens _gmlmrsName (\ s a -> s{_gmlmrsName = a});

-- | A link to the file that contains logs of the 'CreateMLModel' operation.
gmlmrsLogURI :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsLogURI = lens _gmlmrsLogURI (\ s a -> s{_gmlmrsLogURI = a});

-- | The current endpoint of the 'MLModel'
gmlmrsEndpointInfo :: Lens' GetMLModelResponse (Maybe RealtimeEndpointInfo)
gmlmrsEndpointInfo = lens _gmlmrsEndpointInfo (\ s a -> s{_gmlmrsEndpointInfo = a});

-- | The ID of the training 'DataSource'.
gmlmrsTrainingDataSourceId :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsTrainingDataSourceId = lens _gmlmrsTrainingDataSourceId (\ s a -> s{_gmlmrsTrainingDataSourceId = a});

-- | Description of the most recent details about accessing the 'MLModel'.
gmlmrsMessage :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsMessage = lens _gmlmrsMessage (\ s a -> s{_gmlmrsMessage = a});

-- | Identifies the 'MLModel' category. The following are the available
-- types:
--
-- -   REGRESSION -- Produces a numeric result. For example, \"What listing
--     price should a house have?\"
-- -   BINARY -- Produces one of two possible results. For example, \"Is
--     this an e-commerce website?\"
-- -   MULTICLASS -- Produces more than two possible results. For example,
--     \"Is this a HIGH, LOW or MEDIUM risk trade?\"
gmlmrsMLModelType :: Lens' GetMLModelResponse (Maybe MLModelType)
gmlmrsMLModelType = lens _gmlmrsMLModelType (\ s a -> s{_gmlmrsMLModelType = a});

-- | The response status code.
gmlmrsStatus :: Lens' GetMLModelResponse Int
gmlmrsStatus = lens _gmlmrsStatus (\ s a -> s{_gmlmrsStatus = a});
