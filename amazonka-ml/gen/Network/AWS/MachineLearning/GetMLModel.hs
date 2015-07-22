{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.GetMLModel
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns an @MLModel@ that includes detailed metadata, and data source
-- information as well as the current status of the @MLModel@.
--
-- @GetMLModel@ provides results in normal or verbose format.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_GetMLModel.html>
module Network.AWS.MachineLearning.GetMLModel
    (
    -- * Request
      GetMLModel
    -- ** Request constructor
    , getMLModel
    -- ** Request lenses
    , gmlmrqVerbose
    , gmlmrqMLModelId

    -- * Response
    , GetMLModelResponse
    -- ** Response constructor
    , getMLModelResponse
    -- ** Response lenses
    , gmlmrsTrainingParameters
    , gmlmrsLastUpdatedAt
    , gmlmrsCreatedAt
    , gmlmrsScoreThresholdLastUpdatedAt
    , gmlmrsRecipe
    , gmlmrsInputDataLocationS3
    , gmlmrsSizeInBytes
    , gmlmrsMLModelId
    , gmlmrsSchema
    , gmlmrsScoreThreshold
    , gmlmrsName
    , gmlmrsCreatedByIAMUser
    , gmlmrsLogURI
    , gmlmrsEndpointInfo
    , gmlmrsTrainingDataSourceId
    , gmlmrsMessage
    , gmlmrsMLModelType
    , gmlmrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getMLModel' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmlmrqVerbose'
--
-- * 'gmlmrqMLModelId'
data GetMLModel = GetMLModel'
    { _gmlmrqVerbose   :: !(Maybe Bool)
    , _gmlmrqMLModelId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetMLModel' smart constructor.
getMLModel :: Text -> GetMLModel
getMLModel pMLModelId =
    GetMLModel'
    { _gmlmrqVerbose = Nothing
    , _gmlmrqMLModelId = pMLModelId
    }

-- | Specifies whether the @GetMLModel@ operation should return @Recipe@.
--
-- If true, @Recipe@ is returned.
--
-- If false, @Recipe@ is not returned.
gmlmrqVerbose :: Lens' GetMLModel (Maybe Bool)
gmlmrqVerbose = lens _gmlmrqVerbose (\ s a -> s{_gmlmrqVerbose = a});

-- | The ID assigned to the @MLModel@ at creation.
gmlmrqMLModelId :: Lens' GetMLModel Text
gmlmrqMLModelId = lens _gmlmrqMLModelId (\ s a -> s{_gmlmrqMLModelId = a});

instance AWSRequest GetMLModel where
        type Sv GetMLModel = MachineLearning
        type Rs GetMLModel = GetMLModelResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetMLModelResponse' <$>
                   (x .?> "TrainingParameters" .!@ mempty) <*>
                     (x .?> "LastUpdatedAt")
                     <*> (x .?> "CreatedAt")
                     <*> (x .?> "ScoreThresholdLastUpdatedAt")
                     <*> (x .?> "Recipe")
                     <*> (x .?> "InputDataLocationS3")
                     <*> (x .?> "SizeInBytes")
                     <*> (x .?> "MLModelId")
                     <*> (x .?> "Schema")
                     <*> (x .?> "ScoreThreshold")
                     <*> (x .?> "Name")
                     <*> (x .?> "CreatedByIamUser")
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
              ["Verbose" .= _gmlmrqVerbose,
               "MLModelId" .= _gmlmrqMLModelId]

instance ToPath GetMLModel where
        toPath = const "/"

instance ToQuery GetMLModel where
        toQuery = const mempty

-- | Represents the output of a GetMLModel operation, and provides detailed
-- information about a @MLModel@.
--
-- /See:/ 'getMLModelResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmlmrsTrainingParameters'
--
-- * 'gmlmrsLastUpdatedAt'
--
-- * 'gmlmrsCreatedAt'
--
-- * 'gmlmrsScoreThresholdLastUpdatedAt'
--
-- * 'gmlmrsRecipe'
--
-- * 'gmlmrsInputDataLocationS3'
--
-- * 'gmlmrsSizeInBytes'
--
-- * 'gmlmrsMLModelId'
--
-- * 'gmlmrsSchema'
--
-- * 'gmlmrsScoreThreshold'
--
-- * 'gmlmrsName'
--
-- * 'gmlmrsCreatedByIAMUser'
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
data GetMLModelResponse = GetMLModelResponse'
    { _gmlmrsTrainingParameters          :: !(Maybe (Map Text Text))
    , _gmlmrsLastUpdatedAt               :: !(Maybe POSIX)
    , _gmlmrsCreatedAt                   :: !(Maybe POSIX)
    , _gmlmrsScoreThresholdLastUpdatedAt :: !(Maybe POSIX)
    , _gmlmrsRecipe                      :: !(Maybe Text)
    , _gmlmrsInputDataLocationS3         :: !(Maybe Text)
    , _gmlmrsSizeInBytes                 :: !(Maybe Integer)
    , _gmlmrsMLModelId                   :: !(Maybe Text)
    , _gmlmrsSchema                      :: !(Maybe Text)
    , _gmlmrsScoreThreshold              :: !(Maybe Double)
    , _gmlmrsName                        :: !(Maybe Text)
    , _gmlmrsCreatedByIAMUser            :: !(Maybe Text)
    , _gmlmrsLogURI                      :: !(Maybe Text)
    , _gmlmrsEndpointInfo                :: !(Maybe RealtimeEndpointInfo)
    , _gmlmrsTrainingDataSourceId        :: !(Maybe Text)
    , _gmlmrsMessage                     :: !(Maybe Text)
    , _gmlmrsMLModelType                 :: !(Maybe MLModelType)
    , _gmlmrsStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetMLModelResponse' smart constructor.
getMLModelResponse :: Int -> GetMLModelResponse
getMLModelResponse pStatus =
    GetMLModelResponse'
    { _gmlmrsTrainingParameters = Nothing
    , _gmlmrsLastUpdatedAt = Nothing
    , _gmlmrsCreatedAt = Nothing
    , _gmlmrsScoreThresholdLastUpdatedAt = Nothing
    , _gmlmrsRecipe = Nothing
    , _gmlmrsInputDataLocationS3 = Nothing
    , _gmlmrsSizeInBytes = Nothing
    , _gmlmrsMLModelId = Nothing
    , _gmlmrsSchema = Nothing
    , _gmlmrsScoreThreshold = Nothing
    , _gmlmrsName = Nothing
    , _gmlmrsCreatedByIAMUser = Nothing
    , _gmlmrsLogURI = Nothing
    , _gmlmrsEndpointInfo = Nothing
    , _gmlmrsTrainingDataSourceId = Nothing
    , _gmlmrsMessage = Nothing
    , _gmlmrsMLModelType = Nothing
    , _gmlmrsStatus = pStatus
    }

-- | A list of the training parameters in the @MLModel@. The list is
-- implemented as a map of key\/value pairs.
--
-- The following is the current set of training parameters:
--
-- -   @sgd.l1RegularizationAmount@ - Coefficient regularization L1 norm.
--     It controls overfitting the data by penalizing large coefficients.
--     This tends to drive coefficients to zero, resulting in a sparse
--     feature set. If you use this parameter, specify a small value, such
--     as 1.0E-04 or 1.0E-08.
--
--     The value is a double that ranges from 0 to MAX_DOUBLE. The default
--     is not to use L1 normalization. The parameter cannot be used when
--     @L2@ is specified. Use this parameter sparingly.
--
-- -   @sgd.l2RegularizationAmount@ - Coefficient regularization L2 norm.
--     It controls overfitting the data by penalizing large coefficients.
--     This tends to drive coefficients to small, nonzero values. If you
--     use this parameter, specify a small value, such as 1.0E-04 or
--     1.0E-08.
--
--     The value is a double that ranges from 0 to MAX_DOUBLE. The default
--     is not to use L2 normalization. This parameter cannot be used when
--     @L1@ is specified. Use this parameter sparingly.
--
-- -   @sgd.maxPasses@ - The number of times that the training process
--     traverses the observations to build the @MLModel@. The value is an
--     integer that ranges from 1 to 10000. The default value is 10.
--
-- -   @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model.
--     Depending on the input data, the model size might affect
--     performance.
--
--     The value is an integer that ranges from 100000 to 2147483648. The
--     default value is 33554432.
--
gmlmrsTrainingParameters :: Lens' GetMLModelResponse (HashMap Text Text)
gmlmrsTrainingParameters = lens _gmlmrsTrainingParameters (\ s a -> s{_gmlmrsTrainingParameters = a}) . _Default . _Map;

-- | The time of the most recent edit to the @MLModel@. The time is expressed
-- in epoch time.
gmlmrsLastUpdatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsLastUpdatedAt = lens _gmlmrsLastUpdatedAt (\ s a -> s{_gmlmrsLastUpdatedAt = a}) . mapping _Time;

-- | The time that the @MLModel@ was created. The time is expressed in epoch
-- time.
gmlmrsCreatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsCreatedAt = lens _gmlmrsCreatedAt (\ s a -> s{_gmlmrsCreatedAt = a}) . mapping _Time;

-- | The time of the most recent edit to the @ScoreThreshold@. The time is
-- expressed in epoch time.
gmlmrsScoreThresholdLastUpdatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrsScoreThresholdLastUpdatedAt = lens _gmlmrsScoreThresholdLastUpdatedAt (\ s a -> s{_gmlmrsScoreThresholdLastUpdatedAt = a}) . mapping _Time;

-- | The recipe to use when training the @MLModel@. The @Recipe@ provides
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

-- | FIXME: Undocumented member.
gmlmrsSizeInBytes :: Lens' GetMLModelResponse (Maybe Integer)
gmlmrsSizeInBytes = lens _gmlmrsSizeInBytes (\ s a -> s{_gmlmrsSizeInBytes = a});

-- | The MLModel ID which is same as the @MLModelId@ in the request.
gmlmrsMLModelId :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsMLModelId = lens _gmlmrsMLModelId (\ s a -> s{_gmlmrsMLModelId = a});

-- | The schema used by all of the data files referenced by the @DataSource@.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
gmlmrsSchema :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsSchema = lens _gmlmrsSchema (\ s a -> s{_gmlmrsSchema = a});

-- | The scoring threshold is used in binary classification @MLModel@s, and
-- marks the boundary between a positive prediction and a negative
-- prediction.
--
-- Output values greater than or equal to the threshold receive a positive
-- result from the MLModel, such as @true@. Output values less than the
-- threshold receive a negative response from the MLModel, such as @false@.
gmlmrsScoreThreshold :: Lens' GetMLModelResponse (Maybe Double)
gmlmrsScoreThreshold = lens _gmlmrsScoreThreshold (\ s a -> s{_gmlmrsScoreThreshold = a});

-- | A user-supplied name or description of the @MLModel@.
gmlmrsName :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsName = lens _gmlmrsName (\ s a -> s{_gmlmrsName = a});

-- | The AWS user account from which the @MLModel@ was created. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
gmlmrsCreatedByIAMUser :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsCreatedByIAMUser = lens _gmlmrsCreatedByIAMUser (\ s a -> s{_gmlmrsCreatedByIAMUser = a});

-- | A link to the file that contains logs of the @CreateMLModel@ operation.
gmlmrsLogURI :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsLogURI = lens _gmlmrsLogURI (\ s a -> s{_gmlmrsLogURI = a});

-- | The current endpoint of the @MLModel@
gmlmrsEndpointInfo :: Lens' GetMLModelResponse (Maybe RealtimeEndpointInfo)
gmlmrsEndpointInfo = lens _gmlmrsEndpointInfo (\ s a -> s{_gmlmrsEndpointInfo = a});

-- | The ID of the training @DataSource@.
gmlmrsTrainingDataSourceId :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsTrainingDataSourceId = lens _gmlmrsTrainingDataSourceId (\ s a -> s{_gmlmrsTrainingDataSourceId = a});

-- | Description of the most recent details about accessing the @MLModel@.
gmlmrsMessage :: Lens' GetMLModelResponse (Maybe Text)
gmlmrsMessage = lens _gmlmrsMessage (\ s a -> s{_gmlmrsMessage = a});

-- | Identifies the @MLModel@ category. The following are the available
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

-- | FIXME: Undocumented member.
gmlmrsStatus :: Lens' GetMLModelResponse Int
gmlmrsStatus = lens _gmlmrsStatus (\ s a -> s{_gmlmrsStatus = a});
