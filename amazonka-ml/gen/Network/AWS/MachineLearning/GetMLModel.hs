{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.MachineLearning.GetMLModel
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns an @MLModel@ that includes detailed metadata, and data source
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
    , gmlmVerbose
    , gmlmMLModelId

    -- * Response
    , GetMLModelResponse
    -- ** Response constructor
    , getMLModelResponse
    -- ** Response lenses
    , gmlmrTrainingParameters
    , gmlmrLastUpdatedAt
    , gmlmrCreatedAt
    , gmlmrScoreThresholdLastUpdatedAt
    , gmlmrRecipe
    , gmlmrInputDataLocationS3
    , gmlmrSizeInBytes
    , gmlmrMLModelId
    , gmlmrSchema
    , gmlmrScoreThreshold
    , gmlmrName
    , gmlmrCreatedByIAMUser
    , gmlmrLogURI
    , gmlmrEndpointInfo
    , gmlmrTrainingDataSourceId
    , gmlmrMessage
    , gmlmrMLModelType
    , gmlmrStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getMLModel' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmlmVerbose'
--
-- * 'gmlmMLModelId'
data GetMLModel = GetMLModel'
    { _gmlmVerbose   :: Maybe Bool
    , _gmlmMLModelId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetMLModel' smart constructor.
getMLModel :: Text -> GetMLModel
getMLModel pMLModelId =
    GetMLModel'
    { _gmlmVerbose = Nothing
    , _gmlmMLModelId = pMLModelId
    }

-- | Specifies whether the @GetMLModel@ operation should return @Recipe@.
--
-- If true, @Recipe@ is returned.
--
-- If false, @Recipe@ is not returned.
gmlmVerbose :: Lens' GetMLModel (Maybe Bool)
gmlmVerbose = lens _gmlmVerbose (\ s a -> s{_gmlmVerbose = a});

-- | The ID assigned to the @MLModel@ at creation.
gmlmMLModelId :: Lens' GetMLModel Text
gmlmMLModelId = lens _gmlmMLModelId (\ s a -> s{_gmlmMLModelId = a});

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
              ["Verbose" .= _gmlmVerbose,
               "MLModelId" .= _gmlmMLModelId]

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
-- * 'gmlmrTrainingParameters'
--
-- * 'gmlmrLastUpdatedAt'
--
-- * 'gmlmrCreatedAt'
--
-- * 'gmlmrScoreThresholdLastUpdatedAt'
--
-- * 'gmlmrRecipe'
--
-- * 'gmlmrInputDataLocationS3'
--
-- * 'gmlmrSizeInBytes'
--
-- * 'gmlmrMLModelId'
--
-- * 'gmlmrSchema'
--
-- * 'gmlmrScoreThreshold'
--
-- * 'gmlmrName'
--
-- * 'gmlmrCreatedByIAMUser'
--
-- * 'gmlmrLogURI'
--
-- * 'gmlmrEndpointInfo'
--
-- * 'gmlmrTrainingDataSourceId'
--
-- * 'gmlmrMessage'
--
-- * 'gmlmrMLModelType'
--
-- * 'gmlmrStatus'
data GetMLModelResponse = GetMLModelResponse'
    { _gmlmrTrainingParameters          :: Maybe (Map Text Text)
    , _gmlmrLastUpdatedAt               :: Maybe POSIX
    , _gmlmrCreatedAt                   :: Maybe POSIX
    , _gmlmrScoreThresholdLastUpdatedAt :: Maybe POSIX
    , _gmlmrRecipe                      :: Maybe Text
    , _gmlmrInputDataLocationS3         :: Maybe Text
    , _gmlmrSizeInBytes                 :: Maybe Integer
    , _gmlmrMLModelId                   :: Maybe Text
    , _gmlmrSchema                      :: Maybe Text
    , _gmlmrScoreThreshold              :: Maybe Double
    , _gmlmrName                        :: Maybe Text
    , _gmlmrCreatedByIAMUser            :: Maybe Text
    , _gmlmrLogURI                      :: Maybe Text
    , _gmlmrEndpointInfo                :: Maybe RealtimeEndpointInfo
    , _gmlmrTrainingDataSourceId        :: Maybe Text
    , _gmlmrMessage                     :: Maybe Text
    , _gmlmrMLModelType                 :: Maybe MLModelType
    , _gmlmrStatus                      :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetMLModelResponse' smart constructor.
getMLModelResponse :: Int -> GetMLModelResponse
getMLModelResponse pStatus =
    GetMLModelResponse'
    { _gmlmrTrainingParameters = Nothing
    , _gmlmrLastUpdatedAt = Nothing
    , _gmlmrCreatedAt = Nothing
    , _gmlmrScoreThresholdLastUpdatedAt = Nothing
    , _gmlmrRecipe = Nothing
    , _gmlmrInputDataLocationS3 = Nothing
    , _gmlmrSizeInBytes = Nothing
    , _gmlmrMLModelId = Nothing
    , _gmlmrSchema = Nothing
    , _gmlmrScoreThreshold = Nothing
    , _gmlmrName = Nothing
    , _gmlmrCreatedByIAMUser = Nothing
    , _gmlmrLogURI = Nothing
    , _gmlmrEndpointInfo = Nothing
    , _gmlmrTrainingDataSourceId = Nothing
    , _gmlmrMessage = Nothing
    , _gmlmrMLModelType = Nothing
    , _gmlmrStatus = pStatus
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
gmlmrTrainingParameters :: Lens' GetMLModelResponse (HashMap Text Text)
gmlmrTrainingParameters = lens _gmlmrTrainingParameters (\ s a -> s{_gmlmrTrainingParameters = a}) . _Default . _Map;

-- | The time of the most recent edit to the @MLModel@. The time is expressed
-- in epoch time.
gmlmrLastUpdatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrLastUpdatedAt = lens _gmlmrLastUpdatedAt (\ s a -> s{_gmlmrLastUpdatedAt = a}) . mapping _Time;

-- | The time that the @MLModel@ was created. The time is expressed in epoch
-- time.
gmlmrCreatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrCreatedAt = lens _gmlmrCreatedAt (\ s a -> s{_gmlmrCreatedAt = a}) . mapping _Time;

-- | The time of the most recent edit to the @ScoreThreshold@. The time is
-- expressed in epoch time.
gmlmrScoreThresholdLastUpdatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrScoreThresholdLastUpdatedAt = lens _gmlmrScoreThresholdLastUpdatedAt (\ s a -> s{_gmlmrScoreThresholdLastUpdatedAt = a}) . mapping _Time;

-- | The recipe to use when training the @MLModel@. The @Recipe@ provides
-- detailed information about the observation data to use during training,
-- as well as manipulations to perform on the observation data during
-- training.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
gmlmrRecipe :: Lens' GetMLModelResponse (Maybe Text)
gmlmrRecipe = lens _gmlmrRecipe (\ s a -> s{_gmlmrRecipe = a});

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
gmlmrInputDataLocationS3 :: Lens' GetMLModelResponse (Maybe Text)
gmlmrInputDataLocationS3 = lens _gmlmrInputDataLocationS3 (\ s a -> s{_gmlmrInputDataLocationS3 = a});

-- | FIXME: Undocumented member.
gmlmrSizeInBytes :: Lens' GetMLModelResponse (Maybe Integer)
gmlmrSizeInBytes = lens _gmlmrSizeInBytes (\ s a -> s{_gmlmrSizeInBytes = a});

-- | The MLModel ID which is same as the @MLModelId@ in the request.
gmlmrMLModelId :: Lens' GetMLModelResponse (Maybe Text)
gmlmrMLModelId = lens _gmlmrMLModelId (\ s a -> s{_gmlmrMLModelId = a});

-- | The schema used by all of the data files referenced by the @DataSource@.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
gmlmrSchema :: Lens' GetMLModelResponse (Maybe Text)
gmlmrSchema = lens _gmlmrSchema (\ s a -> s{_gmlmrSchema = a});

-- | The scoring threshold is used in binary classification @MLModel@s, and
-- marks the boundary between a positive prediction and a negative
-- prediction.
--
-- Output values greater than or equal to the threshold receive a positive
-- result from the MLModel, such as @true@. Output values less than the
-- threshold receive a negative response from the MLModel, such as @false@.
gmlmrScoreThreshold :: Lens' GetMLModelResponse (Maybe Double)
gmlmrScoreThreshold = lens _gmlmrScoreThreshold (\ s a -> s{_gmlmrScoreThreshold = a});

-- | A user-supplied name or description of the @MLModel@.
gmlmrName :: Lens' GetMLModelResponse (Maybe Text)
gmlmrName = lens _gmlmrName (\ s a -> s{_gmlmrName = a});

-- | The AWS user account from which the @MLModel@ was created. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
gmlmrCreatedByIAMUser :: Lens' GetMLModelResponse (Maybe Text)
gmlmrCreatedByIAMUser = lens _gmlmrCreatedByIAMUser (\ s a -> s{_gmlmrCreatedByIAMUser = a});

-- | A link to the file that contains logs of the @CreateMLModel@ operation.
gmlmrLogURI :: Lens' GetMLModelResponse (Maybe Text)
gmlmrLogURI = lens _gmlmrLogURI (\ s a -> s{_gmlmrLogURI = a});

-- | The current endpoint of the @MLModel@
gmlmrEndpointInfo :: Lens' GetMLModelResponse (Maybe RealtimeEndpointInfo)
gmlmrEndpointInfo = lens _gmlmrEndpointInfo (\ s a -> s{_gmlmrEndpointInfo = a});

-- | The ID of the training @DataSource@.
gmlmrTrainingDataSourceId :: Lens' GetMLModelResponse (Maybe Text)
gmlmrTrainingDataSourceId = lens _gmlmrTrainingDataSourceId (\ s a -> s{_gmlmrTrainingDataSourceId = a});

-- | Description of the most recent details about accessing the @MLModel@.
gmlmrMessage :: Lens' GetMLModelResponse (Maybe Text)
gmlmrMessage = lens _gmlmrMessage (\ s a -> s{_gmlmrMessage = a});

-- | Identifies the @MLModel@ category. The following are the available
-- types:
--
-- -   REGRESSION -- Produces a numeric result. For example, \"What listing
--     price should a house have?\"
-- -   BINARY -- Produces one of two possible results. For example, \"Is
--     this an e-commerce website?\"
-- -   MULTICLASS -- Produces more than two possible results. For example,
--     \"Is this a HIGH, LOW or MEDIUM risk trade?\"
gmlmrMLModelType :: Lens' GetMLModelResponse (Maybe MLModelType)
gmlmrMLModelType = lens _gmlmrMLModelType (\ s a -> s{_gmlmrMLModelType = a});

-- | FIXME: Undocumented member.
gmlmrStatus :: Lens' GetMLModelResponse Int
gmlmrStatus = lens _gmlmrStatus (\ s a -> s{_gmlmrStatus = a});
