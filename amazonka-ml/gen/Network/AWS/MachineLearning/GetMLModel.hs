{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.MachineLearning.GetMLModel
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns an 'MLModel' that includes detailed metadata, and data source
-- information as well as the current status of the 'MLModel'.
--
-- 'GetMLModel' provides results in normal or verbose format.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_GetMLModel.html>
module Network.AWS.MachineLearning.GetMLModel
    (
    -- * Request
      GetMLModel
    -- ** Request constructor
    , getMLModel
    -- ** Request lenses
    , gmlmMLModelId
    , gmlmVerbose

    -- * Response
    , GetMLModelResponse
    -- ** Response constructor
    , getMLModelResponse
    -- ** Response lenses
    , gmlmrCreatedAt
    , gmlmrCreatedByIamUser
    , gmlmrEndpointInfo
    , gmlmrInputDataLocationS3
    , gmlmrLastUpdatedAt
    , gmlmrLogUri
    , gmlmrMLModelId
    , gmlmrMLModelType
    , gmlmrMessage
    , gmlmrName
    , gmlmrRecipe
    , gmlmrSchema
    , gmlmrScoreThreshold
    , gmlmrScoreThresholdLastUpdatedAt
    , gmlmrSizeInBytes
    , gmlmrStatus
    , gmlmrTrainingDataSourceId
    , gmlmrTrainingParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data GetMLModel = GetMLModel
    { _gmlmMLModelId :: Text
    , _gmlmVerbose   :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'GetMLModel' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmlmMLModelId' @::@ 'Text'
--
-- * 'gmlmVerbose' @::@ 'Maybe' 'Bool'
--
getMLModel :: Text -- ^ 'gmlmMLModelId'
           -> GetMLModel
getMLModel p1 = GetMLModel
    { _gmlmMLModelId = p1
    , _gmlmVerbose   = Nothing
    }

-- | The ID assigned to the 'MLModel' at creation.
gmlmMLModelId :: Lens' GetMLModel Text
gmlmMLModelId = lens _gmlmMLModelId (\s a -> s { _gmlmMLModelId = a })

-- | Specifies whether the 'GetMLModel' operation should return 'Recipe'.
--
-- If true, 'Recipe' is returned.
--
-- If false, 'Recipe' is not returned.
gmlmVerbose :: Lens' GetMLModel (Maybe Bool)
gmlmVerbose = lens _gmlmVerbose (\s a -> s { _gmlmVerbose = a })

data GetMLModelResponse = GetMLModelResponse
    { _gmlmrCreatedAt                   :: Maybe POSIX
    , _gmlmrCreatedByIamUser            :: Maybe Text
    , _gmlmrEndpointInfo                :: Maybe RealtimeEndpointInfo
    , _gmlmrInputDataLocationS3         :: Maybe Text
    , _gmlmrLastUpdatedAt               :: Maybe POSIX
    , _gmlmrLogUri                      :: Maybe Text
    , _gmlmrMLModelId                   :: Maybe Text
    , _gmlmrMLModelType                 :: Maybe MLModelType
    , _gmlmrMessage                     :: Maybe Text
    , _gmlmrName                        :: Maybe Text
    , _gmlmrRecipe                      :: Maybe Text
    , _gmlmrSchema                      :: Maybe Text
    , _gmlmrScoreThreshold              :: Maybe Double
    , _gmlmrScoreThresholdLastUpdatedAt :: Maybe POSIX
    , _gmlmrSizeInBytes                 :: Maybe Integer
    , _gmlmrStatus                      :: Maybe EntityStatus
    , _gmlmrTrainingDataSourceId        :: Maybe Text
    , _gmlmrTrainingParameters          :: Map Text Text
    } deriving (Eq, Read, Show)

-- | 'GetMLModelResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gmlmrCreatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'gmlmrCreatedByIamUser' @::@ 'Maybe' 'Text'
--
-- * 'gmlmrEndpointInfo' @::@ 'Maybe' 'RealtimeEndpointInfo'
--
-- * 'gmlmrInputDataLocationS3' @::@ 'Maybe' 'Text'
--
-- * 'gmlmrLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'gmlmrLogUri' @::@ 'Maybe' 'Text'
--
-- * 'gmlmrMLModelId' @::@ 'Maybe' 'Text'
--
-- * 'gmlmrMLModelType' @::@ 'Maybe' 'MLModelType'
--
-- * 'gmlmrMessage' @::@ 'Maybe' 'Text'
--
-- * 'gmlmrName' @::@ 'Maybe' 'Text'
--
-- * 'gmlmrRecipe' @::@ 'Maybe' 'Text'
--
-- * 'gmlmrSchema' @::@ 'Maybe' 'Text'
--
-- * 'gmlmrScoreThreshold' @::@ 'Maybe' 'Double'
--
-- * 'gmlmrScoreThresholdLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'gmlmrSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'gmlmrStatus' @::@ 'Maybe' 'EntityStatus'
--
-- * 'gmlmrTrainingDataSourceId' @::@ 'Maybe' 'Text'
--
-- * 'gmlmrTrainingParameters' @::@ 'HashMap' 'Text' 'Text'
--
getMLModelResponse :: GetMLModelResponse
getMLModelResponse = GetMLModelResponse
    { _gmlmrMLModelId                   = Nothing
    , _gmlmrTrainingDataSourceId        = Nothing
    , _gmlmrCreatedByIamUser            = Nothing
    , _gmlmrCreatedAt                   = Nothing
    , _gmlmrLastUpdatedAt               = Nothing
    , _gmlmrName                        = Nothing
    , _gmlmrStatus                      = Nothing
    , _gmlmrSizeInBytes                 = Nothing
    , _gmlmrEndpointInfo                = Nothing
    , _gmlmrTrainingParameters          = mempty
    , _gmlmrInputDataLocationS3         = Nothing
    , _gmlmrMLModelType                 = Nothing
    , _gmlmrScoreThreshold              = Nothing
    , _gmlmrScoreThresholdLastUpdatedAt = Nothing
    , _gmlmrLogUri                      = Nothing
    , _gmlmrMessage                     = Nothing
    , _gmlmrRecipe                      = Nothing
    , _gmlmrSchema                      = Nothing
    }

-- | The time that the 'MLModel' was created. The time is expressed in epoch time.
gmlmrCreatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrCreatedAt = lens _gmlmrCreatedAt (\s a -> s { _gmlmrCreatedAt = a }) . mapping _Time

-- | The AWS user account from which the 'MLModel' was created. The account type can
-- be either an AWS root account or an AWS Identity and Access Management (IAM)
-- user account.
gmlmrCreatedByIamUser :: Lens' GetMLModelResponse (Maybe Text)
gmlmrCreatedByIamUser =
    lens _gmlmrCreatedByIamUser (\s a -> s { _gmlmrCreatedByIamUser = a })

-- | The current endpoint of the 'MLModel'
gmlmrEndpointInfo :: Lens' GetMLModelResponse (Maybe RealtimeEndpointInfo)
gmlmrEndpointInfo =
    lens _gmlmrEndpointInfo (\s a -> s { _gmlmrEndpointInfo = a })

-- | The location of the data file or directory in Amazon Simple Storage Service
-- (Amazon S3).
gmlmrInputDataLocationS3 :: Lens' GetMLModelResponse (Maybe Text)
gmlmrInputDataLocationS3 =
    lens _gmlmrInputDataLocationS3
        (\s a -> s { _gmlmrInputDataLocationS3 = a })

-- | The time of the most recent edit to the 'MLModel'. The time is expressed in
-- epoch time.
gmlmrLastUpdatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrLastUpdatedAt =
    lens _gmlmrLastUpdatedAt (\s a -> s { _gmlmrLastUpdatedAt = a })
        . mapping _Time

-- | A link to the file that contains logs of the 'CreateMLModel' operation.
gmlmrLogUri :: Lens' GetMLModelResponse (Maybe Text)
gmlmrLogUri = lens _gmlmrLogUri (\s a -> s { _gmlmrLogUri = a })

-- | The MLModel ID which is same as the 'MLModelId' in the request.
gmlmrMLModelId :: Lens' GetMLModelResponse (Maybe Text)
gmlmrMLModelId = lens _gmlmrMLModelId (\s a -> s { _gmlmrMLModelId = a })

-- | Identifies the 'MLModel' category. The following are the available types:
--
-- REGRESSION -- Produces a numeric result. For example, "What listing price
-- should a house have?" BINARY -- Produces one of two possible results. For
-- example, "Is this an e-commerce website?" MULTICLASS -- Produces more than
-- two possible results. For example, "Is this a HIGH, LOW or MEDIUM risk trade?"
--
gmlmrMLModelType :: Lens' GetMLModelResponse (Maybe MLModelType)
gmlmrMLModelType = lens _gmlmrMLModelType (\s a -> s { _gmlmrMLModelType = a })

-- | Description of the most recent details about accessing the 'MLModel'.
gmlmrMessage :: Lens' GetMLModelResponse (Maybe Text)
gmlmrMessage = lens _gmlmrMessage (\s a -> s { _gmlmrMessage = a })

-- | A user-supplied name or description of the 'MLModel'.
gmlmrName :: Lens' GetMLModelResponse (Maybe Text)
gmlmrName = lens _gmlmrName (\s a -> s { _gmlmrName = a })

-- | The recipe to use when training the 'MLModel'. The 'Recipe' provides detailed
-- information about the observation data to use during training, as well as
-- manipulations to perform on the observation data during training.
--
-- Note This parameter is provided as part of the verbose format.
--
gmlmrRecipe :: Lens' GetMLModelResponse (Maybe Text)
gmlmrRecipe = lens _gmlmrRecipe (\s a -> s { _gmlmrRecipe = a })

-- | The schema used by all of the data files referenced by the 'DataSource'.
--
-- Note This parameter is provided as part of the verbose format.
--
gmlmrSchema :: Lens' GetMLModelResponse (Maybe Text)
gmlmrSchema = lens _gmlmrSchema (\s a -> s { _gmlmrSchema = a })

-- | The scoring threshold is used in binary classification 'MLModel's, and marks
-- the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the threshold receive a positive
-- result from the MLModel, such as 'true'. Output values less than the threshold
-- receive a negative response from the MLModel, such as 'false'.
gmlmrScoreThreshold :: Lens' GetMLModelResponse (Maybe Double)
gmlmrScoreThreshold =
    lens _gmlmrScoreThreshold (\s a -> s { _gmlmrScoreThreshold = a })

-- | The time of the most recent edit to the 'ScoreThreshold'. The time is expressed
-- in epoch time.
gmlmrScoreThresholdLastUpdatedAt :: Lens' GetMLModelResponse (Maybe UTCTime)
gmlmrScoreThresholdLastUpdatedAt =
    lens _gmlmrScoreThresholdLastUpdatedAt
        (\s a -> s { _gmlmrScoreThresholdLastUpdatedAt = a })
            . mapping _Time

gmlmrSizeInBytes :: Lens' GetMLModelResponse (Maybe Integer)
gmlmrSizeInBytes = lens _gmlmrSizeInBytes (\s a -> s { _gmlmrSizeInBytes = a })

-- | The current status of the 'MLModel'. This element can have one of the following
-- values:
--
-- 'PENDING' - Amazon Machine Learning (Amazon ML) submitted a request to
-- describe a 'MLModel'.  'INPROGRESS' - The request is processing.  'FAILED' - The
-- request did not run to completion. It is not usable.  'COMPLETED' - The request
-- completed successfully.  'DELETED' - The 'MLModel' is marked as deleted. It is
-- not usable.
gmlmrStatus :: Lens' GetMLModelResponse (Maybe EntityStatus)
gmlmrStatus = lens _gmlmrStatus (\s a -> s { _gmlmrStatus = a })

-- | The ID of the training 'DataSource'.
gmlmrTrainingDataSourceId :: Lens' GetMLModelResponse (Maybe Text)
gmlmrTrainingDataSourceId =
    lens _gmlmrTrainingDataSourceId
        (\s a -> s { _gmlmrTrainingDataSourceId = a })

-- | A list of the training parameters in the 'MLModel'. The list is implemented as
-- a map of key/value pairs.
--
-- The following is the current set of training parameters:
--
-- 'sgd.l1RegularizationAmount' - Coefficient regularization L1 norm. It
-- controls overfitting the data by penalizing large coefficients. This tends to
-- drive coefficients to zero, resulting in a sparse feature set. If you use
-- this parameter, specify a small value, such as 1.0E-04 or 1.0E-08.
--
-- The value is a double that ranges from 0 to MAX_DOUBLE. The default is not
-- to use L1 normalization. The parameter cannot be used when 'L2' is specified.
-- Use this parameter sparingly.
--
-- 'sgd.l2RegularizationAmount' - Coefficient regularization L2 norm. It
-- controls overfitting the data by penalizing large coefficients. This tends to
-- drive coefficients to small, nonzero values. If you use this parameter,
-- specify a small value, such as 1.0E-04 or 1.0E-08.
--
-- The value is a double that ranges from 0 to MAX_DOUBLE. The default is not
-- to use L2 normalization. This parameter cannot be used when 'L1' is specified.
-- Use this parameter sparingly.
--
-- 'sgd.maxPasses' - The number of times that the training process traverses the
-- observations to build the 'MLModel'. The value is an integer that ranges from 1
-- to 10000. The default value is 10.
--
-- 'sgd.maxMLModelSizeInBytes' - The maximum allowed size of the model.
-- Depending on the input data, the model size might affect performance.
--
-- The value is an integer that ranges from 100000 to 2147483648. The default
-- value is 33554432.
--
--
gmlmrTrainingParameters :: Lens' GetMLModelResponse (HashMap Text Text)
gmlmrTrainingParameters =
    lens _gmlmrTrainingParameters (\s a -> s { _gmlmrTrainingParameters = a })
        . _Map

instance ToPath GetMLModel where
    toPath = const "/"

instance ToQuery GetMLModel where
    toQuery = const mempty

instance ToHeaders GetMLModel

instance ToJSON GetMLModel where
    toJSON GetMLModel{..} = object
        [ "MLModelId" .= _gmlmMLModelId
        , "Verbose"   .= _gmlmVerbose
        ]

instance AWSRequest GetMLModel where
    type Sv GetMLModel = MachineLearning
    type Rs GetMLModel = GetMLModelResponse

    request  = post "GetMLModel"
    response = jsonResponse

instance FromJSON GetMLModelResponse where
    parseJSON = withObject "GetMLModelResponse" $ \o -> GetMLModelResponse
        <$> o .:? "CreatedAt"
        <*> o .:? "CreatedByIamUser"
        <*> o .:? "EndpointInfo"
        <*> o .:? "InputDataLocationS3"
        <*> o .:? "LastUpdatedAt"
        <*> o .:? "LogUri"
        <*> o .:? "MLModelId"
        <*> o .:? "MLModelType"
        <*> o .:? "Message"
        <*> o .:? "Name"
        <*> o .:? "Recipe"
        <*> o .:? "Schema"
        <*> o .:? "ScoreThreshold"
        <*> o .:? "ScoreThresholdLastUpdatedAt"
        <*> o .:? "SizeInBytes"
        <*> o .:? "Status"
        <*> o .:? "TrainingDataSourceId"
        <*> o .:? "TrainingParameters" .!= mempty
