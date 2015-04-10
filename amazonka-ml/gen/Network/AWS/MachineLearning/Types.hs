{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.MachineLearning.Types
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

module Network.AWS.MachineLearning.Types
    (
    -- * Service
      MachineLearning
    -- ** Error
    , JSONError

    -- * BatchPredictionFilterVariable
    , BatchPredictionFilterVariable (..)

    -- * PerformanceMetrics
    , PerformanceMetrics
    , performanceMetrics
    , pmProperties

    -- * RealtimeEndpointInfo
    , RealtimeEndpointInfo
    , realtimeEndpointInfo
    , reiCreatedAt
    , reiEndpointStatus
    , reiEndpointUrl
    , reiPeakRequestsPerSecond

    -- * Prediction
    , Prediction
    , prediction
    , pDetails
    , pPredictedLabel
    , pPredictedScores
    , pPredictedValue

    -- * S3DataSpec
    , S3DataSpec
    , s3DataSpec
    , sdsDataLocationS3
    , sdsDataRearrangement
    , sdsDataSchema
    , sdsDataSchemaLocationS3

    -- * DetailsAttributes
    , DetailsAttributes (..)

    -- * EvaluationFilterVariable
    , EvaluationFilterVariable (..)

    -- * RealtimeEndpointStatus
    , RealtimeEndpointStatus (..)

    -- * RDSDataSpec
    , RDSDataSpec
    , rdsdataSpec
    , rdsdsDataRearrangement
    , rdsdsDataSchema
    , rdsdsDataSchemaUri
    , rdsdsDatabaseCredentials
    , rdsdsDatabaseInformation
    , rdsdsResourceRole
    , rdsdsS3StagingLocation
    , rdsdsSecurityGroupIds
    , rdsdsSelectSqlQuery
    , rdsdsServiceRole
    , rdsdsSubnetId

    -- * RDSMetadata
    , RDSMetadata
    , rdsmetadata
    , rdsmDataPipelineId
    , rdsmDatabase
    , rdsmDatabaseUserName
    , rdsmResourceRole
    , rdsmSelectSqlQuery
    , rdsmServiceRole

    -- * RedshiftDatabase
    , RedshiftDatabase
    , redshiftDatabase
    , rdClusterIdentifier
    , rdDatabaseName

    -- * RedshiftDatabaseCredentials
    , RedshiftDatabaseCredentials
    , redshiftDatabaseCredentials
    , rdcPassword
    , rdcUsername

    -- * MLModel
    , MLModel
    , mlmodel
    , mlmAlgorithm
    , mlmCreatedAt
    , mlmCreatedByIamUser
    , mlmEndpointInfo
    , mlmInputDataLocationS3
    , mlmLastUpdatedAt
    , mlmMLModelId
    , mlmMLModelType
    , mlmMessage
    , mlmName
    , mlmScoreThreshold
    , mlmScoreThresholdLastUpdatedAt
    , mlmSizeInBytes
    , mlmStatus
    , mlmTrainingDataSourceId
    , mlmTrainingParameters

    -- * BatchPrediction
    , BatchPrediction
    , batchPrediction
    , bpBatchPredictionDataSourceId
    , bpBatchPredictionId
    , bpCreatedAt
    , bpCreatedByIamUser
    , bpInputDataLocationS3
    , bpLastUpdatedAt
    , bpMLModelId
    , bpMessage
    , bpName
    , bpOutputUri
    , bpStatus

    -- * SortOrder
    , SortOrder (..)

    -- * Algorithm
    , Algorithm (..)

    -- * EntityStatus
    , EntityStatus (..)

    -- * DataSource
    , DataSource
    , dataSource
    , dsComputeStatistics
    , dsCreatedAt
    , dsCreatedByIamUser
    , dsDataLocationS3
    , dsDataRearrangement
    , dsDataSizeInBytes
    , dsDataSourceId
    , dsLastUpdatedAt
    , dsMessage
    , dsName
    , dsNumberOfFiles
    , dsRDSMetadata
    , dsRedshiftMetadata
    , dsRoleARN
    , dsStatus

    -- * RDSDatabase
    , RDSDatabase
    , rdsdatabase
    , rdsdDatabaseName
    , rdsdInstanceIdentifier

    -- * RDSDatabaseCredentials
    , RDSDatabaseCredentials
    , rdsdatabaseCredentials
    , rdsdcPassword
    , rdsdcUsername

    -- * MLModelFilterVariable
    , MLModelFilterVariable (..)

    -- * DataSourceFilterVariable
    , DataSourceFilterVariable (..)

    -- * RedshiftDataSpec
    , RedshiftDataSpec
    , redshiftDataSpec
    , rdsDataRearrangement
    , rdsDataSchema
    , rdsDataSchemaUri
    , rdsDatabaseCredentials
    , rdsDatabaseInformation
    , rdsS3StagingLocation
    , rdsSelectSqlQuery

    -- * Evaluation
    , Evaluation
    , evaluation
    , eCreatedAt
    , eCreatedByIamUser
    , eEvaluationDataSourceId
    , eEvaluationId
    , eInputDataLocationS3
    , eLastUpdatedAt
    , eMLModelId
    , eMessage
    , eName
    , ePerformanceMetrics
    , eStatus

    -- * RedshiftMetadata
    , RedshiftMetadata
    , redshiftMetadata
    , rmDatabaseUserName
    , rmRedshiftDatabase
    , rmSelectSqlQuery

    -- * MLModelType
    , MLModelType (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2014-12-12@ of the Amazon Machine Learning service.
data MachineLearning

instance AWSService MachineLearning where
    type Sg MachineLearning = V4
    type Er MachineLearning = JSONError

    service = service'
      where
        service' :: Service MachineLearning
        service' = Service
            { _svcAbbrev       = "MachineLearning"
            , _svcPrefix       = "machinelearning"
            , _svcVersion      = "2014-12-12"
            , _svcTargetPrefix = Just "AmazonML_20141212"
            , _svcJSONVersion  = Just "1.1"
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry MachineLearning
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data BatchPredictionFilterVariable
    = CreatedAt     -- ^ CreatedAt
    | DataSourceId  -- ^ DataSourceId
    | DataURI       -- ^ DataURI
    | IAMUser       -- ^ IAMUser
    | LastUpdatedAt -- ^ LastUpdatedAt
    | MLModelId     -- ^ MLModelId
    | Name          -- ^ Name
    | Status        -- ^ Status
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable BatchPredictionFilterVariable

instance FromText BatchPredictionFilterVariable where
    parser = takeLowerText >>= \case
        "createdat"     -> pure CreatedAt
        "datasourceid"  -> pure DataSourceId
        "datauri"       -> pure DataURI
        "iamuser"       -> pure IAMUser
        "lastupdatedat" -> pure LastUpdatedAt
        "mlmodelid"     -> pure MLModelId
        "name"          -> pure Name
        "status"        -> pure Status
        e               -> fail $
            "Failure parsing BatchPredictionFilterVariable from " ++ show e

instance ToText BatchPredictionFilterVariable where
    toText = \case
        CreatedAt     -> "CreatedAt"
        DataSourceId  -> "DataSourceId"
        DataURI       -> "DataURI"
        IAMUser       -> "IAMUser"
        LastUpdatedAt -> "LastUpdatedAt"
        MLModelId     -> "MLModelId"
        Name          -> "Name"
        Status        -> "Status"

instance ToByteString BatchPredictionFilterVariable
instance ToHeader     BatchPredictionFilterVariable
instance ToQuery      BatchPredictionFilterVariable

instance FromJSON BatchPredictionFilterVariable where
    parseJSON = parseJSONText "BatchPredictionFilterVariable"

instance ToJSON BatchPredictionFilterVariable where
    toJSON = toJSONText

newtype PerformanceMetrics = PerformanceMetrics
    { _pmProperties :: Map Text Text
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'PerformanceMetrics' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmProperties' @::@ 'HashMap' 'Text' 'Text'
--
performanceMetrics :: PerformanceMetrics
performanceMetrics = PerformanceMetrics
    { _pmProperties = mempty
    }

pmProperties :: Lens' PerformanceMetrics (HashMap Text Text)
pmProperties = lens _pmProperties (\s a -> s { _pmProperties = a }) . _Map

instance FromJSON PerformanceMetrics where
    parseJSON = withObject "PerformanceMetrics" $ \o -> PerformanceMetrics
        <$> o .:? "Properties" .!= mempty

instance ToJSON PerformanceMetrics where
    toJSON PerformanceMetrics{..} = object
        [ "Properties" .= _pmProperties
        ]

data RealtimeEndpointInfo = RealtimeEndpointInfo
    { _reiCreatedAt             :: Maybe POSIX
    , _reiEndpointStatus        :: Maybe RealtimeEndpointStatus
    , _reiEndpointUrl           :: Maybe Text
    , _reiPeakRequestsPerSecond :: Maybe Int
    } deriving (Eq, Read, Show)

-- | 'RealtimeEndpointInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reiCreatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'reiEndpointStatus' @::@ 'Maybe' 'RealtimeEndpointStatus'
--
-- * 'reiEndpointUrl' @::@ 'Maybe' 'Text'
--
-- * 'reiPeakRequestsPerSecond' @::@ 'Maybe' 'Int'
--
realtimeEndpointInfo :: RealtimeEndpointInfo
realtimeEndpointInfo = RealtimeEndpointInfo
    { _reiPeakRequestsPerSecond = Nothing
    , _reiCreatedAt             = Nothing
    , _reiEndpointUrl           = Nothing
    , _reiEndpointStatus        = Nothing
    }

-- | The time that the request to create the real-time endpoint for the 'MLModel'
-- was received. The time is expressed in epoch time.
reiCreatedAt :: Lens' RealtimeEndpointInfo (Maybe UTCTime)
reiCreatedAt = lens _reiCreatedAt (\s a -> s { _reiCreatedAt = a }) . mapping _Time

-- | The current status of the real-time endpoint for the 'MLModel'. This element
-- can have one of the following values:
--
-- NONE - Endpoint does not exist or was previously deleted. READY - Endpoint
-- is ready to be used for real-time predictions. UPDATING - Updating/creating
-- the endpoint.
reiEndpointStatus :: Lens' RealtimeEndpointInfo (Maybe RealtimeEndpointStatus)
reiEndpointStatus =
    lens _reiEndpointStatus (\s a -> s { _reiEndpointStatus = a })

-- | The URI that specifies where to send real-time prediction requests for the 'MLModel'.
--
-- Note The application must wait until the real-time endpoint is ready before
-- using this URI.
--
--
reiEndpointUrl :: Lens' RealtimeEndpointInfo (Maybe Text)
reiEndpointUrl = lens _reiEndpointUrl (\s a -> s { _reiEndpointUrl = a })

-- | The maximum processing rate for the real-time endpoint for 'MLModel', measured
-- in incoming requests per second.
reiPeakRequestsPerSecond :: Lens' RealtimeEndpointInfo (Maybe Int)
reiPeakRequestsPerSecond =
    lens _reiPeakRequestsPerSecond
        (\s a -> s { _reiPeakRequestsPerSecond = a })

instance FromJSON RealtimeEndpointInfo where
    parseJSON = withObject "RealtimeEndpointInfo" $ \o -> RealtimeEndpointInfo
        <$> o .:? "CreatedAt"
        <*> o .:? "EndpointStatus"
        <*> o .:? "EndpointUrl"
        <*> o .:? "PeakRequestsPerSecond"

instance ToJSON RealtimeEndpointInfo where
    toJSON RealtimeEndpointInfo{..} = object
        [ "PeakRequestsPerSecond" .= _reiPeakRequestsPerSecond
        , "CreatedAt"             .= _reiCreatedAt
        , "EndpointUrl"           .= _reiEndpointUrl
        , "EndpointStatus"        .= _reiEndpointStatus
        ]

data Prediction = Prediction
    { _pDetails         :: Map DetailsAttributes Text
    , _pPredictedLabel  :: Maybe Text
    , _pPredictedScores :: Map Text Double
    , _pPredictedValue  :: Maybe Double
    } deriving (Eq, Read, Show)

-- | 'Prediction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pDetails' @::@ 'HashMap' 'DetailsAttributes' 'Text'
--
-- * 'pPredictedLabel' @::@ 'Maybe' 'Text'
--
-- * 'pPredictedScores' @::@ 'HashMap' 'Text' 'Double'
--
-- * 'pPredictedValue' @::@ 'Maybe' 'Double'
--
prediction :: Prediction
prediction = Prediction
    { _pPredictedLabel  = Nothing
    , _pPredictedValue  = Nothing
    , _pPredictedScores = mempty
    , _pDetails         = mempty
    }

pDetails :: Lens' Prediction (HashMap DetailsAttributes Text)
pDetails = lens _pDetails (\s a -> s { _pDetails = a }) . _Map

-- | The prediction label for either a BINARY or MULTICLASS 'MLModel'.
pPredictedLabel :: Lens' Prediction (Maybe Text)
pPredictedLabel = lens _pPredictedLabel (\s a -> s { _pPredictedLabel = a })

pPredictedScores :: Lens' Prediction (HashMap Text Double)
pPredictedScores = lens _pPredictedScores (\s a -> s { _pPredictedScores = a }) . _Map

-- | The prediction value for REGRESSION 'MLModel'.
pPredictedValue :: Lens' Prediction (Maybe Double)
pPredictedValue = lens _pPredictedValue (\s a -> s { _pPredictedValue = a })

instance FromJSON Prediction where
    parseJSON = withObject "Prediction" $ \o -> Prediction
        <$> o .:? "details" .!= mempty
        <*> o .:? "predictedLabel"
        <*> o .:? "predictedScores" .!= mempty
        <*> o .:? "predictedValue"

instance ToJSON Prediction where
    toJSON Prediction{..} = object
        [ "predictedLabel"  .= _pPredictedLabel
        , "predictedValue"  .= _pPredictedValue
        , "predictedScores" .= _pPredictedScores
        , "details"         .= _pDetails
        ]

data S3DataSpec = S3DataSpec
    { _sdsDataLocationS3       :: Text
    , _sdsDataRearrangement    :: Maybe Text
    , _sdsDataSchema           :: Maybe Text
    , _sdsDataSchemaLocationS3 :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'S3DataSpec' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdsDataLocationS3' @::@ 'Text'
--
-- * 'sdsDataRearrangement' @::@ 'Maybe' 'Text'
--
-- * 'sdsDataSchema' @::@ 'Maybe' 'Text'
--
-- * 'sdsDataSchemaLocationS3' @::@ 'Maybe' 'Text'
--
s3DataSpec :: Text -- ^ 'sdsDataLocationS3'
           -> S3DataSpec
s3DataSpec p1 = S3DataSpec
    { _sdsDataLocationS3       = p1
    , _sdsDataRearrangement    = Nothing
    , _sdsDataSchema           = Nothing
    , _sdsDataSchemaLocationS3 = Nothing
    }

-- | The location of the data file(s) used by a 'DataSource'. The URI specifies a
-- data file or an Amazon Simple Storage Service (Amazon S3) directory or bucket
-- containing data files.
sdsDataLocationS3 :: Lens' S3DataSpec Text
sdsDataLocationS3 =
    lens _sdsDataLocationS3 (\s a -> s { _sdsDataLocationS3 = a })

-- | Describes the splitting requirement of a 'Datasource'.
sdsDataRearrangement :: Lens' S3DataSpec (Maybe Text)
sdsDataRearrangement =
    lens _sdsDataRearrangement (\s a -> s { _sdsDataRearrangement = a })

-- | Describes the schema for an Amazon S3 'DataSource'.
sdsDataSchema :: Lens' S3DataSpec (Maybe Text)
sdsDataSchema = lens _sdsDataSchema (\s a -> s { _sdsDataSchema = a })

-- | Describes the schema Location in Amazon S3.
sdsDataSchemaLocationS3 :: Lens' S3DataSpec (Maybe Text)
sdsDataSchemaLocationS3 =
    lens _sdsDataSchemaLocationS3 (\s a -> s { _sdsDataSchemaLocationS3 = a })

instance FromJSON S3DataSpec where
    parseJSON = withObject "S3DataSpec" $ \o -> S3DataSpec
        <$> o .:  "DataLocationS3"
        <*> o .:? "DataRearrangement"
        <*> o .:? "DataSchema"
        <*> o .:? "DataSchemaLocationS3"

instance ToJSON S3DataSpec where
    toJSON S3DataSpec{..} = object
        [ "DataLocationS3"       .= _sdsDataLocationS3
        , "DataRearrangement"    .= _sdsDataRearrangement
        , "DataSchema"           .= _sdsDataSchema
        , "DataSchemaLocationS3" .= _sdsDataSchemaLocationS3
        ]

data DetailsAttributes
    = DAAlgorithm           -- ^ Algorithm
    | DAPredictiveModelType -- ^ PredictiveModelType
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DetailsAttributes

instance FromText DetailsAttributes where
    parser = takeLowerText >>= \case
        "algorithm"           -> pure DAAlgorithm
        "predictivemodeltype" -> pure DAPredictiveModelType
        e                     -> fail $
            "Failure parsing DetailsAttributes from " ++ show e

instance ToText DetailsAttributes where
    toText = \case
        DAAlgorithm           -> "Algorithm"
        DAPredictiveModelType -> "PredictiveModelType"

instance ToByteString DetailsAttributes
instance ToHeader     DetailsAttributes
instance ToQuery      DetailsAttributes

instance FromJSON DetailsAttributes where
    parseJSON = parseJSONText "DetailsAttributes"

instance ToJSON DetailsAttributes where
    toJSON = toJSONText

data EvaluationFilterVariable
    = EFVCreatedAt     -- ^ CreatedAt
    | EFVDataSourceId  -- ^ DataSourceId
    | EFVDataURI       -- ^ DataURI
    | EFVIAMUser       -- ^ IAMUser
    | EFVLastUpdatedAt -- ^ LastUpdatedAt
    | EFVMLModelId     -- ^ MLModelId
    | EFVName          -- ^ Name
    | EFVStatus        -- ^ Status
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable EvaluationFilterVariable

instance FromText EvaluationFilterVariable where
    parser = takeLowerText >>= \case
        "createdat"     -> pure EFVCreatedAt
        "datasourceid"  -> pure EFVDataSourceId
        "datauri"       -> pure EFVDataURI
        "iamuser"       -> pure EFVIAMUser
        "lastupdatedat" -> pure EFVLastUpdatedAt
        "mlmodelid"     -> pure EFVMLModelId
        "name"          -> pure EFVName
        "status"        -> pure EFVStatus
        e               -> fail $
            "Failure parsing EvaluationFilterVariable from " ++ show e

instance ToText EvaluationFilterVariable where
    toText = \case
        EFVCreatedAt     -> "CreatedAt"
        EFVDataSourceId  -> "DataSourceId"
        EFVDataURI       -> "DataURI"
        EFVIAMUser       -> "IAMUser"
        EFVLastUpdatedAt -> "LastUpdatedAt"
        EFVMLModelId     -> "MLModelId"
        EFVName          -> "Name"
        EFVStatus        -> "Status"

instance ToByteString EvaluationFilterVariable
instance ToHeader     EvaluationFilterVariable
instance ToQuery      EvaluationFilterVariable

instance FromJSON EvaluationFilterVariable where
    parseJSON = parseJSONText "EvaluationFilterVariable"

instance ToJSON EvaluationFilterVariable where
    toJSON = toJSONText

data RealtimeEndpointStatus
    = Failed   -- ^ FAILED
    | None     -- ^ NONE
    | Ready    -- ^ READY
    | Updating -- ^ UPDATING
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable RealtimeEndpointStatus

instance FromText RealtimeEndpointStatus where
    parser = takeLowerText >>= \case
        "failed"   -> pure Failed
        "none"     -> pure None
        "ready"    -> pure Ready
        "updating" -> pure Updating
        e          -> fail $
            "Failure parsing RealtimeEndpointStatus from " ++ show e

instance ToText RealtimeEndpointStatus where
    toText = \case
        Failed   -> "FAILED"
        None     -> "NONE"
        Ready    -> "READY"
        Updating -> "UPDATING"

instance ToByteString RealtimeEndpointStatus
instance ToHeader     RealtimeEndpointStatus
instance ToQuery      RealtimeEndpointStatus

instance FromJSON RealtimeEndpointStatus where
    parseJSON = parseJSONText "RealtimeEndpointStatus"

instance ToJSON RealtimeEndpointStatus where
    toJSON = toJSONText

data RDSDataSpec = RDSDataSpec
    { _rdsdsDataRearrangement   :: Maybe Text
    , _rdsdsDataSchema          :: Maybe Text
    , _rdsdsDataSchemaUri       :: Maybe Text
    , _rdsdsDatabaseCredentials :: RDSDatabaseCredentials
    , _rdsdsDatabaseInformation :: RDSDatabase
    , _rdsdsResourceRole        :: Text
    , _rdsdsS3StagingLocation   :: Text
    , _rdsdsSecurityGroupIds    :: List "SecurityGroupIds" Text
    , _rdsdsSelectSqlQuery      :: Text
    , _rdsdsServiceRole         :: Text
    , _rdsdsSubnetId            :: Text
    } deriving (Eq, Read, Show)

-- | 'RDSDataSpec' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdsdsDataRearrangement' @::@ 'Maybe' 'Text'
--
-- * 'rdsdsDataSchema' @::@ 'Maybe' 'Text'
--
-- * 'rdsdsDataSchemaUri' @::@ 'Maybe' 'Text'
--
-- * 'rdsdsDatabaseCredentials' @::@ 'RDSDatabaseCredentials'
--
-- * 'rdsdsDatabaseInformation' @::@ 'RDSDatabase'
--
-- * 'rdsdsResourceRole' @::@ 'Text'
--
-- * 'rdsdsS3StagingLocation' @::@ 'Text'
--
-- * 'rdsdsSecurityGroupIds' @::@ ['Text']
--
-- * 'rdsdsSelectSqlQuery' @::@ 'Text'
--
-- * 'rdsdsServiceRole' @::@ 'Text'
--
-- * 'rdsdsSubnetId' @::@ 'Text'
--
rdsdataSpec :: RDSDatabase -- ^ 'rdsdsDatabaseInformation'
            -> Text -- ^ 'rdsdsSelectSqlQuery'
            -> RDSDatabaseCredentials -- ^ 'rdsdsDatabaseCredentials'
            -> Text -- ^ 'rdsdsS3StagingLocation'
            -> Text -- ^ 'rdsdsResourceRole'
            -> Text -- ^ 'rdsdsServiceRole'
            -> Text -- ^ 'rdsdsSubnetId'
            -> RDSDataSpec
rdsdataSpec p1 p2 p3 p4 p5 p6 p7 = RDSDataSpec
    { _rdsdsDatabaseInformation = p1
    , _rdsdsSelectSqlQuery      = p2
    , _rdsdsDatabaseCredentials = p3
    , _rdsdsS3StagingLocation   = p4
    , _rdsdsResourceRole        = p5
    , _rdsdsServiceRole         = p6
    , _rdsdsSubnetId            = p7
    , _rdsdsDataRearrangement   = Nothing
    , _rdsdsDataSchema          = Nothing
    , _rdsdsDataSchemaUri       = Nothing
    , _rdsdsSecurityGroupIds    = mempty
    }

-- | DataRearrangement - A JSON string that represents the splitting requirement
-- of a 'DataSource'.
--
--
-- Sample - ' "{\"randomSeed\":\"some-random-seed\",\"splitting\":{\"percentBegin\":10,\"percentEnd\":60}}"'
rdsdsDataRearrangement :: Lens' RDSDataSpec (Maybe Text)
rdsdsDataRearrangement =
    lens _rdsdsDataRearrangement (\s a -> s { _rdsdsDataRearrangement = a })

-- | A JSON string that represents the schema. This is not required if 'DataSchemaUri' is specified.
rdsdsDataSchema :: Lens' RDSDataSpec (Maybe Text)
rdsdsDataSchema = lens _rdsdsDataSchema (\s a -> s { _rdsdsDataSchema = a })

-- | The Amazon S3 location of the 'DataSchema'.
rdsdsDataSchemaUri :: Lens' RDSDataSpec (Maybe Text)
rdsdsDataSchemaUri =
    lens _rdsdsDataSchemaUri (\s a -> s { _rdsdsDataSchemaUri = a })

-- | The AWS Identity and Access Management (IAM) credentials that are used
-- connect to the Amazon RDS database.
rdsdsDatabaseCredentials :: Lens' RDSDataSpec RDSDatabaseCredentials
rdsdsDatabaseCredentials =
    lens _rdsdsDatabaseCredentials
        (\s a -> s { _rdsdsDatabaseCredentials = a })

-- | Describes the 'DatabaseName' and 'InstanceIdentifier' of an an Amazon RDS
-- database.
rdsdsDatabaseInformation :: Lens' RDSDataSpec RDSDatabase
rdsdsDatabaseInformation =
    lens _rdsdsDatabaseInformation
        (\s a -> s { _rdsdsDatabaseInformation = a })

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic
-- Compute Cloud (Amazon EC2) instance to carry out the copy operation from
-- Amazon RDS to an Amazon S3 task. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for
-- data pipelines.
rdsdsResourceRole :: Lens' RDSDataSpec Text
rdsdsResourceRole =
    lens _rdsdsResourceRole (\s a -> s { _rdsdsResourceRole = a })

-- | The Amazon S3 location for staging Amazon RDS data. The data retrieved from
-- Amazon RDS using 'SelectSqlQuery' is stored in this location.
rdsdsS3StagingLocation :: Lens' RDSDataSpec Text
rdsdsS3StagingLocation =
    lens _rdsdsS3StagingLocation (\s a -> s { _rdsdsS3StagingLocation = a })

-- | The security group IDs to be used to access a VPC-based RDS DB instance.
-- Ensure that there are appropriate ingress rules set up to allow access to the
-- RDS DB instance. This attribute is used by Data Pipeline to carry out the
-- copy operation from Amazon RDS to an Amazon S3 task.
rdsdsSecurityGroupIds :: Lens' RDSDataSpec [Text]
rdsdsSecurityGroupIds =
    lens _rdsdsSecurityGroupIds (\s a -> s { _rdsdsSecurityGroupIds = a })
        . _List

-- | The query that is used to retrieve the observation data for the 'DataSource'.
rdsdsSelectSqlQuery :: Lens' RDSDataSpec Text
rdsdsSelectSqlQuery =
    lens _rdsdsSelectSqlQuery (\s a -> s { _rdsdsSelectSqlQuery = a })

-- | The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service to
-- monitor the progress of the copy task from Amazon RDS to Amazon S3. For more
-- information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
rdsdsServiceRole :: Lens' RDSDataSpec Text
rdsdsServiceRole = lens _rdsdsServiceRole (\s a -> s { _rdsdsServiceRole = a })

-- | The subnet ID to be used to access a VPC-based RDS DB instance. This
-- attribute is used by Data Pipeline to carry out the copy task from Amazon RDS
-- to Amazon S3.
rdsdsSubnetId :: Lens' RDSDataSpec Text
rdsdsSubnetId = lens _rdsdsSubnetId (\s a -> s { _rdsdsSubnetId = a })

instance FromJSON RDSDataSpec where
    parseJSON = withObject "RDSDataSpec" $ \o -> RDSDataSpec
        <$> o .:? "DataRearrangement"
        <*> o .:? "DataSchema"
        <*> o .:? "DataSchemaUri"
        <*> o .:  "DatabaseCredentials"
        <*> o .:  "DatabaseInformation"
        <*> o .:  "ResourceRole"
        <*> o .:  "S3StagingLocation"
        <*> o .:? "SecurityGroupIds" .!= mempty
        <*> o .:  "SelectSqlQuery"
        <*> o .:  "ServiceRole"
        <*> o .:  "SubnetId"

instance ToJSON RDSDataSpec where
    toJSON RDSDataSpec{..} = object
        [ "DatabaseInformation" .= _rdsdsDatabaseInformation
        , "SelectSqlQuery"      .= _rdsdsSelectSqlQuery
        , "DatabaseCredentials" .= _rdsdsDatabaseCredentials
        , "S3StagingLocation"   .= _rdsdsS3StagingLocation
        , "DataRearrangement"   .= _rdsdsDataRearrangement
        , "DataSchema"          .= _rdsdsDataSchema
        , "DataSchemaUri"       .= _rdsdsDataSchemaUri
        , "ResourceRole"        .= _rdsdsResourceRole
        , "ServiceRole"         .= _rdsdsServiceRole
        , "SubnetId"            .= _rdsdsSubnetId
        , "SecurityGroupIds"    .= _rdsdsSecurityGroupIds
        ]

data RDSMetadata = RDSMetadata
    { _rdsmDataPipelineId   :: Maybe Text
    , _rdsmDatabase         :: Maybe RDSDatabase
    , _rdsmDatabaseUserName :: Maybe Text
    , _rdsmResourceRole     :: Maybe Text
    , _rdsmSelectSqlQuery   :: Maybe Text
    , _rdsmServiceRole      :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'RDSMetadata' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdsmDataPipelineId' @::@ 'Maybe' 'Text'
--
-- * 'rdsmDatabase' @::@ 'Maybe' 'RDSDatabase'
--
-- * 'rdsmDatabaseUserName' @::@ 'Maybe' 'Text'
--
-- * 'rdsmResourceRole' @::@ 'Maybe' 'Text'
--
-- * 'rdsmSelectSqlQuery' @::@ 'Maybe' 'Text'
--
-- * 'rdsmServiceRole' @::@ 'Maybe' 'Text'
--
rdsmetadata :: RDSMetadata
rdsmetadata = RDSMetadata
    { _rdsmDatabase         = Nothing
    , _rdsmDatabaseUserName = Nothing
    , _rdsmSelectSqlQuery   = Nothing
    , _rdsmResourceRole     = Nothing
    , _rdsmServiceRole      = Nothing
    , _rdsmDataPipelineId   = Nothing
    }

-- | The ID of the Data Pipeline instance that is used to carry to copy data from
-- Amazon RDS to Amazon S3. You can use the ID to find details about the
-- instance in the Data Pipeline console.
rdsmDataPipelineId :: Lens' RDSMetadata (Maybe Text)
rdsmDataPipelineId =
    lens _rdsmDataPipelineId (\s a -> s { _rdsmDataPipelineId = a })

-- | The database details required to connect to an Amazon RDS.
rdsmDatabase :: Lens' RDSMetadata (Maybe RDSDatabase)
rdsmDatabase = lens _rdsmDatabase (\s a -> s { _rdsmDatabase = a })

rdsmDatabaseUserName :: Lens' RDSMetadata (Maybe Text)
rdsmDatabaseUserName =
    lens _rdsmDatabaseUserName (\s a -> s { _rdsmDatabaseUserName = a })

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2 instance
-- to carry out the copy task from Amazon RDS to Amazon S3. For more
-- information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
rdsmResourceRole :: Lens' RDSMetadata (Maybe Text)
rdsmResourceRole = lens _rdsmResourceRole (\s a -> s { _rdsmResourceRole = a })

-- | The SQL query that is supplied during 'CreateDataSourceFromRDS'. Returns only
-- if 'Verbose' is true in 'GetDataSourceInput'.
rdsmSelectSqlQuery :: Lens' RDSMetadata (Maybe Text)
rdsmSelectSqlQuery =
    lens _rdsmSelectSqlQuery (\s a -> s { _rdsmSelectSqlQuery = a })

-- | The role (DataPipelineDefaultRole) assumed by the Data Pipeline service to
-- monitor the progress of the copy task from Amazon RDS to Amazon S3. For more
-- information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
rdsmServiceRole :: Lens' RDSMetadata (Maybe Text)
rdsmServiceRole = lens _rdsmServiceRole (\s a -> s { _rdsmServiceRole = a })

instance FromJSON RDSMetadata where
    parseJSON = withObject "RDSMetadata" $ \o -> RDSMetadata
        <$> o .:? "DataPipelineId"
        <*> o .:? "Database"
        <*> o .:? "DatabaseUserName"
        <*> o .:? "ResourceRole"
        <*> o .:? "SelectSqlQuery"
        <*> o .:? "ServiceRole"

instance ToJSON RDSMetadata where
    toJSON RDSMetadata{..} = object
        [ "Database"         .= _rdsmDatabase
        , "DatabaseUserName" .= _rdsmDatabaseUserName
        , "SelectSqlQuery"   .= _rdsmSelectSqlQuery
        , "ResourceRole"     .= _rdsmResourceRole
        , "ServiceRole"      .= _rdsmServiceRole
        , "DataPipelineId"   .= _rdsmDataPipelineId
        ]

data RedshiftDatabase = RedshiftDatabase
    { _rdClusterIdentifier :: Text
    , _rdDatabaseName      :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RedshiftDatabase' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdClusterIdentifier' @::@ 'Text'
--
-- * 'rdDatabaseName' @::@ 'Text'
--
redshiftDatabase :: Text -- ^ 'rdDatabaseName'
                 -> Text -- ^ 'rdClusterIdentifier'
                 -> RedshiftDatabase
redshiftDatabase p1 p2 = RedshiftDatabase
    { _rdDatabaseName      = p1
    , _rdClusterIdentifier = p2
    }

rdClusterIdentifier :: Lens' RedshiftDatabase Text
rdClusterIdentifier =
    lens _rdClusterIdentifier (\s a -> s { _rdClusterIdentifier = a })

rdDatabaseName :: Lens' RedshiftDatabase Text
rdDatabaseName = lens _rdDatabaseName (\s a -> s { _rdDatabaseName = a })

instance FromJSON RedshiftDatabase where
    parseJSON = withObject "RedshiftDatabase" $ \o -> RedshiftDatabase
        <$> o .:  "ClusterIdentifier"
        <*> o .:  "DatabaseName"

instance ToJSON RedshiftDatabase where
    toJSON RedshiftDatabase{..} = object
        [ "DatabaseName"      .= _rdDatabaseName
        , "ClusterIdentifier" .= _rdClusterIdentifier
        ]

data RedshiftDatabaseCredentials = RedshiftDatabaseCredentials
    { _rdcPassword :: Text
    , _rdcUsername :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RedshiftDatabaseCredentials' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdcPassword' @::@ 'Text'
--
-- * 'rdcUsername' @::@ 'Text'
--
redshiftDatabaseCredentials :: Text -- ^ 'rdcUsername'
                            -> Text -- ^ 'rdcPassword'
                            -> RedshiftDatabaseCredentials
redshiftDatabaseCredentials p1 p2 = RedshiftDatabaseCredentials
    { _rdcUsername = p1
    , _rdcPassword = p2
    }

rdcPassword :: Lens' RedshiftDatabaseCredentials Text
rdcPassword = lens _rdcPassword (\s a -> s { _rdcPassword = a })

rdcUsername :: Lens' RedshiftDatabaseCredentials Text
rdcUsername = lens _rdcUsername (\s a -> s { _rdcUsername = a })

instance FromJSON RedshiftDatabaseCredentials where
    parseJSON = withObject "RedshiftDatabaseCredentials" $ \o -> RedshiftDatabaseCredentials
        <$> o .:  "Password"
        <*> o .:  "Username"

instance ToJSON RedshiftDatabaseCredentials where
    toJSON RedshiftDatabaseCredentials{..} = object
        [ "Username" .= _rdcUsername
        , "Password" .= _rdcPassword
        ]

data MLModel = MLModel
    { _mlmAlgorithm                   :: Maybe Algorithm
    , _mlmCreatedAt                   :: Maybe POSIX
    , _mlmCreatedByIamUser            :: Maybe Text
    , _mlmEndpointInfo                :: Maybe RealtimeEndpointInfo
    , _mlmInputDataLocationS3         :: Maybe Text
    , _mlmLastUpdatedAt               :: Maybe POSIX
    , _mlmMLModelId                   :: Maybe Text
    , _mlmMLModelType                 :: Maybe MLModelType
    , _mlmMessage                     :: Maybe Text
    , _mlmName                        :: Maybe Text
    , _mlmScoreThreshold              :: Maybe Double
    , _mlmScoreThresholdLastUpdatedAt :: Maybe POSIX
    , _mlmSizeInBytes                 :: Maybe Integer
    , _mlmStatus                      :: Maybe EntityStatus
    , _mlmTrainingDataSourceId        :: Maybe Text
    , _mlmTrainingParameters          :: Map Text Text
    } deriving (Eq, Read, Show)

-- | 'MLModel' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mlmAlgorithm' @::@ 'Maybe' 'Algorithm'
--
-- * 'mlmCreatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'mlmCreatedByIamUser' @::@ 'Maybe' 'Text'
--
-- * 'mlmEndpointInfo' @::@ 'Maybe' 'RealtimeEndpointInfo'
--
-- * 'mlmInputDataLocationS3' @::@ 'Maybe' 'Text'
--
-- * 'mlmLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'mlmMLModelId' @::@ 'Maybe' 'Text'
--
-- * 'mlmMLModelType' @::@ 'Maybe' 'MLModelType'
--
-- * 'mlmMessage' @::@ 'Maybe' 'Text'
--
-- * 'mlmName' @::@ 'Maybe' 'Text'
--
-- * 'mlmScoreThreshold' @::@ 'Maybe' 'Double'
--
-- * 'mlmScoreThresholdLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'mlmSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'mlmStatus' @::@ 'Maybe' 'EntityStatus'
--
-- * 'mlmTrainingDataSourceId' @::@ 'Maybe' 'Text'
--
-- * 'mlmTrainingParameters' @::@ 'HashMap' 'Text' 'Text'
--
mlmodel :: MLModel
mlmodel = MLModel
    { _mlmMLModelId                   = Nothing
    , _mlmTrainingDataSourceId        = Nothing
    , _mlmCreatedByIamUser            = Nothing
    , _mlmCreatedAt                   = Nothing
    , _mlmLastUpdatedAt               = Nothing
    , _mlmName                        = Nothing
    , _mlmStatus                      = Nothing
    , _mlmSizeInBytes                 = Nothing
    , _mlmEndpointInfo                = Nothing
    , _mlmTrainingParameters          = mempty
    , _mlmInputDataLocationS3         = Nothing
    , _mlmAlgorithm                   = Nothing
    , _mlmMLModelType                 = Nothing
    , _mlmScoreThreshold              = Nothing
    , _mlmScoreThresholdLastUpdatedAt = Nothing
    , _mlmMessage                     = Nothing
    }

-- | The algorithm used to train the 'MLModel'. The following algorithm is supported:
--
-- SGD -- Stochastic gradient descent. The goal of SGD is to minimize the
-- gradient of the loss function.
mlmAlgorithm :: Lens' MLModel (Maybe Algorithm)
mlmAlgorithm = lens _mlmAlgorithm (\s a -> s { _mlmAlgorithm = a })

-- | The time that the 'MLModel' was created. The time is expressed in epoch time.
mlmCreatedAt :: Lens' MLModel (Maybe UTCTime)
mlmCreatedAt = lens _mlmCreatedAt (\s a -> s { _mlmCreatedAt = a }) . mapping _Time

-- | The AWS user account from which the 'MLModel' was created. The account type can
-- be either an AWS root account or an AWS Identity and Access Management (IAM)
-- user account.
mlmCreatedByIamUser :: Lens' MLModel (Maybe Text)
mlmCreatedByIamUser =
    lens _mlmCreatedByIamUser (\s a -> s { _mlmCreatedByIamUser = a })

-- | The current endpoint of the 'MLModel'.
mlmEndpointInfo :: Lens' MLModel (Maybe RealtimeEndpointInfo)
mlmEndpointInfo = lens _mlmEndpointInfo (\s a -> s { _mlmEndpointInfo = a })

-- | The location of the data file or directory in Amazon Simple Storage Service
-- (Amazon S3).
mlmInputDataLocationS3 :: Lens' MLModel (Maybe Text)
mlmInputDataLocationS3 =
    lens _mlmInputDataLocationS3 (\s a -> s { _mlmInputDataLocationS3 = a })

-- | The time of the most recent edit to the 'MLModel'. The time is expressed in
-- epoch time.
mlmLastUpdatedAt :: Lens' MLModel (Maybe UTCTime)
mlmLastUpdatedAt = lens _mlmLastUpdatedAt (\s a -> s { _mlmLastUpdatedAt = a }) . mapping _Time

-- | The ID assigned to the 'MLModel' at creation.
mlmMLModelId :: Lens' MLModel (Maybe Text)
mlmMLModelId = lens _mlmMLModelId (\s a -> s { _mlmMLModelId = a })

-- | Identifies the 'MLModel' category. The following are the available types:
--
-- REGRESSION - Produces a numeric result. For example, "What listing price
-- should a house have?". BINARY - Produces one of two possible results. For
-- example, "Is this a child-friendly web site?". MULTICLASS - Produces more
-- than two possible results. For example, "Is this a HIGH, LOW or MEDIUM risk
-- trade?".
mlmMLModelType :: Lens' MLModel (Maybe MLModelType)
mlmMLModelType = lens _mlmMLModelType (\s a -> s { _mlmMLModelType = a })

-- | A description of the most recent details about accessing the 'MLModel'.
mlmMessage :: Lens' MLModel (Maybe Text)
mlmMessage = lens _mlmMessage (\s a -> s { _mlmMessage = a })

-- | A user-supplied name or description of the 'MLModel'.
mlmName :: Lens' MLModel (Maybe Text)
mlmName = lens _mlmName (\s a -> s { _mlmName = a })

mlmScoreThreshold :: Lens' MLModel (Maybe Double)
mlmScoreThreshold =
    lens _mlmScoreThreshold (\s a -> s { _mlmScoreThreshold = a })

-- | The time of the most recent edit to the 'ScoreThreshold'. The time is expressed
-- in epoch time.
mlmScoreThresholdLastUpdatedAt :: Lens' MLModel (Maybe UTCTime)
mlmScoreThresholdLastUpdatedAt =
    lens _mlmScoreThresholdLastUpdatedAt
        (\s a -> s { _mlmScoreThresholdLastUpdatedAt = a })
            . mapping _Time

mlmSizeInBytes :: Lens' MLModel (Maybe Integer)
mlmSizeInBytes = lens _mlmSizeInBytes (\s a -> s { _mlmSizeInBytes = a })

-- | The current status of an 'MLModel'. This element can have one of the following
-- values:
--
-- PENDING - Amazon Machine Learning (Amazon ML) submitted a request to create
-- an 'MLModel'. INPROGRESS - The creation process is underway. FAILED - The
-- request to create an 'MLModel' did not run to completion. It is not usable. COMPLETED - The creation process completed successfully.
-- DELETED - The 'MLModel' is marked as deleted. It is not usable.
mlmStatus :: Lens' MLModel (Maybe EntityStatus)
mlmStatus = lens _mlmStatus (\s a -> s { _mlmStatus = a })

-- | The ID of the training 'DataSource'. The 'CreateMLModel' operation uses the 'TrainingDataSourceId'.
mlmTrainingDataSourceId :: Lens' MLModel (Maybe Text)
mlmTrainingDataSourceId =
    lens _mlmTrainingDataSourceId (\s a -> s { _mlmTrainingDataSourceId = a })

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
-- The valus is a double that ranges from 0 to MAX_DOUBLE. The default is not
-- to use L2 normalization. This cannot be used when 'L1' is specified. Use this
-- parameter sparingly.
--
-- 'sgd.maxPasses' - Number of times that the training process traverses the
-- observations to build the 'MLModel'. The value is an integer that ranges from 1
-- to 10000. The default value is 10.
--
-- 'sgd.maxMLModelSizeInBytes' - Maximum allowed size of the model. Depending on
-- the input data, the model size might affect performance.
--
-- The value is an integer that ranges from 100000 to 2147483648. The default
-- value is 33554432.
--
--
mlmTrainingParameters :: Lens' MLModel (HashMap Text Text)
mlmTrainingParameters =
    lens _mlmTrainingParameters (\s a -> s { _mlmTrainingParameters = a })
        . _Map

instance FromJSON MLModel where
    parseJSON = withObject "MLModel" $ \o -> MLModel
        <$> o .:? "Algorithm"
        <*> o .:? "CreatedAt"
        <*> o .:? "CreatedByIamUser"
        <*> o .:? "EndpointInfo"
        <*> o .:? "InputDataLocationS3"
        <*> o .:? "LastUpdatedAt"
        <*> o .:? "MLModelId"
        <*> o .:? "MLModelType"
        <*> o .:? "Message"
        <*> o .:? "Name"
        <*> o .:? "ScoreThreshold"
        <*> o .:? "ScoreThresholdLastUpdatedAt"
        <*> o .:? "SizeInBytes"
        <*> o .:? "Status"
        <*> o .:? "TrainingDataSourceId"
        <*> o .:? "TrainingParameters" .!= mempty

instance ToJSON MLModel where
    toJSON MLModel{..} = object
        [ "MLModelId"                   .= _mlmMLModelId
        , "TrainingDataSourceId"        .= _mlmTrainingDataSourceId
        , "CreatedByIamUser"            .= _mlmCreatedByIamUser
        , "CreatedAt"                   .= _mlmCreatedAt
        , "LastUpdatedAt"               .= _mlmLastUpdatedAt
        , "Name"                        .= _mlmName
        , "Status"                      .= _mlmStatus
        , "SizeInBytes"                 .= _mlmSizeInBytes
        , "EndpointInfo"                .= _mlmEndpointInfo
        , "TrainingParameters"          .= _mlmTrainingParameters
        , "InputDataLocationS3"         .= _mlmInputDataLocationS3
        , "Algorithm"                   .= _mlmAlgorithm
        , "MLModelType"                 .= _mlmMLModelType
        , "ScoreThreshold"              .= _mlmScoreThreshold
        , "ScoreThresholdLastUpdatedAt" .= _mlmScoreThresholdLastUpdatedAt
        , "Message"                     .= _mlmMessage
        ]

data BatchPrediction = BatchPrediction
    { _bpBatchPredictionDataSourceId :: Maybe Text
    , _bpBatchPredictionId           :: Maybe Text
    , _bpCreatedAt                   :: Maybe POSIX
    , _bpCreatedByIamUser            :: Maybe Text
    , _bpInputDataLocationS3         :: Maybe Text
    , _bpLastUpdatedAt               :: Maybe POSIX
    , _bpMLModelId                   :: Maybe Text
    , _bpMessage                     :: Maybe Text
    , _bpName                        :: Maybe Text
    , _bpOutputUri                   :: Maybe Text
    , _bpStatus                      :: Maybe EntityStatus
    } deriving (Eq, Read, Show)

-- | 'BatchPrediction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bpBatchPredictionDataSourceId' @::@ 'Maybe' 'Text'
--
-- * 'bpBatchPredictionId' @::@ 'Maybe' 'Text'
--
-- * 'bpCreatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'bpCreatedByIamUser' @::@ 'Maybe' 'Text'
--
-- * 'bpInputDataLocationS3' @::@ 'Maybe' 'Text'
--
-- * 'bpLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'bpMLModelId' @::@ 'Maybe' 'Text'
--
-- * 'bpMessage' @::@ 'Maybe' 'Text'
--
-- * 'bpName' @::@ 'Maybe' 'Text'
--
-- * 'bpOutputUri' @::@ 'Maybe' 'Text'
--
-- * 'bpStatus' @::@ 'Maybe' 'EntityStatus'
--
batchPrediction :: BatchPrediction
batchPrediction = BatchPrediction
    { _bpBatchPredictionId           = Nothing
    , _bpMLModelId                   = Nothing
    , _bpBatchPredictionDataSourceId = Nothing
    , _bpInputDataLocationS3         = Nothing
    , _bpCreatedByIamUser            = Nothing
    , _bpCreatedAt                   = Nothing
    , _bpLastUpdatedAt               = Nothing
    , _bpName                        = Nothing
    , _bpStatus                      = Nothing
    , _bpOutputUri                   = Nothing
    , _bpMessage                     = Nothing
    }

-- | The ID of the 'DataSource' that points to the group of observations to predict.
bpBatchPredictionDataSourceId :: Lens' BatchPrediction (Maybe Text)
bpBatchPredictionDataSourceId =
    lens _bpBatchPredictionDataSourceId
        (\s a -> s { _bpBatchPredictionDataSourceId = a })

-- | The ID assigned to the 'BatchPrediction' at creation. This value should be
-- identical to the value of the 'BatchPredictionID' in the request.
bpBatchPredictionId :: Lens' BatchPrediction (Maybe Text)
bpBatchPredictionId =
    lens _bpBatchPredictionId (\s a -> s { _bpBatchPredictionId = a })

-- | The time that the 'BatchPrediction' was created. The time is expressed in epoch
-- time.
bpCreatedAt :: Lens' BatchPrediction (Maybe UTCTime)
bpCreatedAt = lens _bpCreatedAt (\s a -> s { _bpCreatedAt = a }) . mapping _Time

-- | The AWS user account that invoked the 'BatchPrediction'. The account type can
-- be either an AWS root account or an AWS Identity and Access Management (IAM)
-- user account.
bpCreatedByIamUser :: Lens' BatchPrediction (Maybe Text)
bpCreatedByIamUser =
    lens _bpCreatedByIamUser (\s a -> s { _bpCreatedByIamUser = a })

-- | The location of the data file or directory in Amazon Simple Storage Service
-- (Amazon S3).
bpInputDataLocationS3 :: Lens' BatchPrediction (Maybe Text)
bpInputDataLocationS3 =
    lens _bpInputDataLocationS3 (\s a -> s { _bpInputDataLocationS3 = a })

-- | The time of the most recent edit to the 'BatchPrediction'. The time is
-- expressed in epoch time.
bpLastUpdatedAt :: Lens' BatchPrediction (Maybe UTCTime)
bpLastUpdatedAt = lens _bpLastUpdatedAt (\s a -> s { _bpLastUpdatedAt = a }) . mapping _Time

-- | The ID of the 'MLModel' that generated predictions for the 'BatchPrediction'
-- request.
bpMLModelId :: Lens' BatchPrediction (Maybe Text)
bpMLModelId = lens _bpMLModelId (\s a -> s { _bpMLModelId = a })

-- | A description of the most recent details about processing the batch
-- prediction request.
bpMessage :: Lens' BatchPrediction (Maybe Text)
bpMessage = lens _bpMessage (\s a -> s { _bpMessage = a })

-- | A user-supplied name or description of the 'BatchPrediction'.
bpName :: Lens' BatchPrediction (Maybe Text)
bpName = lens _bpName (\s a -> s { _bpName = a })

-- | The location of an Amazon S3 bucket or directory to receive the operation
-- results. The following substrings are not allowed in the s3 key portion of
-- the "outputURI" field: ':', '//', '/./', '/../'.
bpOutputUri :: Lens' BatchPrediction (Maybe Text)
bpOutputUri = lens _bpOutputUri (\s a -> s { _bpOutputUri = a })

-- | The status of the 'BatchPrediction'. This element can have one of the following
-- values:
--
-- 'PENDING' - Amazon Machine Learning (Amazon ML) submitted a request to
-- generate predictions for a batch of observations.  'INPROGRESS' - The process
-- is underway.  'FAILED' - The request to peform a batch prediction did not run
-- to completion. It is not usable.  'COMPLETED' - The batch prediction process
-- completed successfully.  'DELETED' - The 'BatchPrediction' is marked as deleted.
-- It is not usable.
bpStatus :: Lens' BatchPrediction (Maybe EntityStatus)
bpStatus = lens _bpStatus (\s a -> s { _bpStatus = a })

instance FromJSON BatchPrediction where
    parseJSON = withObject "BatchPrediction" $ \o -> BatchPrediction
        <$> o .:? "BatchPredictionDataSourceId"
        <*> o .:? "BatchPredictionId"
        <*> o .:? "CreatedAt"
        <*> o .:? "CreatedByIamUser"
        <*> o .:? "InputDataLocationS3"
        <*> o .:? "LastUpdatedAt"
        <*> o .:? "MLModelId"
        <*> o .:? "Message"
        <*> o .:? "Name"
        <*> o .:? "OutputUri"
        <*> o .:? "Status"

instance ToJSON BatchPrediction where
    toJSON BatchPrediction{..} = object
        [ "BatchPredictionId"           .= _bpBatchPredictionId
        , "MLModelId"                   .= _bpMLModelId
        , "BatchPredictionDataSourceId" .= _bpBatchPredictionDataSourceId
        , "InputDataLocationS3"         .= _bpInputDataLocationS3
        , "CreatedByIamUser"            .= _bpCreatedByIamUser
        , "CreatedAt"                   .= _bpCreatedAt
        , "LastUpdatedAt"               .= _bpLastUpdatedAt
        , "Name"                        .= _bpName
        , "Status"                      .= _bpStatus
        , "OutputUri"                   .= _bpOutputUri
        , "Message"                     .= _bpMessage
        ]

data SortOrder
    = Asc -- ^ asc
    | Dsc -- ^ dsc
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SortOrder

instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "asc" -> pure Asc
        "dsc" -> pure Dsc
        e     -> fail $
            "Failure parsing SortOrder from " ++ show e

instance ToText SortOrder where
    toText = \case
        Asc -> "asc"
        Dsc -> "dsc"

instance ToByteString SortOrder
instance ToHeader     SortOrder
instance ToQuery      SortOrder

instance FromJSON SortOrder where
    parseJSON = parseJSONText "SortOrder"

instance ToJSON SortOrder where
    toJSON = toJSONText

data Algorithm
    = Sgd -- ^ sgd
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable Algorithm

instance FromText Algorithm where
    parser = takeLowerText >>= \case
        "sgd" -> pure Sgd
        e     -> fail $
            "Failure parsing Algorithm from " ++ show e

instance ToText Algorithm where
    toText Sgd = "sgd"

instance ToByteString Algorithm
instance ToHeader     Algorithm
instance ToQuery      Algorithm

instance FromJSON Algorithm where
    parseJSON = parseJSONText "Algorithm"

instance ToJSON Algorithm where
    toJSON = toJSONText

data EntityStatus
    = ESCompleted  -- ^ COMPLETED
    | ESDeleted    -- ^ DELETED
    | ESFailed     -- ^ FAILED
    | ESInprogress -- ^ INPROGRESS
    | ESPending    -- ^ PENDING
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable EntityStatus

instance FromText EntityStatus where
    parser = takeLowerText >>= \case
        "completed"  -> pure ESCompleted
        "deleted"    -> pure ESDeleted
        "failed"     -> pure ESFailed
        "inprogress" -> pure ESInprogress
        "pending"    -> pure ESPending
        e            -> fail $
            "Failure parsing EntityStatus from " ++ show e

instance ToText EntityStatus where
    toText = \case
        ESCompleted  -> "COMPLETED"
        ESDeleted    -> "DELETED"
        ESFailed     -> "FAILED"
        ESInprogress -> "INPROGRESS"
        ESPending    -> "PENDING"

instance ToByteString EntityStatus
instance ToHeader     EntityStatus
instance ToQuery      EntityStatus

instance FromJSON EntityStatus where
    parseJSON = parseJSONText "EntityStatus"

instance ToJSON EntityStatus where
    toJSON = toJSONText

data DataSource = DataSource
    { _dsComputeStatistics :: Maybe Bool
    , _dsCreatedAt         :: Maybe POSIX
    , _dsCreatedByIamUser  :: Maybe Text
    , _dsDataLocationS3    :: Maybe Text
    , _dsDataRearrangement :: Maybe Text
    , _dsDataSizeInBytes   :: Maybe Integer
    , _dsDataSourceId      :: Maybe Text
    , _dsLastUpdatedAt     :: Maybe POSIX
    , _dsMessage           :: Maybe Text
    , _dsName              :: Maybe Text
    , _dsNumberOfFiles     :: Maybe Integer
    , _dsRDSMetadata       :: Maybe RDSMetadata
    , _dsRedshiftMetadata  :: Maybe RedshiftMetadata
    , _dsRoleARN           :: Maybe Text
    , _dsStatus            :: Maybe EntityStatus
    } deriving (Eq, Read, Show)

-- | 'DataSource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsComputeStatistics' @::@ 'Maybe' 'Bool'
--
-- * 'dsCreatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'dsCreatedByIamUser' @::@ 'Maybe' 'Text'
--
-- * 'dsDataLocationS3' @::@ 'Maybe' 'Text'
--
-- * 'dsDataRearrangement' @::@ 'Maybe' 'Text'
--
-- * 'dsDataSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dsDataSourceId' @::@ 'Maybe' 'Text'
--
-- * 'dsLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'dsMessage' @::@ 'Maybe' 'Text'
--
-- * 'dsName' @::@ 'Maybe' 'Text'
--
-- * 'dsNumberOfFiles' @::@ 'Maybe' 'Integer'
--
-- * 'dsRDSMetadata' @::@ 'Maybe' 'RDSMetadata'
--
-- * 'dsRedshiftMetadata' @::@ 'Maybe' 'RedshiftMetadata'
--
-- * 'dsRoleARN' @::@ 'Maybe' 'Text'
--
-- * 'dsStatus' @::@ 'Maybe' 'EntityStatus'
--
dataSource :: DataSource
dataSource = DataSource
    { _dsDataSourceId      = Nothing
    , _dsDataLocationS3    = Nothing
    , _dsDataRearrangement = Nothing
    , _dsCreatedByIamUser  = Nothing
    , _dsCreatedAt         = Nothing
    , _dsLastUpdatedAt     = Nothing
    , _dsDataSizeInBytes   = Nothing
    , _dsNumberOfFiles     = Nothing
    , _dsName              = Nothing
    , _dsStatus            = Nothing
    , _dsMessage           = Nothing
    , _dsRedshiftMetadata  = Nothing
    , _dsRDSMetadata       = Nothing
    , _dsRoleARN           = Nothing
    , _dsComputeStatistics = Nothing
    }

-- | The parameter is 'true' if statistics need to be generated from the
-- observation data.
dsComputeStatistics :: Lens' DataSource (Maybe Bool)
dsComputeStatistics =
    lens _dsComputeStatistics (\s a -> s { _dsComputeStatistics = a })

-- | The time that the 'DataSource' was created. The time is expressed in epoch time.
dsCreatedAt :: Lens' DataSource (Maybe UTCTime)
dsCreatedAt = lens _dsCreatedAt (\s a -> s { _dsCreatedAt = a }) . mapping _Time

-- | The AWS user account from which the 'DataSource' was created. The account type
-- can be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
dsCreatedByIamUser :: Lens' DataSource (Maybe Text)
dsCreatedByIamUser =
    lens _dsCreatedByIamUser (\s a -> s { _dsCreatedByIamUser = a })

-- | The location and name of the data in Amazon Simple Storage Service (Amazon
-- S3) that is used by a 'DataSource'.
dsDataLocationS3 :: Lens' DataSource (Maybe Text)
dsDataLocationS3 = lens _dsDataLocationS3 (\s a -> s { _dsDataLocationS3 = a })

-- | A JSON string that represents the splitting requirement of a 'Datasource'.
dsDataRearrangement :: Lens' DataSource (Maybe Text)
dsDataRearrangement =
    lens _dsDataRearrangement (\s a -> s { _dsDataRearrangement = a })

-- | The total number of observations contained in the data files that the 'DataSource' references.
dsDataSizeInBytes :: Lens' DataSource (Maybe Integer)
dsDataSizeInBytes =
    lens _dsDataSizeInBytes (\s a -> s { _dsDataSizeInBytes = a })

-- | The ID that is assigned to the 'DataSource' during creation.
dsDataSourceId :: Lens' DataSource (Maybe Text)
dsDataSourceId = lens _dsDataSourceId (\s a -> s { _dsDataSourceId = a })

-- | The time of the most recent edit to the 'BatchPrediction'. The time is
-- expressed in epoch time.
dsLastUpdatedAt :: Lens' DataSource (Maybe UTCTime)
dsLastUpdatedAt = lens _dsLastUpdatedAt (\s a -> s { _dsLastUpdatedAt = a }) . mapping _Time

-- | A description of the most recent details about creating the 'DataSource'.
dsMessage :: Lens' DataSource (Maybe Text)
dsMessage = lens _dsMessage (\s a -> s { _dsMessage = a })

-- | A user-supplied name or description of the 'DataSource'.
dsName :: Lens' DataSource (Maybe Text)
dsName = lens _dsName (\s a -> s { _dsName = a })

-- | The number of data files referenced by the 'DataSource'.
dsNumberOfFiles :: Lens' DataSource (Maybe Integer)
dsNumberOfFiles = lens _dsNumberOfFiles (\s a -> s { _dsNumberOfFiles = a })

dsRDSMetadata :: Lens' DataSource (Maybe RDSMetadata)
dsRDSMetadata = lens _dsRDSMetadata (\s a -> s { _dsRDSMetadata = a })

dsRedshiftMetadata :: Lens' DataSource (Maybe RedshiftMetadata)
dsRedshiftMetadata =
    lens _dsRedshiftMetadata (\s a -> s { _dsRedshiftMetadata = a })

dsRoleARN :: Lens' DataSource (Maybe Text)
dsRoleARN = lens _dsRoleARN (\s a -> s { _dsRoleARN = a })

-- | The current status of the 'DataSource'. This element can have one of the
-- following values:
--
-- PENDING - Amazon Machine Learning (Amazon ML) submitted a request to create
-- a 'DataSource'. INPROGRESS - The creation process is underway. FAILED - The
-- request to create a 'DataSource' did not run to completion. It is not usable. COMPLETED - The creation process completed successfully.
-- DELETED - The 'DataSource' is marked as deleted. It is not usable.
dsStatus :: Lens' DataSource (Maybe EntityStatus)
dsStatus = lens _dsStatus (\s a -> s { _dsStatus = a })

instance FromJSON DataSource where
    parseJSON = withObject "DataSource" $ \o -> DataSource
        <$> o .:? "ComputeStatistics"
        <*> o .:? "CreatedAt"
        <*> o .:? "CreatedByIamUser"
        <*> o .:? "DataLocationS3"
        <*> o .:? "DataRearrangement"
        <*> o .:? "DataSizeInBytes"
        <*> o .:? "DataSourceId"
        <*> o .:? "LastUpdatedAt"
        <*> o .:? "Message"
        <*> o .:? "Name"
        <*> o .:? "NumberOfFiles"
        <*> o .:? "RDSMetadata"
        <*> o .:? "RedshiftMetadata"
        <*> o .:? "RoleARN"
        <*> o .:? "Status"

instance ToJSON DataSource where
    toJSON DataSource{..} = object
        [ "DataSourceId"      .= _dsDataSourceId
        , "DataLocationS3"    .= _dsDataLocationS3
        , "DataRearrangement" .= _dsDataRearrangement
        , "CreatedByIamUser"  .= _dsCreatedByIamUser
        , "CreatedAt"         .= _dsCreatedAt
        , "LastUpdatedAt"     .= _dsLastUpdatedAt
        , "DataSizeInBytes"   .= _dsDataSizeInBytes
        , "NumberOfFiles"     .= _dsNumberOfFiles
        , "Name"              .= _dsName
        , "Status"            .= _dsStatus
        , "Message"           .= _dsMessage
        , "RedshiftMetadata"  .= _dsRedshiftMetadata
        , "RDSMetadata"       .= _dsRDSMetadata
        , "RoleARN"           .= _dsRoleARN
        , "ComputeStatistics" .= _dsComputeStatistics
        ]

data RDSDatabase = RDSDatabase
    { _rdsdDatabaseName       :: Text
    , _rdsdInstanceIdentifier :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RDSDatabase' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdsdDatabaseName' @::@ 'Text'
--
-- * 'rdsdInstanceIdentifier' @::@ 'Text'
--
rdsdatabase :: Text -- ^ 'rdsdInstanceIdentifier'
            -> Text -- ^ 'rdsdDatabaseName'
            -> RDSDatabase
rdsdatabase p1 p2 = RDSDatabase
    { _rdsdInstanceIdentifier = p1
    , _rdsdDatabaseName       = p2
    }

rdsdDatabaseName :: Lens' RDSDatabase Text
rdsdDatabaseName = lens _rdsdDatabaseName (\s a -> s { _rdsdDatabaseName = a })

-- | The ID of an RDS DB instance.
rdsdInstanceIdentifier :: Lens' RDSDatabase Text
rdsdInstanceIdentifier =
    lens _rdsdInstanceIdentifier (\s a -> s { _rdsdInstanceIdentifier = a })

instance FromJSON RDSDatabase where
    parseJSON = withObject "RDSDatabase" $ \o -> RDSDatabase
        <$> o .:  "DatabaseName"
        <*> o .:  "InstanceIdentifier"

instance ToJSON RDSDatabase where
    toJSON RDSDatabase{..} = object
        [ "InstanceIdentifier" .= _rdsdInstanceIdentifier
        , "DatabaseName"       .= _rdsdDatabaseName
        ]

data RDSDatabaseCredentials = RDSDatabaseCredentials
    { _rdsdcPassword :: Text
    , _rdsdcUsername :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RDSDatabaseCredentials' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdsdcPassword' @::@ 'Text'
--
-- * 'rdsdcUsername' @::@ 'Text'
--
rdsdatabaseCredentials :: Text -- ^ 'rdsdcUsername'
                       -> Text -- ^ 'rdsdcPassword'
                       -> RDSDatabaseCredentials
rdsdatabaseCredentials p1 p2 = RDSDatabaseCredentials
    { _rdsdcUsername = p1
    , _rdsdcPassword = p2
    }

rdsdcPassword :: Lens' RDSDatabaseCredentials Text
rdsdcPassword = lens _rdsdcPassword (\s a -> s { _rdsdcPassword = a })

rdsdcUsername :: Lens' RDSDatabaseCredentials Text
rdsdcUsername = lens _rdsdcUsername (\s a -> s { _rdsdcUsername = a })

instance FromJSON RDSDatabaseCredentials where
    parseJSON = withObject "RDSDatabaseCredentials" $ \o -> RDSDatabaseCredentials
        <$> o .:  "Password"
        <*> o .:  "Username"

instance ToJSON RDSDatabaseCredentials where
    toJSON RDSDatabaseCredentials{..} = object
        [ "Username" .= _rdsdcUsername
        , "Password" .= _rdsdcPassword
        ]

data MLModelFilterVariable
    = MLMFVAlgorithm              -- ^ Algorithm
    | MLMFVCreatedAt              -- ^ CreatedAt
    | MLMFVIAMUser                -- ^ IAMUser
    | MLMFVLastUpdatedAt          -- ^ LastUpdatedAt
    | MLMFVMLModelType            -- ^ MLModelType
    | MLMFVName                   -- ^ Name
    | MLMFVRealtimeEndpointStatus -- ^ RealtimeEndpointStatus
    | MLMFVStatus                 -- ^ Status
    | MLMFVTrainingDataSourceId   -- ^ TrainingDataSourceId
    | MLMFVTrainingDataURI        -- ^ TrainingDataURI
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable MLModelFilterVariable

instance FromText MLModelFilterVariable where
    parser = takeLowerText >>= \case
        "algorithm"              -> pure MLMFVAlgorithm
        "createdat"              -> pure MLMFVCreatedAt
        "iamuser"                -> pure MLMFVIAMUser
        "lastupdatedat"          -> pure MLMFVLastUpdatedAt
        "mlmodeltype"            -> pure MLMFVMLModelType
        "name"                   -> pure MLMFVName
        "realtimeendpointstatus" -> pure MLMFVRealtimeEndpointStatus
        "status"                 -> pure MLMFVStatus
        "trainingdatasourceid"   -> pure MLMFVTrainingDataSourceId
        "trainingdatauri"        -> pure MLMFVTrainingDataURI
        e                        -> fail $
            "Failure parsing MLModelFilterVariable from " ++ show e

instance ToText MLModelFilterVariable where
    toText = \case
        MLMFVAlgorithm              -> "Algorithm"
        MLMFVCreatedAt              -> "CreatedAt"
        MLMFVIAMUser                -> "IAMUser"
        MLMFVLastUpdatedAt          -> "LastUpdatedAt"
        MLMFVMLModelType            -> "MLModelType"
        MLMFVName                   -> "Name"
        MLMFVRealtimeEndpointStatus -> "RealtimeEndpointStatus"
        MLMFVStatus                 -> "Status"
        MLMFVTrainingDataSourceId   -> "TrainingDataSourceId"
        MLMFVTrainingDataURI        -> "TrainingDataURI"

instance ToByteString MLModelFilterVariable
instance ToHeader     MLModelFilterVariable
instance ToQuery      MLModelFilterVariable

instance FromJSON MLModelFilterVariable where
    parseJSON = parseJSONText "MLModelFilterVariable"

instance ToJSON MLModelFilterVariable where
    toJSON = toJSONText

data DataSourceFilterVariable
    = DSFVCreatedAt      -- ^ CreatedAt
    | DSFVDataLocationS3 -- ^ DataLocationS3
    | DSFVIAMUser        -- ^ IAMUser
    | DSFVLastUpdatedAt  -- ^ LastUpdatedAt
    | DSFVName           -- ^ Name
    | DSFVStatus         -- ^ Status
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DataSourceFilterVariable

instance FromText DataSourceFilterVariable where
    parser = takeLowerText >>= \case
        "createdat"      -> pure DSFVCreatedAt
        "datalocations3" -> pure DSFVDataLocationS3
        "iamuser"        -> pure DSFVIAMUser
        "lastupdatedat"  -> pure DSFVLastUpdatedAt
        "name"           -> pure DSFVName
        "status"         -> pure DSFVStatus
        e                -> fail $
            "Failure parsing DataSourceFilterVariable from " ++ show e

instance ToText DataSourceFilterVariable where
    toText = \case
        DSFVCreatedAt      -> "CreatedAt"
        DSFVDataLocationS3 -> "DataLocationS3"
        DSFVIAMUser        -> "IAMUser"
        DSFVLastUpdatedAt  -> "LastUpdatedAt"
        DSFVName           -> "Name"
        DSFVStatus         -> "Status"

instance ToByteString DataSourceFilterVariable
instance ToHeader     DataSourceFilterVariable
instance ToQuery      DataSourceFilterVariable

instance FromJSON DataSourceFilterVariable where
    parseJSON = parseJSONText "DataSourceFilterVariable"

instance ToJSON DataSourceFilterVariable where
    toJSON = toJSONText

data RedshiftDataSpec = RedshiftDataSpec
    { _rdsDataRearrangement   :: Maybe Text
    , _rdsDataSchema          :: Maybe Text
    , _rdsDataSchemaUri       :: Maybe Text
    , _rdsDatabaseCredentials :: RedshiftDatabaseCredentials
    , _rdsDatabaseInformation :: RedshiftDatabase
    , _rdsS3StagingLocation   :: Text
    , _rdsSelectSqlQuery      :: Text
    } deriving (Eq, Read, Show)

-- | 'RedshiftDataSpec' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdsDataRearrangement' @::@ 'Maybe' 'Text'
--
-- * 'rdsDataSchema' @::@ 'Maybe' 'Text'
--
-- * 'rdsDataSchemaUri' @::@ 'Maybe' 'Text'
--
-- * 'rdsDatabaseCredentials' @::@ 'RedshiftDatabaseCredentials'
--
-- * 'rdsDatabaseInformation' @::@ 'RedshiftDatabase'
--
-- * 'rdsS3StagingLocation' @::@ 'Text'
--
-- * 'rdsSelectSqlQuery' @::@ 'Text'
--
redshiftDataSpec :: RedshiftDatabase -- ^ 'rdsDatabaseInformation'
                 -> Text -- ^ 'rdsSelectSqlQuery'
                 -> RedshiftDatabaseCredentials -- ^ 'rdsDatabaseCredentials'
                 -> Text -- ^ 'rdsS3StagingLocation'
                 -> RedshiftDataSpec
redshiftDataSpec p1 p2 p3 p4 = RedshiftDataSpec
    { _rdsDatabaseInformation = p1
    , _rdsSelectSqlQuery      = p2
    , _rdsDatabaseCredentials = p3
    , _rdsS3StagingLocation   = p4
    , _rdsDataRearrangement   = Nothing
    , _rdsDataSchema          = Nothing
    , _rdsDataSchemaUri       = Nothing
    }

-- | Describes the splitting specifications for a 'DataSource'.
rdsDataRearrangement :: Lens' RedshiftDataSpec (Maybe Text)
rdsDataRearrangement =
    lens _rdsDataRearrangement (\s a -> s { _rdsDataRearrangement = a })

-- | Describes the schema for an Amazon Redshift 'DataSource'.
rdsDataSchema :: Lens' RedshiftDataSpec (Maybe Text)
rdsDataSchema = lens _rdsDataSchema (\s a -> s { _rdsDataSchema = a })

-- | Describes the schema location for an Amazon Redshift 'DataSource'.
rdsDataSchemaUri :: Lens' RedshiftDataSpec (Maybe Text)
rdsDataSchemaUri = lens _rdsDataSchemaUri (\s a -> s { _rdsDataSchemaUri = a })

-- | Describes AWS Identity and Access Management (IAM) credentials that are used
-- connect to the Amazon Redshift database.
rdsDatabaseCredentials :: Lens' RedshiftDataSpec RedshiftDatabaseCredentials
rdsDatabaseCredentials =
    lens _rdsDatabaseCredentials (\s a -> s { _rdsDatabaseCredentials = a })

-- | Describes the 'DatabaseName' and 'ClusterIdentifier' for an Amazon Redshift 'DataSource'.
rdsDatabaseInformation :: Lens' RedshiftDataSpec RedshiftDatabase
rdsDatabaseInformation =
    lens _rdsDatabaseInformation (\s a -> s { _rdsDatabaseInformation = a })

-- | Describes an Amazon S3 location to store the result set of the 'SelectSqlQuery'
-- query.
rdsS3StagingLocation :: Lens' RedshiftDataSpec Text
rdsS3StagingLocation =
    lens _rdsS3StagingLocation (\s a -> s { _rdsS3StagingLocation = a })

-- | Describes the SQL Query to execute on an Amazon Redshift database for an
-- Amazon Redshift 'DataSource'.
rdsSelectSqlQuery :: Lens' RedshiftDataSpec Text
rdsSelectSqlQuery =
    lens _rdsSelectSqlQuery (\s a -> s { _rdsSelectSqlQuery = a })

instance FromJSON RedshiftDataSpec where
    parseJSON = withObject "RedshiftDataSpec" $ \o -> RedshiftDataSpec
        <$> o .:? "DataRearrangement"
        <*> o .:? "DataSchema"
        <*> o .:? "DataSchemaUri"
        <*> o .:  "DatabaseCredentials"
        <*> o .:  "DatabaseInformation"
        <*> o .:  "S3StagingLocation"
        <*> o .:  "SelectSqlQuery"

instance ToJSON RedshiftDataSpec where
    toJSON RedshiftDataSpec{..} = object
        [ "DatabaseInformation" .= _rdsDatabaseInformation
        , "SelectSqlQuery"      .= _rdsSelectSqlQuery
        , "DatabaseCredentials" .= _rdsDatabaseCredentials
        , "S3StagingLocation"   .= _rdsS3StagingLocation
        , "DataRearrangement"   .= _rdsDataRearrangement
        , "DataSchema"          .= _rdsDataSchema
        , "DataSchemaUri"       .= _rdsDataSchemaUri
        ]

data Evaluation = Evaluation
    { _eCreatedAt              :: Maybe POSIX
    , _eCreatedByIamUser       :: Maybe Text
    , _eEvaluationDataSourceId :: Maybe Text
    , _eEvaluationId           :: Maybe Text
    , _eInputDataLocationS3    :: Maybe Text
    , _eLastUpdatedAt          :: Maybe POSIX
    , _eMLModelId              :: Maybe Text
    , _eMessage                :: Maybe Text
    , _eName                   :: Maybe Text
    , _ePerformanceMetrics     :: Maybe PerformanceMetrics
    , _eStatus                 :: Maybe EntityStatus
    } deriving (Eq, Read, Show)

-- | 'Evaluation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eCreatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'eCreatedByIamUser' @::@ 'Maybe' 'Text'
--
-- * 'eEvaluationDataSourceId' @::@ 'Maybe' 'Text'
--
-- * 'eEvaluationId' @::@ 'Maybe' 'Text'
--
-- * 'eInputDataLocationS3' @::@ 'Maybe' 'Text'
--
-- * 'eLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'eMLModelId' @::@ 'Maybe' 'Text'
--
-- * 'eMessage' @::@ 'Maybe' 'Text'
--
-- * 'eName' @::@ 'Maybe' 'Text'
--
-- * 'ePerformanceMetrics' @::@ 'Maybe' 'PerformanceMetrics'
--
-- * 'eStatus' @::@ 'Maybe' 'EntityStatus'
--
evaluation :: Evaluation
evaluation = Evaluation
    { _eEvaluationId           = Nothing
    , _eMLModelId              = Nothing
    , _eEvaluationDataSourceId = Nothing
    , _eInputDataLocationS3    = Nothing
    , _eCreatedByIamUser       = Nothing
    , _eCreatedAt              = Nothing
    , _eLastUpdatedAt          = Nothing
    , _eName                   = Nothing
    , _eStatus                 = Nothing
    , _ePerformanceMetrics     = Nothing
    , _eMessage                = Nothing
    }

-- | The time that the 'Evaluation' was created. The time is expressed in epoch time.
eCreatedAt :: Lens' Evaluation (Maybe UTCTime)
eCreatedAt = lens _eCreatedAt (\s a -> s { _eCreatedAt = a }) . mapping _Time

-- | The AWS user account that invoked the evaluation. The account type can be
-- either an AWS root account or an AWS Identity and Access Management (IAM)
-- user account.
eCreatedByIamUser :: Lens' Evaluation (Maybe Text)
eCreatedByIamUser =
    lens _eCreatedByIamUser (\s a -> s { _eCreatedByIamUser = a })

-- | The ID of the 'DataSource' that is used to evaluate the 'MLModel'.
eEvaluationDataSourceId :: Lens' Evaluation (Maybe Text)
eEvaluationDataSourceId =
    lens _eEvaluationDataSourceId (\s a -> s { _eEvaluationDataSourceId = a })

-- | The ID that is assigned to the 'Evaluation' at creation.
eEvaluationId :: Lens' Evaluation (Maybe Text)
eEvaluationId = lens _eEvaluationId (\s a -> s { _eEvaluationId = a })

-- | The location and name of the data in Amazon Simple Storage Server (Amazon S3)
-- that is used in the evaluation.
eInputDataLocationS3 :: Lens' Evaluation (Maybe Text)
eInputDataLocationS3 =
    lens _eInputDataLocationS3 (\s a -> s { _eInputDataLocationS3 = a })

-- | The time of the most recent edit to the 'Evaluation'. The time is expressed in
-- epoch time.
eLastUpdatedAt :: Lens' Evaluation (Maybe UTCTime)
eLastUpdatedAt = lens _eLastUpdatedAt (\s a -> s { _eLastUpdatedAt = a }) . mapping _Time

-- | The ID of the 'MLModel' that is the focus of the evaluation.
eMLModelId :: Lens' Evaluation (Maybe Text)
eMLModelId = lens _eMLModelId (\s a -> s { _eMLModelId = a })

-- | A description of the most recent details about evaluating the 'MLModel'.
eMessage :: Lens' Evaluation (Maybe Text)
eMessage = lens _eMessage (\s a -> s { _eMessage = a })

-- | A user-supplied name or description of the 'Evaluation'.
eName :: Lens' Evaluation (Maybe Text)
eName = lens _eName (\s a -> s { _eName = a })

-- | Measurements of how well the 'MLModel' performed, using observations referenced
-- by the 'DataSource'. One of the following metrics is returned, based on the
-- type of the MLModel:
--
-- BinaryAUC: A binary 'MLModel' uses the Area Under the Curve (AUC) technique
-- to measure performance.
--
-- RegressionRMSE: A regression 'MLModel' uses the Root Mean Square Error
-- (RMSE) technique to measure performance. RMSE measures the difference between
-- predicted and actual values for a single variable.
--
-- MulticlassAvgFScore: A multiclass 'MLModel' uses the F1 score technique to
-- measure performance.
--
-- For more information about performance metrics, please see the <http://docs.aws.amazon.com/machine-learning/latest/dg AmazonMachine Learning Developer Guide>.
ePerformanceMetrics :: Lens' Evaluation (Maybe PerformanceMetrics)
ePerformanceMetrics =
    lens _ePerformanceMetrics (\s a -> s { _ePerformanceMetrics = a })

-- | The status of the evaluation. This element can have one of the following
-- values:
--
-- 'PENDING' - Amazon Machine Learning (Amazon ML) submitted a request to
-- evaluate an 'MLModel'.  'INPROGRESS' - The evaluation is underway.  'FAILED' - The
-- request to evaluate an 'MLModel' did not run to completion. It is not usable.  'COMPLETED' - The evaluation process completed successfully.  'DELETED' - The 'Evaluation'
-- is marked as deleted. It is not usable.
eStatus :: Lens' Evaluation (Maybe EntityStatus)
eStatus = lens _eStatus (\s a -> s { _eStatus = a })

instance FromJSON Evaluation where
    parseJSON = withObject "Evaluation" $ \o -> Evaluation
        <$> o .:? "CreatedAt"
        <*> o .:? "CreatedByIamUser"
        <*> o .:? "EvaluationDataSourceId"
        <*> o .:? "EvaluationId"
        <*> o .:? "InputDataLocationS3"
        <*> o .:? "LastUpdatedAt"
        <*> o .:? "MLModelId"
        <*> o .:? "Message"
        <*> o .:? "Name"
        <*> o .:? "PerformanceMetrics"
        <*> o .:? "Status"

instance ToJSON Evaluation where
    toJSON Evaluation{..} = object
        [ "EvaluationId"           .= _eEvaluationId
        , "MLModelId"              .= _eMLModelId
        , "EvaluationDataSourceId" .= _eEvaluationDataSourceId
        , "InputDataLocationS3"    .= _eInputDataLocationS3
        , "CreatedByIamUser"       .= _eCreatedByIamUser
        , "CreatedAt"              .= _eCreatedAt
        , "LastUpdatedAt"          .= _eLastUpdatedAt
        , "Name"                   .= _eName
        , "Status"                 .= _eStatus
        , "PerformanceMetrics"     .= _ePerformanceMetrics
        , "Message"                .= _eMessage
        ]

data RedshiftMetadata = RedshiftMetadata
    { _rmDatabaseUserName :: Maybe Text
    , _rmRedshiftDatabase :: Maybe RedshiftDatabase
    , _rmSelectSqlQuery   :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'RedshiftMetadata' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmDatabaseUserName' @::@ 'Maybe' 'Text'
--
-- * 'rmRedshiftDatabase' @::@ 'Maybe' 'RedshiftDatabase'
--
-- * 'rmSelectSqlQuery' @::@ 'Maybe' 'Text'
--
redshiftMetadata :: RedshiftMetadata
redshiftMetadata = RedshiftMetadata
    { _rmRedshiftDatabase = Nothing
    , _rmDatabaseUserName = Nothing
    , _rmSelectSqlQuery   = Nothing
    }

rmDatabaseUserName :: Lens' RedshiftMetadata (Maybe Text)
rmDatabaseUserName =
    lens _rmDatabaseUserName (\s a -> s { _rmDatabaseUserName = a })

rmRedshiftDatabase :: Lens' RedshiftMetadata (Maybe RedshiftDatabase)
rmRedshiftDatabase =
    lens _rmRedshiftDatabase (\s a -> s { _rmRedshiftDatabase = a })

-- | The SQL query that is specified during 'CreateDataSourceFromRedshift'. Returns
-- only if 'Verbose' is true in GetDataSourceInput.
rmSelectSqlQuery :: Lens' RedshiftMetadata (Maybe Text)
rmSelectSqlQuery = lens _rmSelectSqlQuery (\s a -> s { _rmSelectSqlQuery = a })

instance FromJSON RedshiftMetadata where
    parseJSON = withObject "RedshiftMetadata" $ \o -> RedshiftMetadata
        <$> o .:? "DatabaseUserName"
        <*> o .:? "RedshiftDatabase"
        <*> o .:? "SelectSqlQuery"

instance ToJSON RedshiftMetadata where
    toJSON RedshiftMetadata{..} = object
        [ "RedshiftDatabase" .= _rmRedshiftDatabase
        , "DatabaseUserName" .= _rmDatabaseUserName
        , "SelectSqlQuery"   .= _rmSelectSqlQuery
        ]

data MLModelType
    = Binary     -- ^ BINARY
    | Multiclass -- ^ MULTICLASS
    | Regression -- ^ REGRESSION
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable MLModelType

instance FromText MLModelType where
    parser = takeLowerText >>= \case
        "binary"     -> pure Binary
        "multiclass" -> pure Multiclass
        "regression" -> pure Regression
        e            -> fail $
            "Failure parsing MLModelType from " ++ show e

instance ToText MLModelType where
    toText = \case
        Binary     -> "BINARY"
        Multiclass -> "MULTICLASS"
        Regression -> "REGRESSION"

instance ToByteString MLModelType
instance ToHeader     MLModelType
instance ToQuery      MLModelType

instance FromJSON MLModelType where
    parseJSON = parseJSONText "MLModelType"

instance ToJSON MLModelType where
    toJSON = toJSONText
