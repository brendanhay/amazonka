{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.Sum where

import Network.AWS.Prelude

-- | The function used to train an @MLModel@ . Training choices supported by Amazon ML include the following:
--
--
--     * @SGD@ - Stochastic Gradient Descent.    * @RandomForest@ - Random forest of decision trees.
--
data Algorithm =
  SGD
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Algorithm where
    parser = takeLowerText >>= \case
        "sgd" -> pure SGD
        e -> fromTextError $ "Failure parsing Algorithm from value: '" <> e
           <> "'. Accepted values: sgd"

instance ToText Algorithm where
    toText = \case
        SGD -> "sgd"

instance Hashable     Algorithm
instance NFData       Algorithm
instance ToByteString Algorithm
instance ToQuery      Algorithm
instance ToHeader     Algorithm

instance FromJSON Algorithm where
    parseJSON = parseJSONText "Algorithm"

-- | A list of the variables to use in searching or filtering @BatchPrediction@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @BatchPrediction@ creation date.    * @Status@ - Sets the search criteria to @BatchPrediction@ status.    * @Name@ - Sets the search criteria to the contents of @BatchPrediction@ ____ @Name@ .    * @IAMUser@ - Sets the search criteria to the user account that invoked the @BatchPrediction@ creation.    * @MLModelId@ - Sets the search criteria to the @MLModel@ used in the @BatchPrediction@ .    * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in the @BatchPrediction@ .    * @DataURI@ - Sets the search criteria to the data file(s) used in the @BatchPrediction@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
data BatchPredictionFilterVariable
  = BatchCreatedAt
  | BatchDataSourceId
  | BatchDataURI
  | BatchIAMUser
  | BatchLastUpdatedAt
  | BatchMLModelId
  | BatchName
  | BatchStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BatchPredictionFilterVariable where
    parser = takeLowerText >>= \case
        "createdat" -> pure BatchCreatedAt
        "datasourceid" -> pure BatchDataSourceId
        "datauri" -> pure BatchDataURI
        "iamuser" -> pure BatchIAMUser
        "lastupdatedat" -> pure BatchLastUpdatedAt
        "mlmodelid" -> pure BatchMLModelId
        "name" -> pure BatchName
        "status" -> pure BatchStatus
        e -> fromTextError $ "Failure parsing BatchPredictionFilterVariable from value: '" <> e
           <> "'. Accepted values: createdat, datasourceid, datauri, iamuser, lastupdatedat, mlmodelid, name, status"

instance ToText BatchPredictionFilterVariable where
    toText = \case
        BatchCreatedAt -> "CreatedAt"
        BatchDataSourceId -> "DataSourceId"
        BatchDataURI -> "DataURI"
        BatchIAMUser -> "IAMUser"
        BatchLastUpdatedAt -> "LastUpdatedAt"
        BatchMLModelId -> "MLModelId"
        BatchName -> "Name"
        BatchStatus -> "Status"

instance Hashable     BatchPredictionFilterVariable
instance NFData       BatchPredictionFilterVariable
instance ToByteString BatchPredictionFilterVariable
instance ToQuery      BatchPredictionFilterVariable
instance ToHeader     BatchPredictionFilterVariable

instance ToJSON BatchPredictionFilterVariable where
    toJSON = toJSONText

-- | A list of the variables to use in searching or filtering @DataSource@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @DataSource@ creation date.    * @Status@ - Sets the search criteria to @DataSource@ status.    * @Name@ - Sets the search criteria to the contents of @DataSource@ ____ @Name@ .    * @DataUri@ - Sets the search criteria to the URI of data files used to create the @DataSource@ . The URI can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.    * @IAMUser@ - Sets the search criteria to the user account that invoked the @DataSource@ creation.
--
data DataSourceFilterVariable
  = DataCreatedAt
  | DataDATALOCATIONS3
  | DataIAMUser
  | DataLastUpdatedAt
  | DataName
  | DataStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DataSourceFilterVariable where
    parser = takeLowerText >>= \case
        "createdat" -> pure DataCreatedAt
        "datalocations3" -> pure DataDATALOCATIONS3
        "iamuser" -> pure DataIAMUser
        "lastupdatedat" -> pure DataLastUpdatedAt
        "name" -> pure DataName
        "status" -> pure DataStatus
        e -> fromTextError $ "Failure parsing DataSourceFilterVariable from value: '" <> e
           <> "'. Accepted values: createdat, datalocations3, iamuser, lastupdatedat, name, status"

instance ToText DataSourceFilterVariable where
    toText = \case
        DataCreatedAt -> "CreatedAt"
        DataDATALOCATIONS3 -> "DataLocationS3"
        DataIAMUser -> "IAMUser"
        DataLastUpdatedAt -> "LastUpdatedAt"
        DataName -> "Name"
        DataStatus -> "Status"

instance Hashable     DataSourceFilterVariable
instance NFData       DataSourceFilterVariable
instance ToByteString DataSourceFilterVariable
instance ToQuery      DataSourceFilterVariable
instance ToHeader     DataSourceFilterVariable

instance ToJSON DataSourceFilterVariable where
    toJSON = toJSONText

-- | Contains the key values of @DetailsMap@ : @PredictiveModelType@ - Indicates the type of the @MLModel@ . @Algorithm@ - Indicates the algorithm that was used for the @MLModel@ .
data DetailsAttributes
  = Algorithm
  | PredictiveModelType
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DetailsAttributes where
    parser = takeLowerText >>= \case
        "algorithm" -> pure Algorithm
        "predictivemodeltype" -> pure PredictiveModelType
        e -> fromTextError $ "Failure parsing DetailsAttributes from value: '" <> e
           <> "'. Accepted values: algorithm, predictivemodeltype"

instance ToText DetailsAttributes where
    toText = \case
        Algorithm -> "Algorithm"
        PredictiveModelType -> "PredictiveModelType"

instance Hashable     DetailsAttributes
instance NFData       DetailsAttributes
instance ToByteString DetailsAttributes
instance ToQuery      DetailsAttributes
instance ToHeader     DetailsAttributes

instance FromJSON DetailsAttributes where
    parseJSON = parseJSONText "DetailsAttributes"

-- | Object status with the following possible values:
--
--
--     * @PENDING@     * @INPROGRESS@     * @FAILED@     * @COMPLETED@     * @DELETED@
--
data EntityStatus
  = ESCompleted
  | ESDeleted
  | ESFailed
  | ESInprogress
  | ESPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EntityStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure ESCompleted
        "deleted" -> pure ESDeleted
        "failed" -> pure ESFailed
        "inprogress" -> pure ESInprogress
        "pending" -> pure ESPending
        e -> fromTextError $ "Failure parsing EntityStatus from value: '" <> e
           <> "'. Accepted values: completed, deleted, failed, inprogress, pending"

instance ToText EntityStatus where
    toText = \case
        ESCompleted -> "COMPLETED"
        ESDeleted -> "DELETED"
        ESFailed -> "FAILED"
        ESInprogress -> "INPROGRESS"
        ESPending -> "PENDING"

instance Hashable     EntityStatus
instance NFData       EntityStatus
instance ToByteString EntityStatus
instance ToQuery      EntityStatus
instance ToHeader     EntityStatus

instance FromJSON EntityStatus where
    parseJSON = parseJSONText "EntityStatus"

-- | A list of the variables to use in searching or filtering @Evaluation@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @Evaluation@ creation date.    * @Status@ - Sets the search criteria to @Evaluation@ status.    * @Name@ - Sets the search criteria to the contents of @Evaluation@ ____ @Name@ .    * @IAMUser@ - Sets the search criteria to the user account that invoked an evaluation.    * @MLModelId@ - Sets the search criteria to the @Predictor@ that was evaluated.    * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in evaluation.    * @DataUri@ - Sets the search criteria to the data file(s) used in evaluation. The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
data EvaluationFilterVariable
  = EvalCreatedAt
  | EvalDataSourceId
  | EvalDataURI
  | EvalIAMUser
  | EvalLastUpdatedAt
  | EvalMLModelId
  | EvalName
  | EvalStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EvaluationFilterVariable where
    parser = takeLowerText >>= \case
        "createdat" -> pure EvalCreatedAt
        "datasourceid" -> pure EvalDataSourceId
        "datauri" -> pure EvalDataURI
        "iamuser" -> pure EvalIAMUser
        "lastupdatedat" -> pure EvalLastUpdatedAt
        "mlmodelid" -> pure EvalMLModelId
        "name" -> pure EvalName
        "status" -> pure EvalStatus
        e -> fromTextError $ "Failure parsing EvaluationFilterVariable from value: '" <> e
           <> "'. Accepted values: createdat, datasourceid, datauri, iamuser, lastupdatedat, mlmodelid, name, status"

instance ToText EvaluationFilterVariable where
    toText = \case
        EvalCreatedAt -> "CreatedAt"
        EvalDataSourceId -> "DataSourceId"
        EvalDataURI -> "DataURI"
        EvalIAMUser -> "IAMUser"
        EvalLastUpdatedAt -> "LastUpdatedAt"
        EvalMLModelId -> "MLModelId"
        EvalName -> "Name"
        EvalStatus -> "Status"

instance Hashable     EvaluationFilterVariable
instance NFData       EvaluationFilterVariable
instance ToByteString EvaluationFilterVariable
instance ToQuery      EvaluationFilterVariable
instance ToHeader     EvaluationFilterVariable

instance ToJSON EvaluationFilterVariable where
    toJSON = toJSONText

data MLModelFilterVariable
  = MLMFVAlgorithm
  | MLMFVCreatedAt
  | MLMFVIAMUser
  | MLMFVLastUpdatedAt
  | MLMFVMLModelType
  | MLMFVName
  | MLMFVRealtimeEndpointStatus
  | MLMFVStatus
  | MLMFVTrainingDataSourceId
  | MLMFVTrainingDataURI
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MLModelFilterVariable where
    parser = takeLowerText >>= \case
        "algorithm" -> pure MLMFVAlgorithm
        "createdat" -> pure MLMFVCreatedAt
        "iamuser" -> pure MLMFVIAMUser
        "lastupdatedat" -> pure MLMFVLastUpdatedAt
        "mlmodeltype" -> pure MLMFVMLModelType
        "name" -> pure MLMFVName
        "realtimeendpointstatus" -> pure MLMFVRealtimeEndpointStatus
        "status" -> pure MLMFVStatus
        "trainingdatasourceid" -> pure MLMFVTrainingDataSourceId
        "trainingdatauri" -> pure MLMFVTrainingDataURI
        e -> fromTextError $ "Failure parsing MLModelFilterVariable from value: '" <> e
           <> "'. Accepted values: algorithm, createdat, iamuser, lastupdatedat, mlmodeltype, name, realtimeendpointstatus, status, trainingdatasourceid, trainingdatauri"

instance ToText MLModelFilterVariable where
    toText = \case
        MLMFVAlgorithm -> "Algorithm"
        MLMFVCreatedAt -> "CreatedAt"
        MLMFVIAMUser -> "IAMUser"
        MLMFVLastUpdatedAt -> "LastUpdatedAt"
        MLMFVMLModelType -> "MLModelType"
        MLMFVName -> "Name"
        MLMFVRealtimeEndpointStatus -> "RealtimeEndpointStatus"
        MLMFVStatus -> "Status"
        MLMFVTrainingDataSourceId -> "TrainingDataSourceId"
        MLMFVTrainingDataURI -> "TrainingDataURI"

instance Hashable     MLModelFilterVariable
instance NFData       MLModelFilterVariable
instance ToByteString MLModelFilterVariable
instance ToQuery      MLModelFilterVariable
instance ToHeader     MLModelFilterVariable

instance ToJSON MLModelFilterVariable where
    toJSON = toJSONText

data MLModelType
  = Binary
  | Multiclass
  | Regression
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MLModelType where
    parser = takeLowerText >>= \case
        "binary" -> pure Binary
        "multiclass" -> pure Multiclass
        "regression" -> pure Regression
        e -> fromTextError $ "Failure parsing MLModelType from value: '" <> e
           <> "'. Accepted values: binary, multiclass, regression"

instance ToText MLModelType where
    toText = \case
        Binary -> "BINARY"
        Multiclass -> "MULTICLASS"
        Regression -> "REGRESSION"

instance Hashable     MLModelType
instance NFData       MLModelType
instance ToByteString MLModelType
instance ToQuery      MLModelType
instance ToHeader     MLModelType

instance ToJSON MLModelType where
    toJSON = toJSONText

instance FromJSON MLModelType where
    parseJSON = parseJSONText "MLModelType"

data RealtimeEndpointStatus
  = Failed
  | None
  | Ready
  | Updating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RealtimeEndpointStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "none" -> pure None
        "ready" -> pure Ready
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing RealtimeEndpointStatus from value: '" <> e
           <> "'. Accepted values: failed, none, ready, updating"

instance ToText RealtimeEndpointStatus where
    toText = \case
        Failed -> "FAILED"
        None -> "NONE"
        Ready -> "READY"
        Updating -> "UPDATING"

instance Hashable     RealtimeEndpointStatus
instance NFData       RealtimeEndpointStatus
instance ToByteString RealtimeEndpointStatus
instance ToQuery      RealtimeEndpointStatus
instance ToHeader     RealtimeEndpointStatus

instance FromJSON RealtimeEndpointStatus where
    parseJSON = parseJSONText "RealtimeEndpointStatus"

-- | The sort order specified in a listing condition. Possible values include the following:
--
--
--     * @asc@ - Present the information in ascending order (from A-Z).    * @dsc@ - Present the information in descending order (from Z-A).
--
data SortOrder
  = Asc
  | Dsc
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "asc" -> pure Asc
        "dsc" -> pure Dsc
        e -> fromTextError $ "Failure parsing SortOrder from value: '" <> e
           <> "'. Accepted values: asc, dsc"

instance ToText SortOrder where
    toText = \case
        Asc -> "asc"
        Dsc -> "dsc"

instance Hashable     SortOrder
instance NFData       SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

data TaggableResourceType
  = BatchPrediction
  | DataSource
  | Evaluation
  | MLModel
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaggableResourceType where
    parser = takeLowerText >>= \case
        "batchprediction" -> pure BatchPrediction
        "datasource" -> pure DataSource
        "evaluation" -> pure Evaluation
        "mlmodel" -> pure MLModel
        e -> fromTextError $ "Failure parsing TaggableResourceType from value: '" <> e
           <> "'. Accepted values: batchprediction, datasource, evaluation, mlmodel"

instance ToText TaggableResourceType where
    toText = \case
        BatchPrediction -> "BatchPrediction"
        DataSource -> "DataSource"
        Evaluation -> "Evaluation"
        MLModel -> "MLModel"

instance Hashable     TaggableResourceType
instance NFData       TaggableResourceType
instance ToByteString TaggableResourceType
instance ToQuery      TaggableResourceType
instance ToHeader     TaggableResourceType

instance ToJSON TaggableResourceType where
    toJSON = toJSONText

instance FromJSON TaggableResourceType where
    parseJSON = parseJSONText "TaggableResourceType"
