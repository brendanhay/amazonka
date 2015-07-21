{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.Sum where

import           Network.AWS.Prelude

-- | The function used to train a @MLModel@. Training choices supported by
-- Amazon ML include the following:
--
-- -   SGD - Stochastic Gradient Descent.
-- -   RandomForest - Random forest of decision trees.
data Algorithm =
    Sgd
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Algorithm where
    parser = takeLowerText >>= \case
        "sgd" -> pure Sgd
        e -> fromTextError $ "Failure parsing Algorithm from value: '" <> e
           <> "'. Accepted values: sgd"

instance ToText Algorithm where
    toText = \case
        Sgd -> "sgd"

instance Hashable Algorithm
instance ToQuery Algorithm
instance ToHeader Algorithm

instance FromJSON Algorithm where
    parseJSON = parseJSONText "Algorithm"

-- | A list of the variables to use in searching or filtering
-- @BatchPrediction@.
--
-- -   @CreatedAt@ - Sets the search criteria to @BatchPrediction@ creation
--     date.
-- -   @Status@ - Sets the search criteria to @BatchPrediction@ status.
-- -   @Name@ - Sets the search criteria to the contents of
--     @BatchPrediction@ ____ @Name@.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @BatchPrediction@ creation.
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ used in the
--     @BatchPrediction@.
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in the @BatchPrediction@.
-- -   @DataURI@ - Sets the search criteria to the data file(s) used in the
--     @BatchPrediction@. The URL can identify either a file or an Amazon
--     Simple Storage Service (Amazon S3) bucket or directory.
data BatchPredictionFilterVariable
    = BatchDataSourceId
    | BatchMLModelId
    | BatchIAMUser
    | BatchStatus
    | BatchCreatedAt
    | BatchDataURI
    | BatchName
    | BatchLastUpdatedAt
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        BatchCreatedAt -> "createdat"
        BatchDataSourceId -> "datasourceid"
        BatchDataURI -> "datauri"
        BatchIAMUser -> "iamuser"
        BatchLastUpdatedAt -> "lastupdatedat"
        BatchMLModelId -> "mlmodelid"
        BatchName -> "name"
        BatchStatus -> "status"

instance Hashable BatchPredictionFilterVariable
instance ToQuery BatchPredictionFilterVariable
instance ToHeader BatchPredictionFilterVariable

instance ToJSON BatchPredictionFilterVariable where
    toJSON = toJSONText

-- | A list of the variables to use in searching or filtering @DataSource@.
--
-- -   @CreatedAt@ - Sets the search criteria to @DataSource@ creation
--     date.
-- -   @Status@ - Sets the search criteria to @DataSource@ status.
-- -   @Name@ - Sets the search criteria to the contents of @DataSource@
--     ____ @Name@.
-- -   @DataUri@ - Sets the search criteria to the URI of data files used
--     to create the @DataSource@. The URI can identify either a file or an
--     Amazon Simple Storage Service (Amazon S3) bucket or directory.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @DataSource@ creation.
--
-- Note
--
-- The variable names should match the variable names in the @DataSource@.
data DataSourceFilterVariable
    = DataStatus
    | DataIAMUser
    | DataLastUpdatedAt
    | DataCreatedAt
    | DataName
    | DataDATALOCATIONS3
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        DataCreatedAt -> "createdat"
        DataDATALOCATIONS3 -> "datalocations3"
        DataIAMUser -> "iamuser"
        DataLastUpdatedAt -> "lastupdatedat"
        DataName -> "name"
        DataStatus -> "status"

instance Hashable DataSourceFilterVariable
instance ToQuery DataSourceFilterVariable
instance ToHeader DataSourceFilterVariable

instance ToJSON DataSourceFilterVariable where
    toJSON = toJSONText

-- | Contains the key values of @DetailsMap@: PredictiveModelType - Indicates
-- the type of the @MLModel@. Algorithm - Indicates the algorithm was used
-- for the @MLModel@.
data DetailsAttributes
    = Algorithm
    | PredictiveModelType
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DetailsAttributes where
    parser = takeLowerText >>= \case
        "algorithm" -> pure Algorithm
        "predictivemodeltype" -> pure PredictiveModelType
        e -> fromTextError $ "Failure parsing DetailsAttributes from value: '" <> e
           <> "'. Accepted values: algorithm, predictivemodeltype"

instance ToText DetailsAttributes where
    toText = \case
        Algorithm -> "algorithm"
        PredictiveModelType -> "predictivemodeltype"

instance Hashable DetailsAttributes
instance ToQuery DetailsAttributes
instance ToHeader DetailsAttributes

instance FromJSON DetailsAttributes where
    parseJSON = parseJSONText "DetailsAttributes"

-- | Entity status with the following possible values:
--
-- -   PENDING
-- -   INPROGRESS
-- -   FAILED
-- -   COMPLETED
-- -   DELETED
data EntityStatus
    = Pending
    | Inprogress
    | Deleted
    | Completed
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EntityStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "deleted" -> pure Deleted
        "failed" -> pure Failed
        "inprogress" -> pure Inprogress
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing EntityStatus from value: '" <> e
           <> "'. Accepted values: completed, deleted, failed, inprogress, pending"

instance ToText EntityStatus where
    toText = \case
        Completed -> "completed"
        Deleted -> "deleted"
        Failed -> "failed"
        Inprogress -> "inprogress"
        Pending -> "pending"

instance Hashable EntityStatus
instance ToQuery EntityStatus
instance ToHeader EntityStatus

instance FromJSON EntityStatus where
    parseJSON = parseJSONText "EntityStatus"

-- | A list of the variables to use in searching or filtering @Evaluation@.
--
-- -   @CreatedAt@ - Sets the search criteria to @Evaluation@ creation
--     date.
-- -   @Status@ - Sets the search criteria to @Evaluation@ status.
-- -   @Name@ - Sets the search criteria to the contents of @Evaluation@
--     ____ @Name@.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked an evaluation.
-- -   @MLModelId@ - Sets the search criteria to the @Predictor@ that was
--     evaluated.
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in evaluation.
-- -   @DataUri@ - Sets the search criteria to the data file(s) used in
--     evaluation. The URL can identify either a file or an Amazon Simple
--     Storage Service (Amazon S3) bucket or directory.
data EvaluationFilterVariable
    = EvalDataURI
    | EvalDataSourceId
    | EvalName
    | EvalLastUpdatedAt
    | EvalIAMUser
    | EvalStatus
    | EvalMLModelId
    | EvalCreatedAt
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        EvalCreatedAt -> "createdat"
        EvalDataSourceId -> "datasourceid"
        EvalDataURI -> "datauri"
        EvalIAMUser -> "iamuser"
        EvalLastUpdatedAt -> "lastupdatedat"
        EvalMLModelId -> "mlmodelid"
        EvalName -> "name"
        EvalStatus -> "status"

instance Hashable EvaluationFilterVariable
instance ToQuery EvaluationFilterVariable
instance ToHeader EvaluationFilterVariable

instance ToJSON EvaluationFilterVariable where
    toJSON = toJSONText

data MLModelFilterVariable
    = MLMFVRealtimeEndpointStatus
    | MLMFVMLModelType
    | MLMFVTrainingDataURI
    | MLMFVStatus
    | MLMFVIAMUser
    | MLMFVCreatedAt
    | MLMFVTrainingDataSourceId
    | MLMFVAlgorithm
    | MLMFVName
    | MLMFVLastUpdatedAt
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        MLMFVAlgorithm -> "algorithm"
        MLMFVCreatedAt -> "createdat"
        MLMFVIAMUser -> "iamuser"
        MLMFVLastUpdatedAt -> "lastupdatedat"
        MLMFVMLModelType -> "mlmodeltype"
        MLMFVName -> "name"
        MLMFVRealtimeEndpointStatus -> "realtimeendpointstatus"
        MLMFVStatus -> "status"
        MLMFVTrainingDataSourceId -> "trainingdatasourceid"
        MLMFVTrainingDataURI -> "trainingdatauri"

instance Hashable MLModelFilterVariable
instance ToQuery MLModelFilterVariable
instance ToHeader MLModelFilterVariable

instance ToJSON MLModelFilterVariable where
    toJSON = toJSONText

data MLModelType
    = Multiclass
    | Regression
    | Binary
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MLModelType where
    parser = takeLowerText >>= \case
        "binary" -> pure Binary
        "multiclass" -> pure Multiclass
        "regression" -> pure Regression
        e -> fromTextError $ "Failure parsing MLModelType from value: '" <> e
           <> "'. Accepted values: binary, multiclass, regression"

instance ToText MLModelType where
    toText = \case
        Binary -> "binary"
        Multiclass -> "multiclass"
        Regression -> "regression"

instance Hashable MLModelType
instance ToQuery MLModelType
instance ToHeader MLModelType

instance ToJSON MLModelType where
    toJSON = toJSONText

instance FromJSON MLModelType where
    parseJSON = parseJSONText "MLModelType"

data RealtimeEndpointStatus
    = RESUpdating
    | RESReady
    | RESFailed
    | RESNone
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RealtimeEndpointStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure RESFailed
        "none" -> pure RESNone
        "ready" -> pure RESReady
        "updating" -> pure RESUpdating
        e -> fromTextError $ "Failure parsing RealtimeEndpointStatus from value: '" <> e
           <> "'. Accepted values: failed, none, ready, updating"

instance ToText RealtimeEndpointStatus where
    toText = \case
        RESFailed -> "failed"
        RESNone -> "none"
        RESReady -> "ready"
        RESUpdating -> "updating"

instance Hashable RealtimeEndpointStatus
instance ToQuery RealtimeEndpointStatus
instance ToHeader RealtimeEndpointStatus

instance FromJSON RealtimeEndpointStatus where
    parseJSON = parseJSONText "RealtimeEndpointStatus"

-- | The sort order specified in a listing condition. Possible values include
-- the following:
--
-- -   @asc@ - Present the information in ascending order (from A-Z).
-- -   @dsc@ - Present the information in descending order (from Z-A).
data SortOrder
    = Dsc
    | Asc
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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

instance Hashable SortOrder
instance ToQuery SortOrder
instance ToHeader SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText
