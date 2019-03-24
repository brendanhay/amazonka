{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Sum where

import Network.AWS.Prelude

data AlgorithmSortBy
  = ASBCreationTime
  | ASBName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AlgorithmSortBy where
    parser = takeLowerText >>= \case
        "creationtime" -> pure ASBCreationTime
        "name" -> pure ASBName
        e -> fromTextError $ "Failure parsing AlgorithmSortBy from value: '" <> e
           <> "'. Accepted values: creationtime, name"

instance ToText AlgorithmSortBy where
    toText = \case
        ASBCreationTime -> "CreationTime"
        ASBName -> "Name"

instance Hashable     AlgorithmSortBy
instance NFData       AlgorithmSortBy
instance ToByteString AlgorithmSortBy
instance ToQuery      AlgorithmSortBy
instance ToHeader     AlgorithmSortBy

instance ToJSON AlgorithmSortBy where
    toJSON = toJSONText

data AlgorithmStatus
  = ASCompleted
  | ASDeleting
  | ASFailed
  | ASInProgress
  | ASPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AlgorithmStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure ASCompleted
        "deleting" -> pure ASDeleting
        "failed" -> pure ASFailed
        "inprogress" -> pure ASInProgress
        "pending" -> pure ASPending
        e -> fromTextError $ "Failure parsing AlgorithmStatus from value: '" <> e
           <> "'. Accepted values: completed, deleting, failed, inprogress, pending"

instance ToText AlgorithmStatus where
    toText = \case
        ASCompleted -> "Completed"
        ASDeleting -> "Deleting"
        ASFailed -> "Failed"
        ASInProgress -> "InProgress"
        ASPending -> "Pending"

instance Hashable     AlgorithmStatus
instance NFData       AlgorithmStatus
instance ToByteString AlgorithmStatus
instance ToQuery      AlgorithmStatus
instance ToHeader     AlgorithmStatus

instance FromJSON AlgorithmStatus where
    parseJSON = parseJSONText "AlgorithmStatus"

data AssemblyType
  = ATLine
  | ATNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssemblyType where
    parser = takeLowerText >>= \case
        "line" -> pure ATLine
        "none" -> pure ATNone
        e -> fromTextError $ "Failure parsing AssemblyType from value: '" <> e
           <> "'. Accepted values: line, none"

instance ToText AssemblyType where
    toText = \case
        ATLine -> "Line"
        ATNone -> "None"

instance Hashable     AssemblyType
instance NFData       AssemblyType
instance ToByteString AssemblyType
instance ToQuery      AssemblyType
instance ToHeader     AssemblyType

instance ToJSON AssemblyType where
    toJSON = toJSONText

instance FromJSON AssemblyType where
    parseJSON = parseJSONText "AssemblyType"

data BatchStrategy
  = MultiRecord
  | SingleRecord
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BatchStrategy where
    parser = takeLowerText >>= \case
        "multirecord" -> pure MultiRecord
        "singlerecord" -> pure SingleRecord
        e -> fromTextError $ "Failure parsing BatchStrategy from value: '" <> e
           <> "'. Accepted values: multirecord, singlerecord"

instance ToText BatchStrategy where
    toText = \case
        MultiRecord -> "MultiRecord"
        SingleRecord -> "SingleRecord"

instance Hashable     BatchStrategy
instance NFData       BatchStrategy
instance ToByteString BatchStrategy
instance ToQuery      BatchStrategy
instance ToHeader     BatchStrategy

instance ToJSON BatchStrategy where
    toJSON = toJSONText

instance FromJSON BatchStrategy where
    parseJSON = parseJSONText "BatchStrategy"

data BooleanOperator
  = And
  | OR
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BooleanOperator where
    parser = takeLowerText >>= \case
        "and" -> pure And
        "or" -> pure OR
        e -> fromTextError $ "Failure parsing BooleanOperator from value: '" <> e
           <> "'. Accepted values: and, or"

instance ToText BooleanOperator where
    toText = \case
        And -> "And"
        OR -> "Or"

instance Hashable     BooleanOperator
instance NFData       BooleanOperator
instance ToByteString BooleanOperator
instance ToQuery      BooleanOperator
instance ToHeader     BooleanOperator

instance ToJSON BooleanOperator where
    toJSON = toJSONText

data CodeRepositorySortBy
  = CRSBCreationTime
  | CRSBLastModifiedTime
  | CRSBName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CodeRepositorySortBy where
    parser = takeLowerText >>= \case
        "creationtime" -> pure CRSBCreationTime
        "lastmodifiedtime" -> pure CRSBLastModifiedTime
        "name" -> pure CRSBName
        e -> fromTextError $ "Failure parsing CodeRepositorySortBy from value: '" <> e
           <> "'. Accepted values: creationtime, lastmodifiedtime, name"

instance ToText CodeRepositorySortBy where
    toText = \case
        CRSBCreationTime -> "CreationTime"
        CRSBLastModifiedTime -> "LastModifiedTime"
        CRSBName -> "Name"

instance Hashable     CodeRepositorySortBy
instance NFData       CodeRepositorySortBy
instance ToByteString CodeRepositorySortBy
instance ToQuery      CodeRepositorySortBy
instance ToHeader     CodeRepositorySortBy

instance ToJSON CodeRepositorySortBy where
    toJSON = toJSONText

data CodeRepositorySortOrder
  = CRSOAscending
  | CRSODescending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CodeRepositorySortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure CRSOAscending
        "descending" -> pure CRSODescending
        e -> fromTextError $ "Failure parsing CodeRepositorySortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText CodeRepositorySortOrder where
    toText = \case
        CRSOAscending -> "Ascending"
        CRSODescending -> "Descending"

instance Hashable     CodeRepositorySortOrder
instance NFData       CodeRepositorySortOrder
instance ToByteString CodeRepositorySortOrder
instance ToQuery      CodeRepositorySortOrder
instance ToHeader     CodeRepositorySortOrder

instance ToJSON CodeRepositorySortOrder where
    toJSON = toJSONText

data CompilationJobStatus
  = CJSCompleted
  | CJSFailed
  | CJSInprogress
  | CJSStarting
  | CJSStopped
  | CJSStopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CompilationJobStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure CJSCompleted
        "failed" -> pure CJSFailed
        "inprogress" -> pure CJSInprogress
        "starting" -> pure CJSStarting
        "stopped" -> pure CJSStopped
        "stopping" -> pure CJSStopping
        e -> fromTextError $ "Failure parsing CompilationJobStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, inprogress, starting, stopped, stopping"

instance ToText CompilationJobStatus where
    toText = \case
        CJSCompleted -> "COMPLETED"
        CJSFailed -> "FAILED"
        CJSInprogress -> "INPROGRESS"
        CJSStarting -> "STARTING"
        CJSStopped -> "STOPPED"
        CJSStopping -> "STOPPING"

instance Hashable     CompilationJobStatus
instance NFData       CompilationJobStatus
instance ToByteString CompilationJobStatus
instance ToQuery      CompilationJobStatus
instance ToHeader     CompilationJobStatus

instance ToJSON CompilationJobStatus where
    toJSON = toJSONText

instance FromJSON CompilationJobStatus where
    parseJSON = parseJSONText "CompilationJobStatus"

data CompressionType
  = CTGzip
  | CTNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CompressionType where
    parser = takeLowerText >>= \case
        "gzip" -> pure CTGzip
        "none" -> pure CTNone
        e -> fromTextError $ "Failure parsing CompressionType from value: '" <> e
           <> "'. Accepted values: gzip, none"

instance ToText CompressionType where
    toText = \case
        CTGzip -> "Gzip"
        CTNone -> "None"

instance Hashable     CompressionType
instance NFData       CompressionType
instance ToByteString CompressionType
instance ToQuery      CompressionType
instance ToHeader     CompressionType

instance ToJSON CompressionType where
    toJSON = toJSONText

instance FromJSON CompressionType where
    parseJSON = parseJSONText "CompressionType"

data ContentClassifier
  = FreeOfAdultContent
  | FreeOfPersonallyIdentifiableInformation
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContentClassifier where
    parser = takeLowerText >>= \case
        "freeofadultcontent" -> pure FreeOfAdultContent
        "freeofpersonallyidentifiableinformation" -> pure FreeOfPersonallyIdentifiableInformation
        e -> fromTextError $ "Failure parsing ContentClassifier from value: '" <> e
           <> "'. Accepted values: freeofadultcontent, freeofpersonallyidentifiableinformation"

instance ToText ContentClassifier where
    toText = \case
        FreeOfAdultContent -> "FreeOfAdultContent"
        FreeOfPersonallyIdentifiableInformation -> "FreeOfPersonallyIdentifiableInformation"

instance Hashable     ContentClassifier
instance NFData       ContentClassifier
instance ToByteString ContentClassifier
instance ToQuery      ContentClassifier
instance ToHeader     ContentClassifier

instance ToJSON ContentClassifier where
    toJSON = toJSONText

instance FromJSON ContentClassifier where
    parseJSON = parseJSONText "ContentClassifier"

data DetailedAlgorithmStatus
  = Completed
  | Failed
  | InProgress
  | NotStarted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DetailedAlgorithmStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "notstarted" -> pure NotStarted
        e -> fromTextError $ "Failure parsing DetailedAlgorithmStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, inprogress, notstarted"

instance ToText DetailedAlgorithmStatus where
    toText = \case
        Completed -> "Completed"
        Failed -> "Failed"
        InProgress -> "InProgress"
        NotStarted -> "NotStarted"

instance Hashable     DetailedAlgorithmStatus
instance NFData       DetailedAlgorithmStatus
instance ToByteString DetailedAlgorithmStatus
instance ToQuery      DetailedAlgorithmStatus
instance ToHeader     DetailedAlgorithmStatus

instance FromJSON DetailedAlgorithmStatus where
    parseJSON = parseJSONText "DetailedAlgorithmStatus"

data DetailedModelPackageStatus
  = DMPSCompleted
  | DMPSFailed
  | DMPSInProgress
  | DMPSNotStarted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DetailedModelPackageStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure DMPSCompleted
        "failed" -> pure DMPSFailed
        "inprogress" -> pure DMPSInProgress
        "notstarted" -> pure DMPSNotStarted
        e -> fromTextError $ "Failure parsing DetailedModelPackageStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, inprogress, notstarted"

instance ToText DetailedModelPackageStatus where
    toText = \case
        DMPSCompleted -> "Completed"
        DMPSFailed -> "Failed"
        DMPSInProgress -> "InProgress"
        DMPSNotStarted -> "NotStarted"

instance Hashable     DetailedModelPackageStatus
instance NFData       DetailedModelPackageStatus
instance ToByteString DetailedModelPackageStatus
instance ToQuery      DetailedModelPackageStatus
instance ToHeader     DetailedModelPackageStatus

instance FromJSON DetailedModelPackageStatus where
    parseJSON = parseJSONText "DetailedModelPackageStatus"

data DirectInternetAccess
  = DIADisabled
  | DIAEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DirectInternetAccess where
    parser = takeLowerText >>= \case
        "disabled" -> pure DIADisabled
        "enabled" -> pure DIAEnabled
        e -> fromTextError $ "Failure parsing DirectInternetAccess from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText DirectInternetAccess where
    toText = \case
        DIADisabled -> "Disabled"
        DIAEnabled -> "Enabled"

instance Hashable     DirectInternetAccess
instance NFData       DirectInternetAccess
instance ToByteString DirectInternetAccess
instance ToQuery      DirectInternetAccess
instance ToHeader     DirectInternetAccess

instance ToJSON DirectInternetAccess where
    toJSON = toJSONText

instance FromJSON DirectInternetAccess where
    parseJSON = parseJSONText "DirectInternetAccess"

data EndpointConfigSortKey
  = ECSKCreationTime
  | ECSKName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EndpointConfigSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure ECSKCreationTime
        "name" -> pure ECSKName
        e -> fromTextError $ "Failure parsing EndpointConfigSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, name"

instance ToText EndpointConfigSortKey where
    toText = \case
        ECSKCreationTime -> "CreationTime"
        ECSKName -> "Name"

instance Hashable     EndpointConfigSortKey
instance NFData       EndpointConfigSortKey
instance ToByteString EndpointConfigSortKey
instance ToQuery      EndpointConfigSortKey
instance ToHeader     EndpointConfigSortKey

instance ToJSON EndpointConfigSortKey where
    toJSON = toJSONText

data EndpointSortKey
  = ESKCreationTime
  | ESKName
  | ESKStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EndpointSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure ESKCreationTime
        "name" -> pure ESKName
        "status" -> pure ESKStatus
        e -> fromTextError $ "Failure parsing EndpointSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, name, status"

instance ToText EndpointSortKey where
    toText = \case
        ESKCreationTime -> "CreationTime"
        ESKName -> "Name"
        ESKStatus -> "Status"

instance Hashable     EndpointSortKey
instance NFData       EndpointSortKey
instance ToByteString EndpointSortKey
instance ToQuery      EndpointSortKey
instance ToHeader     EndpointSortKey

instance ToJSON EndpointSortKey where
    toJSON = toJSONText

data EndpointStatus
  = ESCreating
  | ESDeleting
  | ESFailed
  | ESInService
  | ESOutOfService
  | ESRollingBack
  | ESSystemUpdating
  | ESUpdating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EndpointStatus where
    parser = takeLowerText >>= \case
        "creating" -> pure ESCreating
        "deleting" -> pure ESDeleting
        "failed" -> pure ESFailed
        "inservice" -> pure ESInService
        "outofservice" -> pure ESOutOfService
        "rollingback" -> pure ESRollingBack
        "systemupdating" -> pure ESSystemUpdating
        "updating" -> pure ESUpdating
        e -> fromTextError $ "Failure parsing EndpointStatus from value: '" <> e
           <> "'. Accepted values: creating, deleting, failed, inservice, outofservice, rollingback, systemupdating, updating"

instance ToText EndpointStatus where
    toText = \case
        ESCreating -> "Creating"
        ESDeleting -> "Deleting"
        ESFailed -> "Failed"
        ESInService -> "InService"
        ESOutOfService -> "OutOfService"
        ESRollingBack -> "RollingBack"
        ESSystemUpdating -> "SystemUpdating"
        ESUpdating -> "Updating"

instance Hashable     EndpointStatus
instance NFData       EndpointStatus
instance ToByteString EndpointStatus
instance ToQuery      EndpointStatus
instance ToHeader     EndpointStatus

instance ToJSON EndpointStatus where
    toJSON = toJSONText

instance FromJSON EndpointStatus where
    parseJSON = parseJSONText "EndpointStatus"

data Framework
  = Mxnet
  | Onnx
  | Pytorch
  | Tensorflow
  | Xgboost
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Framework where
    parser = takeLowerText >>= \case
        "mxnet" -> pure Mxnet
        "onnx" -> pure Onnx
        "pytorch" -> pure Pytorch
        "tensorflow" -> pure Tensorflow
        "xgboost" -> pure Xgboost
        e -> fromTextError $ "Failure parsing Framework from value: '" <> e
           <> "'. Accepted values: mxnet, onnx, pytorch, tensorflow, xgboost"

instance ToText Framework where
    toText = \case
        Mxnet -> "MXNET"
        Onnx -> "ONNX"
        Pytorch -> "PYTORCH"
        Tensorflow -> "TENSORFLOW"
        Xgboost -> "XGBOOST"

instance Hashable     Framework
instance NFData       Framework
instance ToByteString Framework
instance ToQuery      Framework
instance ToHeader     Framework

instance ToJSON Framework where
    toJSON = toJSONText

instance FromJSON Framework where
    parseJSON = parseJSONText "Framework"

data HyperParameterScalingType
  = Auto
  | Linear
  | Logarithmic
  | ReverseLogarithmic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HyperParameterScalingType where
    parser = takeLowerText >>= \case
        "auto" -> pure Auto
        "linear" -> pure Linear
        "logarithmic" -> pure Logarithmic
        "reverselogarithmic" -> pure ReverseLogarithmic
        e -> fromTextError $ "Failure parsing HyperParameterScalingType from value: '" <> e
           <> "'. Accepted values: auto, linear, logarithmic, reverselogarithmic"

instance ToText HyperParameterScalingType where
    toText = \case
        Auto -> "Auto"
        Linear -> "Linear"
        Logarithmic -> "Logarithmic"
        ReverseLogarithmic -> "ReverseLogarithmic"

instance Hashable     HyperParameterScalingType
instance NFData       HyperParameterScalingType
instance ToByteString HyperParameterScalingType
instance ToQuery      HyperParameterScalingType
instance ToHeader     HyperParameterScalingType

instance ToJSON HyperParameterScalingType where
    toJSON = toJSONText

instance FromJSON HyperParameterScalingType where
    parseJSON = parseJSONText "HyperParameterScalingType"

data HyperParameterTuningJobObjectiveType
  = Maximize
  | Minimize
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HyperParameterTuningJobObjectiveType where
    parser = takeLowerText >>= \case
        "maximize" -> pure Maximize
        "minimize" -> pure Minimize
        e -> fromTextError $ "Failure parsing HyperParameterTuningJobObjectiveType from value: '" <> e
           <> "'. Accepted values: maximize, minimize"

instance ToText HyperParameterTuningJobObjectiveType where
    toText = \case
        Maximize -> "Maximize"
        Minimize -> "Minimize"

instance Hashable     HyperParameterTuningJobObjectiveType
instance NFData       HyperParameterTuningJobObjectiveType
instance ToByteString HyperParameterTuningJobObjectiveType
instance ToQuery      HyperParameterTuningJobObjectiveType
instance ToHeader     HyperParameterTuningJobObjectiveType

instance ToJSON HyperParameterTuningJobObjectiveType where
    toJSON = toJSONText

instance FromJSON HyperParameterTuningJobObjectiveType where
    parseJSON = parseJSONText "HyperParameterTuningJobObjectiveType"

data HyperParameterTuningJobSortByOptions
  = HPTJSBOCreationTime
  | HPTJSBOName
  | HPTJSBOStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HyperParameterTuningJobSortByOptions where
    parser = takeLowerText >>= \case
        "creationtime" -> pure HPTJSBOCreationTime
        "name" -> pure HPTJSBOName
        "status" -> pure HPTJSBOStatus
        e -> fromTextError $ "Failure parsing HyperParameterTuningJobSortByOptions from value: '" <> e
           <> "'. Accepted values: creationtime, name, status"

instance ToText HyperParameterTuningJobSortByOptions where
    toText = \case
        HPTJSBOCreationTime -> "CreationTime"
        HPTJSBOName -> "Name"
        HPTJSBOStatus -> "Status"

instance Hashable     HyperParameterTuningJobSortByOptions
instance NFData       HyperParameterTuningJobSortByOptions
instance ToByteString HyperParameterTuningJobSortByOptions
instance ToQuery      HyperParameterTuningJobSortByOptions
instance ToHeader     HyperParameterTuningJobSortByOptions

instance ToJSON HyperParameterTuningJobSortByOptions where
    toJSON = toJSONText

data HyperParameterTuningJobStatus
  = HPTJSCompleted
  | HPTJSFailed
  | HPTJSInProgress
  | HPTJSStopped
  | HPTJSStopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HyperParameterTuningJobStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure HPTJSCompleted
        "failed" -> pure HPTJSFailed
        "inprogress" -> pure HPTJSInProgress
        "stopped" -> pure HPTJSStopped
        "stopping" -> pure HPTJSStopping
        e -> fromTextError $ "Failure parsing HyperParameterTuningJobStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText HyperParameterTuningJobStatus where
    toText = \case
        HPTJSCompleted -> "Completed"
        HPTJSFailed -> "Failed"
        HPTJSInProgress -> "InProgress"
        HPTJSStopped -> "Stopped"
        HPTJSStopping -> "Stopping"

instance Hashable     HyperParameterTuningJobStatus
instance NFData       HyperParameterTuningJobStatus
instance ToByteString HyperParameterTuningJobStatus
instance ToQuery      HyperParameterTuningJobStatus
instance ToHeader     HyperParameterTuningJobStatus

instance ToJSON HyperParameterTuningJobStatus where
    toJSON = toJSONText

instance FromJSON HyperParameterTuningJobStatus where
    parseJSON = parseJSONText "HyperParameterTuningJobStatus"

-- | The strategy hyperparameter tuning uses to find the best combination of hyperparameters for your model. Currently, the only supported value is @Bayesian@ .
--
--
data HyperParameterTuningJobStrategyType
  = Bayesian
  | Random
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HyperParameterTuningJobStrategyType where
    parser = takeLowerText >>= \case
        "bayesian" -> pure Bayesian
        "random" -> pure Random
        e -> fromTextError $ "Failure parsing HyperParameterTuningJobStrategyType from value: '" <> e
           <> "'. Accepted values: bayesian, random"

instance ToText HyperParameterTuningJobStrategyType where
    toText = \case
        Bayesian -> "Bayesian"
        Random -> "Random"

instance Hashable     HyperParameterTuningJobStrategyType
instance NFData       HyperParameterTuningJobStrategyType
instance ToByteString HyperParameterTuningJobStrategyType
instance ToQuery      HyperParameterTuningJobStrategyType
instance ToHeader     HyperParameterTuningJobStrategyType

instance ToJSON HyperParameterTuningJobStrategyType where
    toJSON = toJSONText

instance FromJSON HyperParameterTuningJobStrategyType where
    parseJSON = parseJSONText "HyperParameterTuningJobStrategyType"

data HyperParameterTuningJobWarmStartType
  = IdenticalDataAndAlgorithm
  | TransferLearning
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HyperParameterTuningJobWarmStartType where
    parser = takeLowerText >>= \case
        "identicaldataandalgorithm" -> pure IdenticalDataAndAlgorithm
        "transferlearning" -> pure TransferLearning
        e -> fromTextError $ "Failure parsing HyperParameterTuningJobWarmStartType from value: '" <> e
           <> "'. Accepted values: identicaldataandalgorithm, transferlearning"

instance ToText HyperParameterTuningJobWarmStartType where
    toText = \case
        IdenticalDataAndAlgorithm -> "IdenticalDataAndAlgorithm"
        TransferLearning -> "TransferLearning"

instance Hashable     HyperParameterTuningJobWarmStartType
instance NFData       HyperParameterTuningJobWarmStartType
instance ToByteString HyperParameterTuningJobWarmStartType
instance ToQuery      HyperParameterTuningJobWarmStartType
instance ToHeader     HyperParameterTuningJobWarmStartType

instance ToJSON HyperParameterTuningJobWarmStartType where
    toJSON = toJSONText

instance FromJSON HyperParameterTuningJobWarmStartType where
    parseJSON = parseJSONText "HyperParameterTuningJobWarmStartType"

data InstanceType
  = Ml_C4_2XLarge
  | Ml_C4_4XLarge
  | Ml_C4_8XLarge
  | Ml_C4_XLarge
  | Ml_C5_18XLarge
  | Ml_C5_2XLarge
  | Ml_C5_4XLarge
  | Ml_C5_9XLarge
  | Ml_C5_XLarge
  | Ml_C5d_18XLarge
  | Ml_C5d_2XLarge
  | Ml_C5d_4XLarge
  | Ml_C5d_9XLarge
  | Ml_C5d_XLarge
  | Ml_M4_10XLarge
  | Ml_M4_16XLarge
  | Ml_M4_2XLarge
  | Ml_M4_4XLarge
  | Ml_M4_XLarge
  | Ml_M5_12XLarge
  | Ml_M5_24XLarge
  | Ml_M5_2XLarge
  | Ml_M5_4XLarge
  | Ml_M5_XLarge
  | Ml_P2_16XLarge
  | Ml_P2_8XLarge
  | Ml_P2_XLarge
  | Ml_P3_16XLarge
  | Ml_P3_2XLarge
  | Ml_P3_8XLarge
  | Ml_T2_2XLarge
  | Ml_T2_Large
  | Ml_T2_Medium
  | Ml_T2_XLarge
  | Ml_T3_2XLarge
  | Ml_T3_Large
  | Ml_T3_Medium
  | Ml_T3_XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceType where
    parser = takeLowerText >>= \case
        "ml.c4.2xlarge" -> pure Ml_C4_2XLarge
        "ml.c4.4xlarge" -> pure Ml_C4_4XLarge
        "ml.c4.8xlarge" -> pure Ml_C4_8XLarge
        "ml.c4.xlarge" -> pure Ml_C4_XLarge
        "ml.c5.18xlarge" -> pure Ml_C5_18XLarge
        "ml.c5.2xlarge" -> pure Ml_C5_2XLarge
        "ml.c5.4xlarge" -> pure Ml_C5_4XLarge
        "ml.c5.9xlarge" -> pure Ml_C5_9XLarge
        "ml.c5.xlarge" -> pure Ml_C5_XLarge
        "ml.c5d.18xlarge" -> pure Ml_C5d_18XLarge
        "ml.c5d.2xlarge" -> pure Ml_C5d_2XLarge
        "ml.c5d.4xlarge" -> pure Ml_C5d_4XLarge
        "ml.c5d.9xlarge" -> pure Ml_C5d_9XLarge
        "ml.c5d.xlarge" -> pure Ml_C5d_XLarge
        "ml.m4.10xlarge" -> pure Ml_M4_10XLarge
        "ml.m4.16xlarge" -> pure Ml_M4_16XLarge
        "ml.m4.2xlarge" -> pure Ml_M4_2XLarge
        "ml.m4.4xlarge" -> pure Ml_M4_4XLarge
        "ml.m4.xlarge" -> pure Ml_M4_XLarge
        "ml.m5.12xlarge" -> pure Ml_M5_12XLarge
        "ml.m5.24xlarge" -> pure Ml_M5_24XLarge
        "ml.m5.2xlarge" -> pure Ml_M5_2XLarge
        "ml.m5.4xlarge" -> pure Ml_M5_4XLarge
        "ml.m5.xlarge" -> pure Ml_M5_XLarge
        "ml.p2.16xlarge" -> pure Ml_P2_16XLarge
        "ml.p2.8xlarge" -> pure Ml_P2_8XLarge
        "ml.p2.xlarge" -> pure Ml_P2_XLarge
        "ml.p3.16xlarge" -> pure Ml_P3_16XLarge
        "ml.p3.2xlarge" -> pure Ml_P3_2XLarge
        "ml.p3.8xlarge" -> pure Ml_P3_8XLarge
        "ml.t2.2xlarge" -> pure Ml_T2_2XLarge
        "ml.t2.large" -> pure Ml_T2_Large
        "ml.t2.medium" -> pure Ml_T2_Medium
        "ml.t2.xlarge" -> pure Ml_T2_XLarge
        "ml.t3.2xlarge" -> pure Ml_T3_2XLarge
        "ml.t3.large" -> pure Ml_T3_Large
        "ml.t3.medium" -> pure Ml_T3_Medium
        "ml.t3.xlarge" -> pure Ml_T3_XLarge
        e -> fromTextError $ "Failure parsing InstanceType from value: '" <> e
           <> "'. Accepted values: ml.c4.2xlarge, ml.c4.4xlarge, ml.c4.8xlarge, ml.c4.xlarge, ml.c5.18xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.xlarge, ml.c5d.18xlarge, ml.c5d.2xlarge, ml.c5d.4xlarge, ml.c5d.9xlarge, ml.c5d.xlarge, ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.m5.12xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge, ml.t2.2xlarge, ml.t2.large, ml.t2.medium, ml.t2.xlarge, ml.t3.2xlarge, ml.t3.large, ml.t3.medium, ml.t3.xlarge"

instance ToText InstanceType where
    toText = \case
        Ml_C4_2XLarge -> "ml.c4.2xlarge"
        Ml_C4_4XLarge -> "ml.c4.4xlarge"
        Ml_C4_8XLarge -> "ml.c4.8xlarge"
        Ml_C4_XLarge -> "ml.c4.xlarge"
        Ml_C5_18XLarge -> "ml.c5.18xlarge"
        Ml_C5_2XLarge -> "ml.c5.2xlarge"
        Ml_C5_4XLarge -> "ml.c5.4xlarge"
        Ml_C5_9XLarge -> "ml.c5.9xlarge"
        Ml_C5_XLarge -> "ml.c5.xlarge"
        Ml_C5d_18XLarge -> "ml.c5d.18xlarge"
        Ml_C5d_2XLarge -> "ml.c5d.2xlarge"
        Ml_C5d_4XLarge -> "ml.c5d.4xlarge"
        Ml_C5d_9XLarge -> "ml.c5d.9xlarge"
        Ml_C5d_XLarge -> "ml.c5d.xlarge"
        Ml_M4_10XLarge -> "ml.m4.10xlarge"
        Ml_M4_16XLarge -> "ml.m4.16xlarge"
        Ml_M4_2XLarge -> "ml.m4.2xlarge"
        Ml_M4_4XLarge -> "ml.m4.4xlarge"
        Ml_M4_XLarge -> "ml.m4.xlarge"
        Ml_M5_12XLarge -> "ml.m5.12xlarge"
        Ml_M5_24XLarge -> "ml.m5.24xlarge"
        Ml_M5_2XLarge -> "ml.m5.2xlarge"
        Ml_M5_4XLarge -> "ml.m5.4xlarge"
        Ml_M5_XLarge -> "ml.m5.xlarge"
        Ml_P2_16XLarge -> "ml.p2.16xlarge"
        Ml_P2_8XLarge -> "ml.p2.8xlarge"
        Ml_P2_XLarge -> "ml.p2.xlarge"
        Ml_P3_16XLarge -> "ml.p3.16xlarge"
        Ml_P3_2XLarge -> "ml.p3.2xlarge"
        Ml_P3_8XLarge -> "ml.p3.8xlarge"
        Ml_T2_2XLarge -> "ml.t2.2xlarge"
        Ml_T2_Large -> "ml.t2.large"
        Ml_T2_Medium -> "ml.t2.medium"
        Ml_T2_XLarge -> "ml.t2.xlarge"
        Ml_T3_2XLarge -> "ml.t3.2xlarge"
        Ml_T3_Large -> "ml.t3.large"
        Ml_T3_Medium -> "ml.t3.medium"
        Ml_T3_XLarge -> "ml.t3.xlarge"

instance Hashable     InstanceType
instance NFData       InstanceType
instance ToByteString InstanceType
instance ToQuery      InstanceType
instance ToHeader     InstanceType

instance ToJSON InstanceType where
    toJSON = toJSONText

instance FromJSON InstanceType where
    parseJSON = parseJSONText "InstanceType"

data LabelingJobStatus
  = LJSCompleted
  | LJSFailed
  | LJSInProgress
  | LJSStopped
  | LJSStopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LabelingJobStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure LJSCompleted
        "failed" -> pure LJSFailed
        "inprogress" -> pure LJSInProgress
        "stopped" -> pure LJSStopped
        "stopping" -> pure LJSStopping
        e -> fromTextError $ "Failure parsing LabelingJobStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText LabelingJobStatus where
    toText = \case
        LJSCompleted -> "Completed"
        LJSFailed -> "Failed"
        LJSInProgress -> "InProgress"
        LJSStopped -> "Stopped"
        LJSStopping -> "Stopping"

instance Hashable     LabelingJobStatus
instance NFData       LabelingJobStatus
instance ToByteString LabelingJobStatus
instance ToQuery      LabelingJobStatus
instance ToHeader     LabelingJobStatus

instance ToJSON LabelingJobStatus where
    toJSON = toJSONText

instance FromJSON LabelingJobStatus where
    parseJSON = parseJSONText "LabelingJobStatus"

data ListCompilationJobsSortBy
  = LCJSBCreationTime
  | LCJSBName
  | LCJSBStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ListCompilationJobsSortBy where
    parser = takeLowerText >>= \case
        "creationtime" -> pure LCJSBCreationTime
        "name" -> pure LCJSBName
        "status" -> pure LCJSBStatus
        e -> fromTextError $ "Failure parsing ListCompilationJobsSortBy from value: '" <> e
           <> "'. Accepted values: creationtime, name, status"

instance ToText ListCompilationJobsSortBy where
    toText = \case
        LCJSBCreationTime -> "CreationTime"
        LCJSBName -> "Name"
        LCJSBStatus -> "Status"

instance Hashable     ListCompilationJobsSortBy
instance NFData       ListCompilationJobsSortBy
instance ToByteString ListCompilationJobsSortBy
instance ToQuery      ListCompilationJobsSortBy
instance ToHeader     ListCompilationJobsSortBy

instance ToJSON ListCompilationJobsSortBy where
    toJSON = toJSONText

data ListLabelingJobsForWorkteamSortByOptions =
  LLJFWSBOCreationTime
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ListLabelingJobsForWorkteamSortByOptions where
    parser = takeLowerText >>= \case
        "creationtime" -> pure LLJFWSBOCreationTime
        e -> fromTextError $ "Failure parsing ListLabelingJobsForWorkteamSortByOptions from value: '" <> e
           <> "'. Accepted values: creationtime"

instance ToText ListLabelingJobsForWorkteamSortByOptions where
    toText = \case
        LLJFWSBOCreationTime -> "CreationTime"

instance Hashable     ListLabelingJobsForWorkteamSortByOptions
instance NFData       ListLabelingJobsForWorkteamSortByOptions
instance ToByteString ListLabelingJobsForWorkteamSortByOptions
instance ToQuery      ListLabelingJobsForWorkteamSortByOptions
instance ToHeader     ListLabelingJobsForWorkteamSortByOptions

instance ToJSON ListLabelingJobsForWorkteamSortByOptions where
    toJSON = toJSONText

data ListWorkteamsSortByOptions
  = LWSBOCreateDate
  | LWSBOName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ListWorkteamsSortByOptions where
    parser = takeLowerText >>= \case
        "createdate" -> pure LWSBOCreateDate
        "name" -> pure LWSBOName
        e -> fromTextError $ "Failure parsing ListWorkteamsSortByOptions from value: '" <> e
           <> "'. Accepted values: createdate, name"

instance ToText ListWorkteamsSortByOptions where
    toText = \case
        LWSBOCreateDate -> "CreateDate"
        LWSBOName -> "Name"

instance Hashable     ListWorkteamsSortByOptions
instance NFData       ListWorkteamsSortByOptions
instance ToByteString ListWorkteamsSortByOptions
instance ToQuery      ListWorkteamsSortByOptions
instance ToHeader     ListWorkteamsSortByOptions

instance ToJSON ListWorkteamsSortByOptions where
    toJSON = toJSONText

data ModelPackageSortBy
  = CreationTime
  | Name
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ModelPackageSortBy where
    parser = takeLowerText >>= \case
        "creationtime" -> pure CreationTime
        "name" -> pure Name
        e -> fromTextError $ "Failure parsing ModelPackageSortBy from value: '" <> e
           <> "'. Accepted values: creationtime, name"

instance ToText ModelPackageSortBy where
    toText = \case
        CreationTime -> "CreationTime"
        Name -> "Name"

instance Hashable     ModelPackageSortBy
instance NFData       ModelPackageSortBy
instance ToByteString ModelPackageSortBy
instance ToQuery      ModelPackageSortBy
instance ToHeader     ModelPackageSortBy

instance ToJSON ModelPackageSortBy where
    toJSON = toJSONText

data ModelPackageStatus
  = MPSCompleted
  | MPSDeleting
  | MPSFailed
  | MPSInProgress
  | MPSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ModelPackageStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure MPSCompleted
        "deleting" -> pure MPSDeleting
        "failed" -> pure MPSFailed
        "inprogress" -> pure MPSInProgress
        "pending" -> pure MPSPending
        e -> fromTextError $ "Failure parsing ModelPackageStatus from value: '" <> e
           <> "'. Accepted values: completed, deleting, failed, inprogress, pending"

instance ToText ModelPackageStatus where
    toText = \case
        MPSCompleted -> "Completed"
        MPSDeleting -> "Deleting"
        MPSFailed -> "Failed"
        MPSInProgress -> "InProgress"
        MPSPending -> "Pending"

instance Hashable     ModelPackageStatus
instance NFData       ModelPackageStatus
instance ToByteString ModelPackageStatus
instance ToQuery      ModelPackageStatus
instance ToHeader     ModelPackageStatus

instance FromJSON ModelPackageStatus where
    parseJSON = parseJSONText "ModelPackageStatus"

data ModelSortKey
  = MSKCreationTime
  | MSKName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ModelSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure MSKCreationTime
        "name" -> pure MSKName
        e -> fromTextError $ "Failure parsing ModelSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, name"

instance ToText ModelSortKey where
    toText = \case
        MSKCreationTime -> "CreationTime"
        MSKName -> "Name"

instance Hashable     ModelSortKey
instance NFData       ModelSortKey
instance ToByteString ModelSortKey
instance ToQuery      ModelSortKey
instance ToHeader     ModelSortKey

instance ToJSON ModelSortKey where
    toJSON = toJSONText

data NotebookInstanceAcceleratorType
  = Ml_EIA1_Large
  | Ml_EIA1_Medium
  | Ml_EIA1_XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceAcceleratorType where
    parser = takeLowerText >>= \case
        "ml.eia1.large" -> pure Ml_EIA1_Large
        "ml.eia1.medium" -> pure Ml_EIA1_Medium
        "ml.eia1.xlarge" -> pure Ml_EIA1_XLarge
        e -> fromTextError $ "Failure parsing NotebookInstanceAcceleratorType from value: '" <> e
           <> "'. Accepted values: ml.eia1.large, ml.eia1.medium, ml.eia1.xlarge"

instance ToText NotebookInstanceAcceleratorType where
    toText = \case
        Ml_EIA1_Large -> "ml.eia1.large"
        Ml_EIA1_Medium -> "ml.eia1.medium"
        Ml_EIA1_XLarge -> "ml.eia1.xlarge"

instance Hashable     NotebookInstanceAcceleratorType
instance NFData       NotebookInstanceAcceleratorType
instance ToByteString NotebookInstanceAcceleratorType
instance ToQuery      NotebookInstanceAcceleratorType
instance ToHeader     NotebookInstanceAcceleratorType

instance ToJSON NotebookInstanceAcceleratorType where
    toJSON = toJSONText

instance FromJSON NotebookInstanceAcceleratorType where
    parseJSON = parseJSONText "NotebookInstanceAcceleratorType"

data NotebookInstanceLifecycleConfigSortKey
  = NILCSKCreationTime
  | NILCSKLastModifiedTime
  | NILCSKName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceLifecycleConfigSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure NILCSKCreationTime
        "lastmodifiedtime" -> pure NILCSKLastModifiedTime
        "name" -> pure NILCSKName
        e -> fromTextError $ "Failure parsing NotebookInstanceLifecycleConfigSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, lastmodifiedtime, name"

instance ToText NotebookInstanceLifecycleConfigSortKey where
    toText = \case
        NILCSKCreationTime -> "CreationTime"
        NILCSKLastModifiedTime -> "LastModifiedTime"
        NILCSKName -> "Name"

instance Hashable     NotebookInstanceLifecycleConfigSortKey
instance NFData       NotebookInstanceLifecycleConfigSortKey
instance ToByteString NotebookInstanceLifecycleConfigSortKey
instance ToQuery      NotebookInstanceLifecycleConfigSortKey
instance ToHeader     NotebookInstanceLifecycleConfigSortKey

instance ToJSON NotebookInstanceLifecycleConfigSortKey where
    toJSON = toJSONText

data NotebookInstanceLifecycleConfigSortOrder
  = NILCSOAscending
  | NILCSODescending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceLifecycleConfigSortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure NILCSOAscending
        "descending" -> pure NILCSODescending
        e -> fromTextError $ "Failure parsing NotebookInstanceLifecycleConfigSortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText NotebookInstanceLifecycleConfigSortOrder where
    toText = \case
        NILCSOAscending -> "Ascending"
        NILCSODescending -> "Descending"

instance Hashable     NotebookInstanceLifecycleConfigSortOrder
instance NFData       NotebookInstanceLifecycleConfigSortOrder
instance ToByteString NotebookInstanceLifecycleConfigSortOrder
instance ToQuery      NotebookInstanceLifecycleConfigSortOrder
instance ToHeader     NotebookInstanceLifecycleConfigSortOrder

instance ToJSON NotebookInstanceLifecycleConfigSortOrder where
    toJSON = toJSONText

data NotebookInstanceSortKey
  = NISKCreationTime
  | NISKName
  | NISKStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure NISKCreationTime
        "name" -> pure NISKName
        "status" -> pure NISKStatus
        e -> fromTextError $ "Failure parsing NotebookInstanceSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, name, status"

instance ToText NotebookInstanceSortKey where
    toText = \case
        NISKCreationTime -> "CreationTime"
        NISKName -> "Name"
        NISKStatus -> "Status"

instance Hashable     NotebookInstanceSortKey
instance NFData       NotebookInstanceSortKey
instance ToByteString NotebookInstanceSortKey
instance ToQuery      NotebookInstanceSortKey
instance ToHeader     NotebookInstanceSortKey

instance ToJSON NotebookInstanceSortKey where
    toJSON = toJSONText

data NotebookInstanceSortOrder
  = Ascending
  | Descending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceSortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing NotebookInstanceSortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText NotebookInstanceSortOrder where
    toText = \case
        Ascending -> "Ascending"
        Descending -> "Descending"

instance Hashable     NotebookInstanceSortOrder
instance NFData       NotebookInstanceSortOrder
instance ToByteString NotebookInstanceSortOrder
instance ToQuery      NotebookInstanceSortOrder
instance ToHeader     NotebookInstanceSortOrder

instance ToJSON NotebookInstanceSortOrder where
    toJSON = toJSONText

data NotebookInstanceStatus
  = NISDeleting
  | NISFailed
  | NISInService
  | NISPending
  | NISStopped
  | NISStopping
  | NISUpdating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotebookInstanceStatus where
    parser = takeLowerText >>= \case
        "deleting" -> pure NISDeleting
        "failed" -> pure NISFailed
        "inservice" -> pure NISInService
        "pending" -> pure NISPending
        "stopped" -> pure NISStopped
        "stopping" -> pure NISStopping
        "updating" -> pure NISUpdating
        e -> fromTextError $ "Failure parsing NotebookInstanceStatus from value: '" <> e
           <> "'. Accepted values: deleting, failed, inservice, pending, stopped, stopping, updating"

instance ToText NotebookInstanceStatus where
    toText = \case
        NISDeleting -> "Deleting"
        NISFailed -> "Failed"
        NISInService -> "InService"
        NISPending -> "Pending"
        NISStopped -> "Stopped"
        NISStopping -> "Stopping"
        NISUpdating -> "Updating"

instance Hashable     NotebookInstanceStatus
instance NFData       NotebookInstanceStatus
instance ToByteString NotebookInstanceStatus
instance ToQuery      NotebookInstanceStatus
instance ToHeader     NotebookInstanceStatus

instance ToJSON NotebookInstanceStatus where
    toJSON = toJSONText

instance FromJSON NotebookInstanceStatus where
    parseJSON = parseJSONText "NotebookInstanceStatus"

data ObjectiveStatus
  = OSFailed
  | OSPending
  | OSSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ObjectiveStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure OSFailed
        "pending" -> pure OSPending
        "succeeded" -> pure OSSucceeded
        e -> fromTextError $ "Failure parsing ObjectiveStatus from value: '" <> e
           <> "'. Accepted values: failed, pending, succeeded"

instance ToText ObjectiveStatus where
    toText = \case
        OSFailed -> "Failed"
        OSPending -> "Pending"
        OSSucceeded -> "Succeeded"

instance Hashable     ObjectiveStatus
instance NFData       ObjectiveStatus
instance ToByteString ObjectiveStatus
instance ToQuery      ObjectiveStatus
instance ToHeader     ObjectiveStatus

instance FromJSON ObjectiveStatus where
    parseJSON = parseJSONText "ObjectiveStatus"

data Operator
  = Contains
  | Equals
  | GreaterThan
  | GreaterThanOrEqualTo
  | LessThan
  | LessThanOrEqualTo
  | NotEquals
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Operator where
    parser = takeLowerText >>= \case
        "contains" -> pure Contains
        "equals" -> pure Equals
        "greaterthan" -> pure GreaterThan
        "greaterthanorequalto" -> pure GreaterThanOrEqualTo
        "lessthan" -> pure LessThan
        "lessthanorequalto" -> pure LessThanOrEqualTo
        "notequals" -> pure NotEquals
        e -> fromTextError $ "Failure parsing Operator from value: '" <> e
           <> "'. Accepted values: contains, equals, greaterthan, greaterthanorequalto, lessthan, lessthanorequalto, notequals"

instance ToText Operator where
    toText = \case
        Contains -> "Contains"
        Equals -> "Equals"
        GreaterThan -> "GreaterThan"
        GreaterThanOrEqualTo -> "GreaterThanOrEqualTo"
        LessThan -> "LessThan"
        LessThanOrEqualTo -> "LessThanOrEqualTo"
        NotEquals -> "NotEquals"

instance Hashable     Operator
instance NFData       Operator
instance ToByteString Operator
instance ToQuery      Operator
instance ToHeader     Operator

instance ToJSON Operator where
    toJSON = toJSONText

data OrderKey
  = OKAscending
  | OKDescending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrderKey where
    parser = takeLowerText >>= \case
        "ascending" -> pure OKAscending
        "descending" -> pure OKDescending
        e -> fromTextError $ "Failure parsing OrderKey from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText OrderKey where
    toText = \case
        OKAscending -> "Ascending"
        OKDescending -> "Descending"

instance Hashable     OrderKey
instance NFData       OrderKey
instance ToByteString OrderKey
instance ToQuery      OrderKey
instance ToHeader     OrderKey

instance ToJSON OrderKey where
    toJSON = toJSONText

data ParameterType
  = Categorical
  | Continuous
  | FreeText
  | Integer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ParameterType where
    parser = takeLowerText >>= \case
        "categorical" -> pure Categorical
        "continuous" -> pure Continuous
        "freetext" -> pure FreeText
        "integer" -> pure Integer
        e -> fromTextError $ "Failure parsing ParameterType from value: '" <> e
           <> "'. Accepted values: categorical, continuous, freetext, integer"

instance ToText ParameterType where
    toText = \case
        Categorical -> "Categorical"
        Continuous -> "Continuous"
        FreeText -> "FreeText"
        Integer -> "Integer"

instance Hashable     ParameterType
instance NFData       ParameterType
instance ToByteString ParameterType
instance ToQuery      ParameterType
instance ToHeader     ParameterType

instance ToJSON ParameterType where
    toJSON = toJSONText

instance FromJSON ParameterType where
    parseJSON = parseJSONText "ParameterType"

data ProductionVariantAcceleratorType
  = PVATMl_EIA1_Large
  | PVATMl_EIA1_Medium
  | PVATMl_EIA1_XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProductionVariantAcceleratorType where
    parser = takeLowerText >>= \case
        "ml.eia1.large" -> pure PVATMl_EIA1_Large
        "ml.eia1.medium" -> pure PVATMl_EIA1_Medium
        "ml.eia1.xlarge" -> pure PVATMl_EIA1_XLarge
        e -> fromTextError $ "Failure parsing ProductionVariantAcceleratorType from value: '" <> e
           <> "'. Accepted values: ml.eia1.large, ml.eia1.medium, ml.eia1.xlarge"

instance ToText ProductionVariantAcceleratorType where
    toText = \case
        PVATMl_EIA1_Large -> "ml.eia1.large"
        PVATMl_EIA1_Medium -> "ml.eia1.medium"
        PVATMl_EIA1_XLarge -> "ml.eia1.xlarge"

instance Hashable     ProductionVariantAcceleratorType
instance NFData       ProductionVariantAcceleratorType
instance ToByteString ProductionVariantAcceleratorType
instance ToQuery      ProductionVariantAcceleratorType
instance ToHeader     ProductionVariantAcceleratorType

instance ToJSON ProductionVariantAcceleratorType where
    toJSON = toJSONText

instance FromJSON ProductionVariantAcceleratorType where
    parseJSON = parseJSONText "ProductionVariantAcceleratorType"

data ProductionVariantInstanceType
  = PVITMl_C4_2XLarge
  | PVITMl_C4_4XLarge
  | PVITMl_C4_8XLarge
  | PVITMl_C4_Large
  | PVITMl_C4_XLarge
  | PVITMl_C5_18XLarge
  | PVITMl_C5_2XLarge
  | PVITMl_C5_4XLarge
  | PVITMl_C5_9XLarge
  | PVITMl_C5_Large
  | PVITMl_C5_XLarge
  | PVITMl_M4_10XLarge
  | PVITMl_M4_16XLarge
  | PVITMl_M4_2XLarge
  | PVITMl_M4_4XLarge
  | PVITMl_M4_XLarge
  | PVITMl_M5_12XLarge
  | PVITMl_M5_24XLarge
  | PVITMl_M5_2XLarge
  | PVITMl_M5_4XLarge
  | PVITMl_M5_Large
  | PVITMl_M5_XLarge
  | PVITMl_P2_16XLarge
  | PVITMl_P2_8XLarge
  | PVITMl_P2_XLarge
  | PVITMl_P3_16XLarge
  | PVITMl_P3_2XLarge
  | PVITMl_P3_8XLarge
  | PVITMl_T2_2XLarge
  | PVITMl_T2_Large
  | PVITMl_T2_Medium
  | PVITMl_T2_XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProductionVariantInstanceType where
    parser = takeLowerText >>= \case
        "ml.c4.2xlarge" -> pure PVITMl_C4_2XLarge
        "ml.c4.4xlarge" -> pure PVITMl_C4_4XLarge
        "ml.c4.8xlarge" -> pure PVITMl_C4_8XLarge
        "ml.c4.large" -> pure PVITMl_C4_Large
        "ml.c4.xlarge" -> pure PVITMl_C4_XLarge
        "ml.c5.18xlarge" -> pure PVITMl_C5_18XLarge
        "ml.c5.2xlarge" -> pure PVITMl_C5_2XLarge
        "ml.c5.4xlarge" -> pure PVITMl_C5_4XLarge
        "ml.c5.9xlarge" -> pure PVITMl_C5_9XLarge
        "ml.c5.large" -> pure PVITMl_C5_Large
        "ml.c5.xlarge" -> pure PVITMl_C5_XLarge
        "ml.m4.10xlarge" -> pure PVITMl_M4_10XLarge
        "ml.m4.16xlarge" -> pure PVITMl_M4_16XLarge
        "ml.m4.2xlarge" -> pure PVITMl_M4_2XLarge
        "ml.m4.4xlarge" -> pure PVITMl_M4_4XLarge
        "ml.m4.xlarge" -> pure PVITMl_M4_XLarge
        "ml.m5.12xlarge" -> pure PVITMl_M5_12XLarge
        "ml.m5.24xlarge" -> pure PVITMl_M5_24XLarge
        "ml.m5.2xlarge" -> pure PVITMl_M5_2XLarge
        "ml.m5.4xlarge" -> pure PVITMl_M5_4XLarge
        "ml.m5.large" -> pure PVITMl_M5_Large
        "ml.m5.xlarge" -> pure PVITMl_M5_XLarge
        "ml.p2.16xlarge" -> pure PVITMl_P2_16XLarge
        "ml.p2.8xlarge" -> pure PVITMl_P2_8XLarge
        "ml.p2.xlarge" -> pure PVITMl_P2_XLarge
        "ml.p3.16xlarge" -> pure PVITMl_P3_16XLarge
        "ml.p3.2xlarge" -> pure PVITMl_P3_2XLarge
        "ml.p3.8xlarge" -> pure PVITMl_P3_8XLarge
        "ml.t2.2xlarge" -> pure PVITMl_T2_2XLarge
        "ml.t2.large" -> pure PVITMl_T2_Large
        "ml.t2.medium" -> pure PVITMl_T2_Medium
        "ml.t2.xlarge" -> pure PVITMl_T2_XLarge
        e -> fromTextError $ "Failure parsing ProductionVariantInstanceType from value: '" <> e
           <> "'. Accepted values: ml.c4.2xlarge, ml.c4.4xlarge, ml.c4.8xlarge, ml.c4.large, ml.c4.xlarge, ml.c5.18xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.large, ml.c5.xlarge, ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.m5.12xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.large, ml.m5.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge, ml.t2.2xlarge, ml.t2.large, ml.t2.medium, ml.t2.xlarge"

instance ToText ProductionVariantInstanceType where
    toText = \case
        PVITMl_C4_2XLarge -> "ml.c4.2xlarge"
        PVITMl_C4_4XLarge -> "ml.c4.4xlarge"
        PVITMl_C4_8XLarge -> "ml.c4.8xlarge"
        PVITMl_C4_Large -> "ml.c4.large"
        PVITMl_C4_XLarge -> "ml.c4.xlarge"
        PVITMl_C5_18XLarge -> "ml.c5.18xlarge"
        PVITMl_C5_2XLarge -> "ml.c5.2xlarge"
        PVITMl_C5_4XLarge -> "ml.c5.4xlarge"
        PVITMl_C5_9XLarge -> "ml.c5.9xlarge"
        PVITMl_C5_Large -> "ml.c5.large"
        PVITMl_C5_XLarge -> "ml.c5.xlarge"
        PVITMl_M4_10XLarge -> "ml.m4.10xlarge"
        PVITMl_M4_16XLarge -> "ml.m4.16xlarge"
        PVITMl_M4_2XLarge -> "ml.m4.2xlarge"
        PVITMl_M4_4XLarge -> "ml.m4.4xlarge"
        PVITMl_M4_XLarge -> "ml.m4.xlarge"
        PVITMl_M5_12XLarge -> "ml.m5.12xlarge"
        PVITMl_M5_24XLarge -> "ml.m5.24xlarge"
        PVITMl_M5_2XLarge -> "ml.m5.2xlarge"
        PVITMl_M5_4XLarge -> "ml.m5.4xlarge"
        PVITMl_M5_Large -> "ml.m5.large"
        PVITMl_M5_XLarge -> "ml.m5.xlarge"
        PVITMl_P2_16XLarge -> "ml.p2.16xlarge"
        PVITMl_P2_8XLarge -> "ml.p2.8xlarge"
        PVITMl_P2_XLarge -> "ml.p2.xlarge"
        PVITMl_P3_16XLarge -> "ml.p3.16xlarge"
        PVITMl_P3_2XLarge -> "ml.p3.2xlarge"
        PVITMl_P3_8XLarge -> "ml.p3.8xlarge"
        PVITMl_T2_2XLarge -> "ml.t2.2xlarge"
        PVITMl_T2_Large -> "ml.t2.large"
        PVITMl_T2_Medium -> "ml.t2.medium"
        PVITMl_T2_XLarge -> "ml.t2.xlarge"

instance Hashable     ProductionVariantInstanceType
instance NFData       ProductionVariantInstanceType
instance ToByteString ProductionVariantInstanceType
instance ToQuery      ProductionVariantInstanceType
instance ToHeader     ProductionVariantInstanceType

instance ToJSON ProductionVariantInstanceType where
    toJSON = toJSONText

instance FromJSON ProductionVariantInstanceType where
    parseJSON = parseJSONText "ProductionVariantInstanceType"

data RecordWrapper
  = None
  | RecordIO
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecordWrapper where
    parser = takeLowerText >>= \case
        "none" -> pure None
        "recordio" -> pure RecordIO
        e -> fromTextError $ "Failure parsing RecordWrapper from value: '" <> e
           <> "'. Accepted values: none, recordio"

instance ToText RecordWrapper where
    toText = \case
        None -> "None"
        RecordIO -> "RecordIO"

instance Hashable     RecordWrapper
instance NFData       RecordWrapper
instance ToByteString RecordWrapper
instance ToQuery      RecordWrapper
instance ToHeader     RecordWrapper

instance ToJSON RecordWrapper where
    toJSON = toJSONText

instance FromJSON RecordWrapper where
    parseJSON = parseJSONText "RecordWrapper"

data ResourceType =
  TrainingJob
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "trainingjob" -> pure TrainingJob
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: trainingjob"

instance ToText ResourceType where
    toText = \case
        TrainingJob -> "TrainingJob"

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

data RootAccess
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RootAccess where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing RootAccess from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText RootAccess where
    toText = \case
        Disabled -> "Disabled"
        Enabled -> "Enabled"

instance Hashable     RootAccess
instance NFData       RootAccess
instance ToByteString RootAccess
instance ToQuery      RootAccess
instance ToHeader     RootAccess

instance ToJSON RootAccess where
    toJSON = toJSONText

instance FromJSON RootAccess where
    parseJSON = parseJSONText "RootAccess"

data S3DataDistribution
  = FullyReplicated
  | ShardedByS3Key
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText S3DataDistribution where
    parser = takeLowerText >>= \case
        "fullyreplicated" -> pure FullyReplicated
        "shardedbys3key" -> pure ShardedByS3Key
        e -> fromTextError $ "Failure parsing S3DataDistribution from value: '" <> e
           <> "'. Accepted values: fullyreplicated, shardedbys3key"

instance ToText S3DataDistribution where
    toText = \case
        FullyReplicated -> "FullyReplicated"
        ShardedByS3Key -> "ShardedByS3Key"

instance Hashable     S3DataDistribution
instance NFData       S3DataDistribution
instance ToByteString S3DataDistribution
instance ToQuery      S3DataDistribution
instance ToHeader     S3DataDistribution

instance ToJSON S3DataDistribution where
    toJSON = toJSONText

instance FromJSON S3DataDistribution where
    parseJSON = parseJSONText "S3DataDistribution"

data S3DataType
  = AugmentedManifestFile
  | ManifestFile
  | S3Prefix
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText S3DataType where
    parser = takeLowerText >>= \case
        "augmentedmanifestfile" -> pure AugmentedManifestFile
        "manifestfile" -> pure ManifestFile
        "s3prefix" -> pure S3Prefix
        e -> fromTextError $ "Failure parsing S3DataType from value: '" <> e
           <> "'. Accepted values: augmentedmanifestfile, manifestfile, s3prefix"

instance ToText S3DataType where
    toText = \case
        AugmentedManifestFile -> "AugmentedManifestFile"
        ManifestFile -> "ManifestFile"
        S3Prefix -> "S3Prefix"

instance Hashable     S3DataType
instance NFData       S3DataType
instance ToByteString S3DataType
instance ToQuery      S3DataType
instance ToHeader     S3DataType

instance ToJSON S3DataType where
    toJSON = toJSONText

instance FromJSON S3DataType where
    parseJSON = parseJSONText "S3DataType"

data SearchSortOrder
  = SSOAscending
  | SSODescending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SearchSortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure SSOAscending
        "descending" -> pure SSODescending
        e -> fromTextError $ "Failure parsing SearchSortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText SearchSortOrder where
    toText = \case
        SSOAscending -> "Ascending"
        SSODescending -> "Descending"

instance Hashable     SearchSortOrder
instance NFData       SearchSortOrder
instance ToByteString SearchSortOrder
instance ToQuery      SearchSortOrder
instance ToHeader     SearchSortOrder

instance ToJSON SearchSortOrder where
    toJSON = toJSONText

data SecondaryStatus
  = SSCompleted
  | SSDownloading
  | SSDownloadingTrainingImage
  | SSFailed
  | SSLaunchingMLInstances
  | SSMaxRuntimeExceeded
  | SSPreparingTrainingStack
  | SSStarting
  | SSStopped
  | SSStopping
  | SSTraining
  | SSUploading
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SecondaryStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure SSCompleted
        "downloading" -> pure SSDownloading
        "downloadingtrainingimage" -> pure SSDownloadingTrainingImage
        "failed" -> pure SSFailed
        "launchingmlinstances" -> pure SSLaunchingMLInstances
        "maxruntimeexceeded" -> pure SSMaxRuntimeExceeded
        "preparingtrainingstack" -> pure SSPreparingTrainingStack
        "starting" -> pure SSStarting
        "stopped" -> pure SSStopped
        "stopping" -> pure SSStopping
        "training" -> pure SSTraining
        "uploading" -> pure SSUploading
        e -> fromTextError $ "Failure parsing SecondaryStatus from value: '" <> e
           <> "'. Accepted values: completed, downloading, downloadingtrainingimage, failed, launchingmlinstances, maxruntimeexceeded, preparingtrainingstack, starting, stopped, stopping, training, uploading"

instance ToText SecondaryStatus where
    toText = \case
        SSCompleted -> "Completed"
        SSDownloading -> "Downloading"
        SSDownloadingTrainingImage -> "DownloadingTrainingImage"
        SSFailed -> "Failed"
        SSLaunchingMLInstances -> "LaunchingMLInstances"
        SSMaxRuntimeExceeded -> "MaxRuntimeExceeded"
        SSPreparingTrainingStack -> "PreparingTrainingStack"
        SSStarting -> "Starting"
        SSStopped -> "Stopped"
        SSStopping -> "Stopping"
        SSTraining -> "Training"
        SSUploading -> "Uploading"

instance Hashable     SecondaryStatus
instance NFData       SecondaryStatus
instance ToByteString SecondaryStatus
instance ToQuery      SecondaryStatus
instance ToHeader     SecondaryStatus

instance FromJSON SecondaryStatus where
    parseJSON = parseJSONText "SecondaryStatus"

data SortBy
  = SBCreationTime
  | SBName
  | SBStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortBy where
    parser = takeLowerText >>= \case
        "creationtime" -> pure SBCreationTime
        "name" -> pure SBName
        "status" -> pure SBStatus
        e -> fromTextError $ "Failure parsing SortBy from value: '" <> e
           <> "'. Accepted values: creationtime, name, status"

instance ToText SortBy where
    toText = \case
        SBCreationTime -> "CreationTime"
        SBName -> "Name"
        SBStatus -> "Status"

instance Hashable     SortBy
instance NFData       SortBy
instance ToByteString SortBy
instance ToQuery      SortBy
instance ToHeader     SortBy

instance ToJSON SortBy where
    toJSON = toJSONText

data SortOrder
  = SOAscending
  | SODescending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure SOAscending
        "descending" -> pure SODescending
        e -> fromTextError $ "Failure parsing SortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText SortOrder where
    toText = \case
        SOAscending -> "Ascending"
        SODescending -> "Descending"

instance Hashable     SortOrder
instance NFData       SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

data SplitType
  = STLine
  | STNone
  | STRecordIO
  | STTFRecord
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SplitType where
    parser = takeLowerText >>= \case
        "line" -> pure STLine
        "none" -> pure STNone
        "recordio" -> pure STRecordIO
        "tfrecord" -> pure STTFRecord
        e -> fromTextError $ "Failure parsing SplitType from value: '" <> e
           <> "'. Accepted values: line, none, recordio, tfrecord"

instance ToText SplitType where
    toText = \case
        STLine -> "Line"
        STNone -> "None"
        STRecordIO -> "RecordIO"
        STTFRecord -> "TFRecord"

instance Hashable     SplitType
instance NFData       SplitType
instance ToByteString SplitType
instance ToQuery      SplitType
instance ToHeader     SplitType

instance ToJSON SplitType where
    toJSON = toJSONText

instance FromJSON SplitType where
    parseJSON = parseJSONText "SplitType"

data TargetDevice
  = Deeplens
  | JetsonTX1
  | JetsonTX2
  | MlC4
  | MlC5
  | MlM4
  | MlM5
  | MlP2
  | MlP3
  | RK3288
  | RK3399
  | Rasp3b
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TargetDevice where
    parser = takeLowerText >>= \case
        "deeplens" -> pure Deeplens
        "jetson_tx1" -> pure JetsonTX1
        "jetson_tx2" -> pure JetsonTX2
        "ml_c4" -> pure MlC4
        "ml_c5" -> pure MlC5
        "ml_m4" -> pure MlM4
        "ml_m5" -> pure MlM5
        "ml_p2" -> pure MlP2
        "ml_p3" -> pure MlP3
        "rk3288" -> pure RK3288
        "rk3399" -> pure RK3399
        "rasp3b" -> pure Rasp3b
        e -> fromTextError $ "Failure parsing TargetDevice from value: '" <> e
           <> "'. Accepted values: deeplens, jetson_tx1, jetson_tx2, ml_c4, ml_c5, ml_m4, ml_m5, ml_p2, ml_p3, rk3288, rk3399, rasp3b"

instance ToText TargetDevice where
    toText = \case
        Deeplens -> "deeplens"
        JetsonTX1 -> "jetson_tx1"
        JetsonTX2 -> "jetson_tx2"
        MlC4 -> "ml_c4"
        MlC5 -> "ml_c5"
        MlM4 -> "ml_m4"
        MlM5 -> "ml_m5"
        MlP2 -> "ml_p2"
        MlP3 -> "ml_p3"
        RK3288 -> "rk3288"
        RK3399 -> "rk3399"
        Rasp3b -> "rasp3b"

instance Hashable     TargetDevice
instance NFData       TargetDevice
instance ToByteString TargetDevice
instance ToQuery      TargetDevice
instance ToHeader     TargetDevice

instance ToJSON TargetDevice where
    toJSON = toJSONText

instance FromJSON TargetDevice where
    parseJSON = parseJSONText "TargetDevice"

data TrainingInputMode
  = File
  | Pipe
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrainingInputMode where
    parser = takeLowerText >>= \case
        "file" -> pure File
        "pipe" -> pure Pipe
        e -> fromTextError $ "Failure parsing TrainingInputMode from value: '" <> e
           <> "'. Accepted values: file, pipe"

instance ToText TrainingInputMode where
    toText = \case
        File -> "File"
        Pipe -> "Pipe"

instance Hashable     TrainingInputMode
instance NFData       TrainingInputMode
instance ToByteString TrainingInputMode
instance ToQuery      TrainingInputMode
instance ToHeader     TrainingInputMode

instance ToJSON TrainingInputMode where
    toJSON = toJSONText

instance FromJSON TrainingInputMode where
    parseJSON = parseJSONText "TrainingInputMode"

data TrainingInstanceType
  = TITMl_C4_2XLarge
  | TITMl_C4_4XLarge
  | TITMl_C4_8XLarge
  | TITMl_C4_XLarge
  | TITMl_C5_18XLarge
  | TITMl_C5_2XLarge
  | TITMl_C5_4XLarge
  | TITMl_C5_9XLarge
  | TITMl_C5_XLarge
  | TITMl_M4_10XLarge
  | TITMl_M4_16XLarge
  | TITMl_M4_2XLarge
  | TITMl_M4_4XLarge
  | TITMl_M4_XLarge
  | TITMl_M5_12XLarge
  | TITMl_M5_24XLarge
  | TITMl_M5_2XLarge
  | TITMl_M5_4XLarge
  | TITMl_M5_Large
  | TITMl_M5_XLarge
  | TITMl_P2_16XLarge
  | TITMl_P2_8XLarge
  | TITMl_P2_XLarge
  | TITMl_P3_16XLarge
  | TITMl_P3_2XLarge
  | TITMl_P3_8XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrainingInstanceType where
    parser = takeLowerText >>= \case
        "ml.c4.2xlarge" -> pure TITMl_C4_2XLarge
        "ml.c4.4xlarge" -> pure TITMl_C4_4XLarge
        "ml.c4.8xlarge" -> pure TITMl_C4_8XLarge
        "ml.c4.xlarge" -> pure TITMl_C4_XLarge
        "ml.c5.18xlarge" -> pure TITMl_C5_18XLarge
        "ml.c5.2xlarge" -> pure TITMl_C5_2XLarge
        "ml.c5.4xlarge" -> pure TITMl_C5_4XLarge
        "ml.c5.9xlarge" -> pure TITMl_C5_9XLarge
        "ml.c5.xlarge" -> pure TITMl_C5_XLarge
        "ml.m4.10xlarge" -> pure TITMl_M4_10XLarge
        "ml.m4.16xlarge" -> pure TITMl_M4_16XLarge
        "ml.m4.2xlarge" -> pure TITMl_M4_2XLarge
        "ml.m4.4xlarge" -> pure TITMl_M4_4XLarge
        "ml.m4.xlarge" -> pure TITMl_M4_XLarge
        "ml.m5.12xlarge" -> pure TITMl_M5_12XLarge
        "ml.m5.24xlarge" -> pure TITMl_M5_24XLarge
        "ml.m5.2xlarge" -> pure TITMl_M5_2XLarge
        "ml.m5.4xlarge" -> pure TITMl_M5_4XLarge
        "ml.m5.large" -> pure TITMl_M5_Large
        "ml.m5.xlarge" -> pure TITMl_M5_XLarge
        "ml.p2.16xlarge" -> pure TITMl_P2_16XLarge
        "ml.p2.8xlarge" -> pure TITMl_P2_8XLarge
        "ml.p2.xlarge" -> pure TITMl_P2_XLarge
        "ml.p3.16xlarge" -> pure TITMl_P3_16XLarge
        "ml.p3.2xlarge" -> pure TITMl_P3_2XLarge
        "ml.p3.8xlarge" -> pure TITMl_P3_8XLarge
        e -> fromTextError $ "Failure parsing TrainingInstanceType from value: '" <> e
           <> "'. Accepted values: ml.c4.2xlarge, ml.c4.4xlarge, ml.c4.8xlarge, ml.c4.xlarge, ml.c5.18xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.xlarge, ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.m5.12xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.large, ml.m5.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge"

instance ToText TrainingInstanceType where
    toText = \case
        TITMl_C4_2XLarge -> "ml.c4.2xlarge"
        TITMl_C4_4XLarge -> "ml.c4.4xlarge"
        TITMl_C4_8XLarge -> "ml.c4.8xlarge"
        TITMl_C4_XLarge -> "ml.c4.xlarge"
        TITMl_C5_18XLarge -> "ml.c5.18xlarge"
        TITMl_C5_2XLarge -> "ml.c5.2xlarge"
        TITMl_C5_4XLarge -> "ml.c5.4xlarge"
        TITMl_C5_9XLarge -> "ml.c5.9xlarge"
        TITMl_C5_XLarge -> "ml.c5.xlarge"
        TITMl_M4_10XLarge -> "ml.m4.10xlarge"
        TITMl_M4_16XLarge -> "ml.m4.16xlarge"
        TITMl_M4_2XLarge -> "ml.m4.2xlarge"
        TITMl_M4_4XLarge -> "ml.m4.4xlarge"
        TITMl_M4_XLarge -> "ml.m4.xlarge"
        TITMl_M5_12XLarge -> "ml.m5.12xlarge"
        TITMl_M5_24XLarge -> "ml.m5.24xlarge"
        TITMl_M5_2XLarge -> "ml.m5.2xlarge"
        TITMl_M5_4XLarge -> "ml.m5.4xlarge"
        TITMl_M5_Large -> "ml.m5.large"
        TITMl_M5_XLarge -> "ml.m5.xlarge"
        TITMl_P2_16XLarge -> "ml.p2.16xlarge"
        TITMl_P2_8XLarge -> "ml.p2.8xlarge"
        TITMl_P2_XLarge -> "ml.p2.xlarge"
        TITMl_P3_16XLarge -> "ml.p3.16xlarge"
        TITMl_P3_2XLarge -> "ml.p3.2xlarge"
        TITMl_P3_8XLarge -> "ml.p3.8xlarge"

instance Hashable     TrainingInstanceType
instance NFData       TrainingInstanceType
instance ToByteString TrainingInstanceType
instance ToQuery      TrainingInstanceType
instance ToHeader     TrainingInstanceType

instance ToJSON TrainingInstanceType where
    toJSON = toJSONText

instance FromJSON TrainingInstanceType where
    parseJSON = parseJSONText "TrainingInstanceType"

data TrainingJobEarlyStoppingType
  = TJESTAuto
  | TJESTOff
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrainingJobEarlyStoppingType where
    parser = takeLowerText >>= \case
        "auto" -> pure TJESTAuto
        "off" -> pure TJESTOff
        e -> fromTextError $ "Failure parsing TrainingJobEarlyStoppingType from value: '" <> e
           <> "'. Accepted values: auto, off"

instance ToText TrainingJobEarlyStoppingType where
    toText = \case
        TJESTAuto -> "Auto"
        TJESTOff -> "Off"

instance Hashable     TrainingJobEarlyStoppingType
instance NFData       TrainingJobEarlyStoppingType
instance ToByteString TrainingJobEarlyStoppingType
instance ToQuery      TrainingJobEarlyStoppingType
instance ToHeader     TrainingJobEarlyStoppingType

instance ToJSON TrainingJobEarlyStoppingType where
    toJSON = toJSONText

instance FromJSON TrainingJobEarlyStoppingType where
    parseJSON = parseJSONText "TrainingJobEarlyStoppingType"

data TrainingJobSortByOptions
  = TJSBOCreationTime
  | TJSBOFinalObjectiveMetricValue
  | TJSBOName
  | TJSBOStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrainingJobSortByOptions where
    parser = takeLowerText >>= \case
        "creationtime" -> pure TJSBOCreationTime
        "finalobjectivemetricvalue" -> pure TJSBOFinalObjectiveMetricValue
        "name" -> pure TJSBOName
        "status" -> pure TJSBOStatus
        e -> fromTextError $ "Failure parsing TrainingJobSortByOptions from value: '" <> e
           <> "'. Accepted values: creationtime, finalobjectivemetricvalue, name, status"

instance ToText TrainingJobSortByOptions where
    toText = \case
        TJSBOCreationTime -> "CreationTime"
        TJSBOFinalObjectiveMetricValue -> "FinalObjectiveMetricValue"
        TJSBOName -> "Name"
        TJSBOStatus -> "Status"

instance Hashable     TrainingJobSortByOptions
instance NFData       TrainingJobSortByOptions
instance ToByteString TrainingJobSortByOptions
instance ToQuery      TrainingJobSortByOptions
instance ToHeader     TrainingJobSortByOptions

instance ToJSON TrainingJobSortByOptions where
    toJSON = toJSONText

data TrainingJobStatus
  = TJSCompleted
  | TJSFailed
  | TJSInProgress
  | TJSStopped
  | TJSStopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrainingJobStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure TJSCompleted
        "failed" -> pure TJSFailed
        "inprogress" -> pure TJSInProgress
        "stopped" -> pure TJSStopped
        "stopping" -> pure TJSStopping
        e -> fromTextError $ "Failure parsing TrainingJobStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText TrainingJobStatus where
    toText = \case
        TJSCompleted -> "Completed"
        TJSFailed -> "Failed"
        TJSInProgress -> "InProgress"
        TJSStopped -> "Stopped"
        TJSStopping -> "Stopping"

instance Hashable     TrainingJobStatus
instance NFData       TrainingJobStatus
instance ToByteString TrainingJobStatus
instance ToQuery      TrainingJobStatus
instance ToHeader     TrainingJobStatus

instance ToJSON TrainingJobStatus where
    toJSON = toJSONText

instance FromJSON TrainingJobStatus where
    parseJSON = parseJSONText "TrainingJobStatus"

data TransformInstanceType
  = TMl_C4_2XLarge
  | TMl_C4_4XLarge
  | TMl_C4_8XLarge
  | TMl_C4_XLarge
  | TMl_C5_18XLarge
  | TMl_C5_2XLarge
  | TMl_C5_4XLarge
  | TMl_C5_9XLarge
  | TMl_C5_XLarge
  | TMl_M4_10XLarge
  | TMl_M4_16XLarge
  | TMl_M4_2XLarge
  | TMl_M4_4XLarge
  | TMl_M4_XLarge
  | TMl_M5_12XLarge
  | TMl_M5_24XLarge
  | TMl_M5_2XLarge
  | TMl_M5_4XLarge
  | TMl_M5_Large
  | TMl_M5_XLarge
  | TMl_P2_16XLarge
  | TMl_P2_8XLarge
  | TMl_P2_XLarge
  | TMl_P3_16XLarge
  | TMl_P3_2XLarge
  | TMl_P3_8XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransformInstanceType where
    parser = takeLowerText >>= \case
        "ml.c4.2xlarge" -> pure TMl_C4_2XLarge
        "ml.c4.4xlarge" -> pure TMl_C4_4XLarge
        "ml.c4.8xlarge" -> pure TMl_C4_8XLarge
        "ml.c4.xlarge" -> pure TMl_C4_XLarge
        "ml.c5.18xlarge" -> pure TMl_C5_18XLarge
        "ml.c5.2xlarge" -> pure TMl_C5_2XLarge
        "ml.c5.4xlarge" -> pure TMl_C5_4XLarge
        "ml.c5.9xlarge" -> pure TMl_C5_9XLarge
        "ml.c5.xlarge" -> pure TMl_C5_XLarge
        "ml.m4.10xlarge" -> pure TMl_M4_10XLarge
        "ml.m4.16xlarge" -> pure TMl_M4_16XLarge
        "ml.m4.2xlarge" -> pure TMl_M4_2XLarge
        "ml.m4.4xlarge" -> pure TMl_M4_4XLarge
        "ml.m4.xlarge" -> pure TMl_M4_XLarge
        "ml.m5.12xlarge" -> pure TMl_M5_12XLarge
        "ml.m5.24xlarge" -> pure TMl_M5_24XLarge
        "ml.m5.2xlarge" -> pure TMl_M5_2XLarge
        "ml.m5.4xlarge" -> pure TMl_M5_4XLarge
        "ml.m5.large" -> pure TMl_M5_Large
        "ml.m5.xlarge" -> pure TMl_M5_XLarge
        "ml.p2.16xlarge" -> pure TMl_P2_16XLarge
        "ml.p2.8xlarge" -> pure TMl_P2_8XLarge
        "ml.p2.xlarge" -> pure TMl_P2_XLarge
        "ml.p3.16xlarge" -> pure TMl_P3_16XLarge
        "ml.p3.2xlarge" -> pure TMl_P3_2XLarge
        "ml.p3.8xlarge" -> pure TMl_P3_8XLarge
        e -> fromTextError $ "Failure parsing TransformInstanceType from value: '" <> e
           <> "'. Accepted values: ml.c4.2xlarge, ml.c4.4xlarge, ml.c4.8xlarge, ml.c4.xlarge, ml.c5.18xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.xlarge, ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.m5.12xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.large, ml.m5.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge"

instance ToText TransformInstanceType where
    toText = \case
        TMl_C4_2XLarge -> "ml.c4.2xlarge"
        TMl_C4_4XLarge -> "ml.c4.4xlarge"
        TMl_C4_8XLarge -> "ml.c4.8xlarge"
        TMl_C4_XLarge -> "ml.c4.xlarge"
        TMl_C5_18XLarge -> "ml.c5.18xlarge"
        TMl_C5_2XLarge -> "ml.c5.2xlarge"
        TMl_C5_4XLarge -> "ml.c5.4xlarge"
        TMl_C5_9XLarge -> "ml.c5.9xlarge"
        TMl_C5_XLarge -> "ml.c5.xlarge"
        TMl_M4_10XLarge -> "ml.m4.10xlarge"
        TMl_M4_16XLarge -> "ml.m4.16xlarge"
        TMl_M4_2XLarge -> "ml.m4.2xlarge"
        TMl_M4_4XLarge -> "ml.m4.4xlarge"
        TMl_M4_XLarge -> "ml.m4.xlarge"
        TMl_M5_12XLarge -> "ml.m5.12xlarge"
        TMl_M5_24XLarge -> "ml.m5.24xlarge"
        TMl_M5_2XLarge -> "ml.m5.2xlarge"
        TMl_M5_4XLarge -> "ml.m5.4xlarge"
        TMl_M5_Large -> "ml.m5.large"
        TMl_M5_XLarge -> "ml.m5.xlarge"
        TMl_P2_16XLarge -> "ml.p2.16xlarge"
        TMl_P2_8XLarge -> "ml.p2.8xlarge"
        TMl_P2_XLarge -> "ml.p2.xlarge"
        TMl_P3_16XLarge -> "ml.p3.16xlarge"
        TMl_P3_2XLarge -> "ml.p3.2xlarge"
        TMl_P3_8XLarge -> "ml.p3.8xlarge"

instance Hashable     TransformInstanceType
instance NFData       TransformInstanceType
instance ToByteString TransformInstanceType
instance ToQuery      TransformInstanceType
instance ToHeader     TransformInstanceType

instance ToJSON TransformInstanceType where
    toJSON = toJSONText

instance FromJSON TransformInstanceType where
    parseJSON = parseJSONText "TransformInstanceType"

data TransformJobStatus
  = TCompleted
  | TFailed
  | TInProgress
  | TStopped
  | TStopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TransformJobStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure TCompleted
        "failed" -> pure TFailed
        "inprogress" -> pure TInProgress
        "stopped" -> pure TStopped
        "stopping" -> pure TStopping
        e -> fromTextError $ "Failure parsing TransformJobStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText TransformJobStatus where
    toText = \case
        TCompleted -> "Completed"
        TFailed -> "Failed"
        TInProgress -> "InProgress"
        TStopped -> "Stopped"
        TStopping -> "Stopping"

instance Hashable     TransformJobStatus
instance NFData       TransformJobStatus
instance ToByteString TransformJobStatus
instance ToQuery      TransformJobStatus
instance ToHeader     TransformJobStatus

instance ToJSON TransformJobStatus where
    toJSON = toJSONText

instance FromJSON TransformJobStatus where
    parseJSON = parseJSONText "TransformJobStatus"
