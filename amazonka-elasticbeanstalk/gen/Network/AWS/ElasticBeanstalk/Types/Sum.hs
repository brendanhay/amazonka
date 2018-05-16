{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Sum where

import Network.AWS.Prelude

data ActionHistoryStatus
  = AHSCompleted
  | AHSFailed
  | AHSUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionHistoryStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure AHSCompleted
        "failed" -> pure AHSFailed
        "unknown" -> pure AHSUnknown
        e -> fromTextError $ "Failure parsing ActionHistoryStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, unknown"

instance ToText ActionHistoryStatus where
    toText = \case
        AHSCompleted -> "Completed"
        AHSFailed -> "Failed"
        AHSUnknown -> "Unknown"

instance Hashable     ActionHistoryStatus
instance NFData       ActionHistoryStatus
instance ToByteString ActionHistoryStatus
instance ToQuery      ActionHistoryStatus
instance ToHeader     ActionHistoryStatus

instance FromXML ActionHistoryStatus where
    parseXML = parseXMLText "ActionHistoryStatus"

data ActionStatus
  = ASPending
  | ASRunning
  | ASScheduled
  | ASUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionStatus where
    parser = takeLowerText >>= \case
        "pending" -> pure ASPending
        "running" -> pure ASRunning
        "scheduled" -> pure ASScheduled
        "unknown" -> pure ASUnknown
        e -> fromTextError $ "Failure parsing ActionStatus from value: '" <> e
           <> "'. Accepted values: pending, running, scheduled, unknown"

instance ToText ActionStatus where
    toText = \case
        ASPending -> "Pending"
        ASRunning -> "Running"
        ASScheduled -> "Scheduled"
        ASUnknown -> "Unknown"

instance Hashable     ActionStatus
instance NFData       ActionStatus
instance ToByteString ActionStatus
instance ToQuery      ActionStatus
instance ToHeader     ActionStatus

instance FromXML ActionStatus where
    parseXML = parseXMLText "ActionStatus"

data ActionType
  = InstanceRefresh
  | PlatformUpdate
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionType where
    parser = takeLowerText >>= \case
        "instancerefresh" -> pure InstanceRefresh
        "platformupdate" -> pure PlatformUpdate
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing ActionType from value: '" <> e
           <> "'. Accepted values: instancerefresh, platformupdate, unknown"

instance ToText ActionType where
    toText = \case
        InstanceRefresh -> "InstanceRefresh"
        PlatformUpdate -> "PlatformUpdate"
        Unknown -> "Unknown"

instance Hashable     ActionType
instance NFData       ActionType
instance ToByteString ActionType
instance ToQuery      ActionType
instance ToHeader     ActionType

instance FromXML ActionType where
    parseXML = parseXMLText "ActionType"

data ApplicationVersionStatus
  = AVSBuilding
  | AVSFailed
  | AVSProcessed
  | AVSProcessing
  | AVSUnprocessed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ApplicationVersionStatus where
    parser = takeLowerText >>= \case
        "building" -> pure AVSBuilding
        "failed" -> pure AVSFailed
        "processed" -> pure AVSProcessed
        "processing" -> pure AVSProcessing
        "unprocessed" -> pure AVSUnprocessed
        e -> fromTextError $ "Failure parsing ApplicationVersionStatus from value: '" <> e
           <> "'. Accepted values: building, failed, processed, processing, unprocessed"

instance ToText ApplicationVersionStatus where
    toText = \case
        AVSBuilding -> "Building"
        AVSFailed -> "Failed"
        AVSProcessed -> "Processed"
        AVSProcessing -> "Processing"
        AVSUnprocessed -> "Unprocessed"

instance Hashable     ApplicationVersionStatus
instance NFData       ApplicationVersionStatus
instance ToByteString ApplicationVersionStatus
instance ToQuery      ApplicationVersionStatus
instance ToHeader     ApplicationVersionStatus

instance FromXML ApplicationVersionStatus where
    parseXML = parseXMLText "ApplicationVersionStatus"

data ComputeType
  = BuildGENERAL1Large
  | BuildGENERAL1Medium
  | BuildGENERAL1Small
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComputeType where
    parser = takeLowerText >>= \case
        "build_general1_large" -> pure BuildGENERAL1Large
        "build_general1_medium" -> pure BuildGENERAL1Medium
        "build_general1_small" -> pure BuildGENERAL1Small
        e -> fromTextError $ "Failure parsing ComputeType from value: '" <> e
           <> "'. Accepted values: build_general1_large, build_general1_medium, build_general1_small"

instance ToText ComputeType where
    toText = \case
        BuildGENERAL1Large -> "BUILD_GENERAL1_LARGE"
        BuildGENERAL1Medium -> "BUILD_GENERAL1_MEDIUM"
        BuildGENERAL1Small -> "BUILD_GENERAL1_SMALL"

instance Hashable     ComputeType
instance NFData       ComputeType
instance ToByteString ComputeType
instance ToQuery      ComputeType
instance ToHeader     ComputeType

data ConfigurationDeploymentStatus
  = CDSDeployed
  | CDSFailed
  | CDSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConfigurationDeploymentStatus where
    parser = takeLowerText >>= \case
        "deployed" -> pure CDSDeployed
        "failed" -> pure CDSFailed
        "pending" -> pure CDSPending
        e -> fromTextError $ "Failure parsing ConfigurationDeploymentStatus from value: '" <> e
           <> "'. Accepted values: deployed, failed, pending"

instance ToText ConfigurationDeploymentStatus where
    toText = \case
        CDSDeployed -> "deployed"
        CDSFailed -> "failed"
        CDSPending -> "pending"

instance Hashable     ConfigurationDeploymentStatus
instance NFData       ConfigurationDeploymentStatus
instance ToByteString ConfigurationDeploymentStatus
instance ToQuery      ConfigurationDeploymentStatus
instance ToHeader     ConfigurationDeploymentStatus

instance FromXML ConfigurationDeploymentStatus where
    parseXML = parseXMLText "ConfigurationDeploymentStatus"

data ConfigurationOptionValueType
  = List
  | Scalar
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConfigurationOptionValueType where
    parser = takeLowerText >>= \case
        "list" -> pure List
        "scalar" -> pure Scalar
        e -> fromTextError $ "Failure parsing ConfigurationOptionValueType from value: '" <> e
           <> "'. Accepted values: list, scalar"

instance ToText ConfigurationOptionValueType where
    toText = \case
        List -> "List"
        Scalar -> "Scalar"

instance Hashable     ConfigurationOptionValueType
instance NFData       ConfigurationOptionValueType
instance ToByteString ConfigurationOptionValueType
instance ToQuery      ConfigurationOptionValueType
instance ToHeader     ConfigurationOptionValueType

instance FromXML ConfigurationOptionValueType where
    parseXML = parseXMLText "ConfigurationOptionValueType"

data EnvironmentHealth
  = Green
  | Grey
  | Red
  | Yellow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnvironmentHealth where
    parser = takeLowerText >>= \case
        "green" -> pure Green
        "grey" -> pure Grey
        "red" -> pure Red
        "yellow" -> pure Yellow
        e -> fromTextError $ "Failure parsing EnvironmentHealth from value: '" <> e
           <> "'. Accepted values: green, grey, red, yellow"

instance ToText EnvironmentHealth where
    toText = \case
        Green -> "Green"
        Grey -> "Grey"
        Red -> "Red"
        Yellow -> "Yellow"

instance Hashable     EnvironmentHealth
instance NFData       EnvironmentHealth
instance ToByteString EnvironmentHealth
instance ToQuery      EnvironmentHealth
instance ToHeader     EnvironmentHealth

instance FromXML EnvironmentHealth where
    parseXML = parseXMLText "EnvironmentHealth"

data EnvironmentHealthAttribute
  = EHAAll
  | EHAApplicationMetrics
  | EHACauses
  | EHAColor
  | EHAHealthStatus
  | EHAInstancesHealth
  | EHARefreshedAt
  | EHAStatus
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnvironmentHealthAttribute where
    parser = takeLowerText >>= \case
        "all" -> pure EHAAll
        "applicationmetrics" -> pure EHAApplicationMetrics
        "causes" -> pure EHACauses
        "color" -> pure EHAColor
        "healthstatus" -> pure EHAHealthStatus
        "instanceshealth" -> pure EHAInstancesHealth
        "refreshedat" -> pure EHARefreshedAt
        "status" -> pure EHAStatus
        e -> fromTextError $ "Failure parsing EnvironmentHealthAttribute from value: '" <> e
           <> "'. Accepted values: all, applicationmetrics, causes, color, healthstatus, instanceshealth, refreshedat, status"

instance ToText EnvironmentHealthAttribute where
    toText = \case
        EHAAll -> "All"
        EHAApplicationMetrics -> "ApplicationMetrics"
        EHACauses -> "Causes"
        EHAColor -> "Color"
        EHAHealthStatus -> "HealthStatus"
        EHAInstancesHealth -> "InstancesHealth"
        EHARefreshedAt -> "RefreshedAt"
        EHAStatus -> "Status"

instance Hashable     EnvironmentHealthAttribute
instance NFData       EnvironmentHealthAttribute
instance ToByteString EnvironmentHealthAttribute
instance ToQuery      EnvironmentHealthAttribute
instance ToHeader     EnvironmentHealthAttribute

data EnvironmentHealthStatus
  = EHSDegraded
  | EHSInfo
  | EHSNoData
  | EHSOK
  | EHSPending
  | EHSSevere
  | EHSUnknown
  | EHSWarning
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnvironmentHealthStatus where
    parser = takeLowerText >>= \case
        "degraded" -> pure EHSDegraded
        "info" -> pure EHSInfo
        "nodata" -> pure EHSNoData
        "ok" -> pure EHSOK
        "pending" -> pure EHSPending
        "severe" -> pure EHSSevere
        "unknown" -> pure EHSUnknown
        "warning" -> pure EHSWarning
        e -> fromTextError $ "Failure parsing EnvironmentHealthStatus from value: '" <> e
           <> "'. Accepted values: degraded, info, nodata, ok, pending, severe, unknown, warning"

instance ToText EnvironmentHealthStatus where
    toText = \case
        EHSDegraded -> "Degraded"
        EHSInfo -> "Info"
        EHSNoData -> "NoData"
        EHSOK -> "Ok"
        EHSPending -> "Pending"
        EHSSevere -> "Severe"
        EHSUnknown -> "Unknown"
        EHSWarning -> "Warning"

instance Hashable     EnvironmentHealthStatus
instance NFData       EnvironmentHealthStatus
instance ToByteString EnvironmentHealthStatus
instance ToQuery      EnvironmentHealthStatus
instance ToHeader     EnvironmentHealthStatus

instance FromXML EnvironmentHealthStatus where
    parseXML = parseXMLText "EnvironmentHealthStatus"

data EnvironmentInfoType
  = Bundle
  | Tail
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnvironmentInfoType where
    parser = takeLowerText >>= \case
        "bundle" -> pure Bundle
        "tail" -> pure Tail
        e -> fromTextError $ "Failure parsing EnvironmentInfoType from value: '" <> e
           <> "'. Accepted values: bundle, tail"

instance ToText EnvironmentInfoType where
    toText = \case
        Bundle -> "bundle"
        Tail -> "tail"

instance Hashable     EnvironmentInfoType
instance NFData       EnvironmentInfoType
instance ToByteString EnvironmentInfoType
instance ToQuery      EnvironmentInfoType
instance ToHeader     EnvironmentInfoType

instance FromXML EnvironmentInfoType where
    parseXML = parseXMLText "EnvironmentInfoType"

data EnvironmentStatus
  = ESLaunching
  | ESReady
  | ESTerminated
  | ESTerminating
  | ESUpdating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnvironmentStatus where
    parser = takeLowerText >>= \case
        "launching" -> pure ESLaunching
        "ready" -> pure ESReady
        "terminated" -> pure ESTerminated
        "terminating" -> pure ESTerminating
        "updating" -> pure ESUpdating
        e -> fromTextError $ "Failure parsing EnvironmentStatus from value: '" <> e
           <> "'. Accepted values: launching, ready, terminated, terminating, updating"

instance ToText EnvironmentStatus where
    toText = \case
        ESLaunching -> "Launching"
        ESReady -> "Ready"
        ESTerminated -> "Terminated"
        ESTerminating -> "Terminating"
        ESUpdating -> "Updating"

instance Hashable     EnvironmentStatus
instance NFData       EnvironmentStatus
instance ToByteString EnvironmentStatus
instance ToQuery      EnvironmentStatus
instance ToHeader     EnvironmentStatus

instance FromXML EnvironmentStatus where
    parseXML = parseXMLText "EnvironmentStatus"

data EventSeverity
  = LevelDebug
  | LevelError'
  | LevelFatal
  | LevelInfo
  | LevelTrace
  | LevelWarn
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventSeverity where
    parser = takeLowerText >>= \case
        "debug" -> pure LevelDebug
        "error" -> pure LevelError'
        "fatal" -> pure LevelFatal
        "info" -> pure LevelInfo
        "trace" -> pure LevelTrace
        "warn" -> pure LevelWarn
        e -> fromTextError $ "Failure parsing EventSeverity from value: '" <> e
           <> "'. Accepted values: debug, error, fatal, info, trace, warn"

instance ToText EventSeverity where
    toText = \case
        LevelDebug -> "DEBUG"
        LevelError' -> "ERROR"
        LevelFatal -> "FATAL"
        LevelInfo -> "INFO"
        LevelTrace -> "TRACE"
        LevelWarn -> "WARN"

instance Hashable     EventSeverity
instance NFData       EventSeverity
instance ToByteString EventSeverity
instance ToQuery      EventSeverity
instance ToHeader     EventSeverity

instance FromXML EventSeverity where
    parseXML = parseXMLText "EventSeverity"

data FailureType
  = CancellationFailed
  | InternalFailure
  | InvalidEnvironmentState
  | PermissionsError
  | RollbackFailed
  | RollbackSuccessful
  | UpdateCancelled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FailureType where
    parser = takeLowerText >>= \case
        "cancellationfailed" -> pure CancellationFailed
        "internalfailure" -> pure InternalFailure
        "invalidenvironmentstate" -> pure InvalidEnvironmentState
        "permissionserror" -> pure PermissionsError
        "rollbackfailed" -> pure RollbackFailed
        "rollbacksuccessful" -> pure RollbackSuccessful
        "updatecancelled" -> pure UpdateCancelled
        e -> fromTextError $ "Failure parsing FailureType from value: '" <> e
           <> "'. Accepted values: cancellationfailed, internalfailure, invalidenvironmentstate, permissionserror, rollbackfailed, rollbacksuccessful, updatecancelled"

instance ToText FailureType where
    toText = \case
        CancellationFailed -> "CancellationFailed"
        InternalFailure -> "InternalFailure"
        InvalidEnvironmentState -> "InvalidEnvironmentState"
        PermissionsError -> "PermissionsError"
        RollbackFailed -> "RollbackFailed"
        RollbackSuccessful -> "RollbackSuccessful"
        UpdateCancelled -> "UpdateCancelled"

instance Hashable     FailureType
instance NFData       FailureType
instance ToByteString FailureType
instance ToQuery      FailureType
instance ToHeader     FailureType

instance FromXML FailureType where
    parseXML = parseXMLText "FailureType"

data InstancesHealthAttribute
  = All
  | ApplicationMetrics
  | AvailabilityZone
  | Causes
  | Color
  | Deployment
  | HealthStatus
  | InstanceType
  | LaunchedAt
  | RefreshedAt
  | System
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstancesHealthAttribute where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "applicationmetrics" -> pure ApplicationMetrics
        "availabilityzone" -> pure AvailabilityZone
        "causes" -> pure Causes
        "color" -> pure Color
        "deployment" -> pure Deployment
        "healthstatus" -> pure HealthStatus
        "instancetype" -> pure InstanceType
        "launchedat" -> pure LaunchedAt
        "refreshedat" -> pure RefreshedAt
        "system" -> pure System
        e -> fromTextError $ "Failure parsing InstancesHealthAttribute from value: '" <> e
           <> "'. Accepted values: all, applicationmetrics, availabilityzone, causes, color, deployment, healthstatus, instancetype, launchedat, refreshedat, system"

instance ToText InstancesHealthAttribute where
    toText = \case
        All -> "All"
        ApplicationMetrics -> "ApplicationMetrics"
        AvailabilityZone -> "AvailabilityZone"
        Causes -> "Causes"
        Color -> "Color"
        Deployment -> "Deployment"
        HealthStatus -> "HealthStatus"
        InstanceType -> "InstanceType"
        LaunchedAt -> "LaunchedAt"
        RefreshedAt -> "RefreshedAt"
        System -> "System"

instance Hashable     InstancesHealthAttribute
instance NFData       InstancesHealthAttribute
instance ToByteString InstancesHealthAttribute
instance ToQuery      InstancesHealthAttribute
instance ToHeader     InstancesHealthAttribute

data PlatformStatus
  = Creating
  | Deleted
  | Deleting
  | Failed
  | Ready
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlatformStatus where
    parser = takeLowerText >>= \case
        "creating" -> pure Creating
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "failed" -> pure Failed
        "ready" -> pure Ready
        e -> fromTextError $ "Failure parsing PlatformStatus from value: '" <> e
           <> "'. Accepted values: creating, deleted, deleting, failed, ready"

instance ToText PlatformStatus where
    toText = \case
        Creating -> "Creating"
        Deleted -> "Deleted"
        Deleting -> "Deleting"
        Failed -> "Failed"
        Ready -> "Ready"

instance Hashable     PlatformStatus
instance NFData       PlatformStatus
instance ToByteString PlatformStatus
instance ToQuery      PlatformStatus
instance ToHeader     PlatformStatus

instance FromXML PlatformStatus where
    parseXML = parseXMLText "PlatformStatus"

data SourceRepository
  = CodeCommit
  | S3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SourceRepository where
    parser = takeLowerText >>= \case
        "codecommit" -> pure CodeCommit
        "s3" -> pure S3
        e -> fromTextError $ "Failure parsing SourceRepository from value: '" <> e
           <> "'. Accepted values: codecommit, s3"

instance ToText SourceRepository where
    toText = \case
        CodeCommit -> "CodeCommit"
        S3 -> "S3"

instance Hashable     SourceRepository
instance NFData       SourceRepository
instance ToByteString SourceRepository
instance ToQuery      SourceRepository
instance ToHeader     SourceRepository

instance FromXML SourceRepository where
    parseXML = parseXMLText "SourceRepository"

data SourceType
  = Git
  | Zip
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SourceType where
    parser = takeLowerText >>= \case
        "git" -> pure Git
        "zip" -> pure Zip
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: git, zip"

instance ToText SourceType where
    toText = \case
        Git -> "Git"
        Zip -> "Zip"

instance Hashable     SourceType
instance NFData       SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"

data ValidationSeverity
  = Error'
  | Warning
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ValidationSeverity where
    parser = takeLowerText >>= \case
        "error" -> pure Error'
        "warning" -> pure Warning
        e -> fromTextError $ "Failure parsing ValidationSeverity from value: '" <> e
           <> "'. Accepted values: error, warning"

instance ToText ValidationSeverity where
    toText = \case
        Error' -> "error"
        Warning -> "warning"

instance Hashable     ValidationSeverity
instance NFData       ValidationSeverity
instance ToByteString ValidationSeverity
instance ToQuery      ValidationSeverity
instance ToHeader     ValidationSeverity

instance FromXML ValidationSeverity where
    parseXML = parseXMLText "ValidationSeverity"
