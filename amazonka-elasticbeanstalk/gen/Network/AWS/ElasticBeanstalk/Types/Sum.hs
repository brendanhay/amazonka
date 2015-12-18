{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Sum where

import           Network.AWS.Prelude

data ApplicationVersionStatus
    = Failed
    | Processed
    | Processing
    | Unprocessed
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ApplicationVersionStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "processed" -> pure Processed
        "processing" -> pure Processing
        "unprocessed" -> pure Unprocessed
        e -> fromTextError $ "Failure parsing ApplicationVersionStatus from value: '" <> e
           <> "'. Accepted values: Failed, Processed, Processing, Unprocessed"

instance ToText ApplicationVersionStatus where
    toText = \case
        Failed -> "Failed"
        Processed -> "Processed"
        Processing -> "Processing"
        Unprocessed -> "Unprocessed"

instance Hashable     ApplicationVersionStatus
instance ToByteString ApplicationVersionStatus
instance ToQuery      ApplicationVersionStatus
instance ToHeader     ApplicationVersionStatus

instance FromXML ApplicationVersionStatus where
    parseXML = parseXMLText "ApplicationVersionStatus"

data ConfigurationDeploymentStatus
    = CDSDeployed
    | CDSFailed
    | CDSPending
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
instance ToByteString ConfigurationDeploymentStatus
instance ToQuery      ConfigurationDeploymentStatus
instance ToHeader     ConfigurationDeploymentStatus

instance FromXML ConfigurationDeploymentStatus where
    parseXML = parseXMLText "ConfigurationDeploymentStatus"

data ConfigurationOptionValueType
    = List
    | Scalar
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ConfigurationOptionValueType where
    parser = takeLowerText >>= \case
        "list" -> pure List
        "scalar" -> pure Scalar
        e -> fromTextError $ "Failure parsing ConfigurationOptionValueType from value: '" <> e
           <> "'. Accepted values: List, Scalar"

instance ToText ConfigurationOptionValueType where
    toText = \case
        List -> "List"
        Scalar -> "Scalar"

instance Hashable     ConfigurationOptionValueType
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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText EnvironmentHealth where
    parser = takeLowerText >>= \case
        "green" -> pure Green
        "grey" -> pure Grey
        "red" -> pure Red
        "yellow" -> pure Yellow
        e -> fromTextError $ "Failure parsing EnvironmentHealth from value: '" <> e
           <> "'. Accepted values: Green, Grey, Red, Yellow"

instance ToText EnvironmentHealth where
    toText = \case
        Green -> "Green"
        Grey -> "Grey"
        Red -> "Red"
        Yellow -> "Yellow"

instance Hashable     EnvironmentHealth
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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: All, ApplicationMetrics, Causes, Color, HealthStatus, InstancesHealth, RefreshedAt, Status"

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
instance ToByteString EnvironmentHealthAttribute
instance ToQuery      EnvironmentHealthAttribute
instance ToHeader     EnvironmentHealthAttribute

data EnvironmentHealthStatus
    = Degraded
    | Info
    | NoData
    | OK
    | Pending
    | Severe
    | Unknown
    | Warning
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText EnvironmentHealthStatus where
    parser = takeLowerText >>= \case
        "degraded" -> pure Degraded
        "info" -> pure Info
        "nodata" -> pure NoData
        "ok" -> pure OK
        "pending" -> pure Pending
        "severe" -> pure Severe
        "unknown" -> pure Unknown
        "warning" -> pure Warning
        e -> fromTextError $ "Failure parsing EnvironmentHealthStatus from value: '" <> e
           <> "'. Accepted values: Degraded, Info, NoData, Ok, Pending, Severe, Unknown, Warning"

instance ToText EnvironmentHealthStatus where
    toText = \case
        Degraded -> "Degraded"
        Info -> "Info"
        NoData -> "NoData"
        OK -> "Ok"
        Pending -> "Pending"
        Severe -> "Severe"
        Unknown -> "Unknown"
        Warning -> "Warning"

instance Hashable     EnvironmentHealthStatus
instance ToByteString EnvironmentHealthStatus
instance ToQuery      EnvironmentHealthStatus
instance ToHeader     EnvironmentHealthStatus

instance FromXML EnvironmentHealthStatus where
    parseXML = parseXMLText "EnvironmentHealthStatus"

data EnvironmentInfoType
    = Bundle
    | Tail
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
instance ToByteString EnvironmentInfoType
instance ToQuery      EnvironmentInfoType
instance ToHeader     EnvironmentInfoType

instance FromXML EnvironmentInfoType where
    parseXML = parseXMLText "EnvironmentInfoType"

data EnvironmentStatus
    = Launching
    | Ready
    | Terminated
    | Terminating
    | Updating
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText EnvironmentStatus where
    parser = takeLowerText >>= \case
        "launching" -> pure Launching
        "ready" -> pure Ready
        "terminated" -> pure Terminated
        "terminating" -> pure Terminating
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing EnvironmentStatus from value: '" <> e
           <> "'. Accepted values: Launching, Ready, Terminated, Terminating, Updating"

instance ToText EnvironmentStatus where
    toText = \case
        Launching -> "Launching"
        Ready -> "Ready"
        Terminated -> "Terminated"
        Terminating -> "Terminating"
        Updating -> "Updating"

instance Hashable     EnvironmentStatus
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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText EventSeverity where
    parser = takeLowerText >>= \case
        "debug" -> pure LevelDebug
        "error" -> pure LevelError'
        "fatal" -> pure LevelFatal
        "info" -> pure LevelInfo
        "trace" -> pure LevelTrace
        "warn" -> pure LevelWarn
        e -> fromTextError $ "Failure parsing EventSeverity from value: '" <> e
           <> "'. Accepted values: DEBUG, ERROR, FATAL, INFO, TRACE, WARN"

instance ToText EventSeverity where
    toText = \case
        LevelDebug -> "DEBUG"
        LevelError' -> "ERROR"
        LevelFatal -> "FATAL"
        LevelInfo -> "INFO"
        LevelTrace -> "TRACE"
        LevelWarn -> "WARN"

instance Hashable     EventSeverity
instance ToByteString EventSeverity
instance ToQuery      EventSeverity
instance ToHeader     EventSeverity

instance FromXML EventSeverity where
    parseXML = parseXMLText "EventSeverity"

data InstancesHealthAttribute
    = All
    | ApplicationMetrics
    | Causes
    | Color
    | HealthStatus
    | LaunchedAt
    | RefreshedAt
    | System
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InstancesHealthAttribute where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "applicationmetrics" -> pure ApplicationMetrics
        "causes" -> pure Causes
        "color" -> pure Color
        "healthstatus" -> pure HealthStatus
        "launchedat" -> pure LaunchedAt
        "refreshedat" -> pure RefreshedAt
        "system" -> pure System
        e -> fromTextError $ "Failure parsing InstancesHealthAttribute from value: '" <> e
           <> "'. Accepted values: All, ApplicationMetrics, Causes, Color, HealthStatus, LaunchedAt, RefreshedAt, System"

instance ToText InstancesHealthAttribute where
    toText = \case
        All -> "All"
        ApplicationMetrics -> "ApplicationMetrics"
        Causes -> "Causes"
        Color -> "Color"
        HealthStatus -> "HealthStatus"
        LaunchedAt -> "LaunchedAt"
        RefreshedAt -> "RefreshedAt"
        System -> "System"

instance Hashable     InstancesHealthAttribute
instance ToByteString InstancesHealthAttribute
instance ToQuery      InstancesHealthAttribute
instance ToHeader     InstancesHealthAttribute

data ValidationSeverity
    = VSError'
    | VSWarning
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ValidationSeverity where
    parser = takeLowerText >>= \case
        "error" -> pure VSError'
        "warning" -> pure VSWarning
        e -> fromTextError $ "Failure parsing ValidationSeverity from value: '" <> e
           <> "'. Accepted values: error, warning"

instance ToText ValidationSeverity where
    toText = \case
        VSError' -> "error"
        VSWarning -> "warning"

instance Hashable     ValidationSeverity
instance ToByteString ValidationSeverity
instance ToQuery      ValidationSeverity
instance ToHeader     ValidationSeverity

instance FromXML ValidationSeverity where
    parseXML = parseXMLText "ValidationSeverity"
