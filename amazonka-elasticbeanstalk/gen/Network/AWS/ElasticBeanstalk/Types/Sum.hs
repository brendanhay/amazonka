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

data ConfigurationDeploymentStatus
    = Deployed
    | Failed
    | Pending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ConfigurationDeploymentStatus where
    parser = takeLowerText >>= \case
        "deployed" -> pure Deployed
        "failed" -> pure Failed
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing ConfigurationDeploymentStatus from value: '" <> e
           <> "'. Accepted values: deployed, failed, pending"

instance ToText ConfigurationDeploymentStatus where
    toText = \case
        Deployed -> "deployed"
        Failed -> "failed"
        Pending -> "pending"

instance Hashable     ConfigurationDeploymentStatus
instance ToByteString ConfigurationDeploymentStatus
instance ToQuery      ConfigurationDeploymentStatus
instance ToHeader     ConfigurationDeploymentStatus

instance FromXML ConfigurationDeploymentStatus where
    parseXML = parseXMLText "ConfigurationDeploymentStatus"

data ConfigurationOptionValueType
    = List
    | Scalar
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ConfigurationOptionValueType where
    parser = takeLowerText >>= \case
        "list" -> pure List
        "scalar" -> pure Scalar
        e -> fromTextError $ "Failure parsing ConfigurationOptionValueType from value: '" <> e
           <> "'. Accepted values: list, scalar"

instance ToText ConfigurationOptionValueType where
    toText = \case
        List -> "list"
        Scalar -> "scalar"

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        Green -> "green"
        Grey -> "grey"
        Red -> "red"
        Yellow -> "yellow"

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        EHAAll -> "all"
        EHAApplicationMetrics -> "applicationmetrics"
        EHACauses -> "causes"
        EHAColor -> "color"
        EHAHealthStatus -> "healthstatus"
        EHAInstancesHealth -> "instanceshealth"
        EHARefreshedAt -> "refreshedat"
        EHAStatus -> "status"

instance Hashable     EnvironmentHealthAttribute
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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        EHSDegraded -> "degraded"
        EHSInfo -> "info"
        EHSNoData -> "nodata"
        EHSOK -> "ok"
        EHSPending -> "pending"
        EHSSevere -> "severe"
        EHSUnknown -> "unknown"
        EHSWarning -> "warning"

instance Hashable     EnvironmentHealthStatus
instance ToByteString EnvironmentHealthStatus
instance ToQuery      EnvironmentHealthStatus
instance ToHeader     EnvironmentHealthStatus

instance FromXML EnvironmentHealthStatus where
    parseXML = parseXMLText "EnvironmentHealthStatus"

data EnvironmentInfoType
    = Bundle
    | Tail
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EnvironmentStatus where
    parser = takeLowerText >>= \case
        "launching" -> pure Launching
        "ready" -> pure Ready
        "terminated" -> pure Terminated
        "terminating" -> pure Terminating
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing EnvironmentStatus from value: '" <> e
           <> "'. Accepted values: launching, ready, terminated, terminating, updating"

instance ToText EnvironmentStatus where
    toText = \case
        Launching -> "launching"
        Ready -> "ready"
        Terminated -> "terminated"
        Terminating -> "terminating"
        Updating -> "updating"

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        LevelDebug -> "debug"
        LevelError' -> "error"
        LevelFatal -> "fatal"
        LevelInfo -> "info"
        LevelTrace -> "trace"
        LevelWarn -> "warn"

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
           <> "'. Accepted values: all, applicationmetrics, causes, color, healthstatus, launchedat, refreshedat, system"

instance ToText InstancesHealthAttribute where
    toText = \case
        All -> "all"
        ApplicationMetrics -> "applicationmetrics"
        Causes -> "causes"
        Color -> "color"
        HealthStatus -> "healthstatus"
        LaunchedAt -> "launchedat"
        RefreshedAt -> "refreshedat"
        System -> "system"

instance Hashable     InstancesHealthAttribute
instance ToByteString InstancesHealthAttribute
instance ToQuery      InstancesHealthAttribute
instance ToHeader     InstancesHealthAttribute

data ValidationSeverity
    = Error'
    | Warning
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
instance ToByteString ValidationSeverity
instance ToQuery      ValidationSeverity
instance ToHeader     ValidationSeverity

instance FromXML ValidationSeverity where
    parseXML = parseXMLText "ValidationSeverity"
