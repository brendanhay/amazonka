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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Sum where

import           Network.AWS.Prelude

data ConfigurationDeploymentStatus
    = Pending
    | Deployed
    | Failed
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
instance ToPath       ConfigurationDeploymentStatus
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
instance ToPath       ConfigurationOptionValueType
instance ToQuery      ConfigurationOptionValueType
instance ToHeader     ConfigurationOptionValueType

instance FromXML ConfigurationOptionValueType where
    parseXML = parseXMLText "ConfigurationOptionValueType"

data EnvironmentHealth
    = Red
    | Yellow
    | Green
    | Grey
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
instance ToPath       EnvironmentHealth
instance ToQuery      EnvironmentHealth
instance ToHeader     EnvironmentHealth

instance FromXML EnvironmentHealth where
    parseXML = parseXMLText "EnvironmentHealth"

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
instance ToPath       EnvironmentInfoType
instance ToQuery      EnvironmentInfoType
instance ToHeader     EnvironmentInfoType

instance FromXML EnvironmentInfoType where
    parseXML = parseXMLText "EnvironmentInfoType"

data EnvironmentStatus
    = Updating
    | Terminating
    | Launching
    | Terminated
    | Ready
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
instance ToPath       EnvironmentStatus
instance ToQuery      EnvironmentStatus
instance ToHeader     EnvironmentStatus

instance FromXML EnvironmentStatus where
    parseXML = parseXMLText "EnvironmentStatus"

data EventSeverity
    = LevelDebug
    | LevelInfo
    | LevelError'
    | LevelWarn
    | LevelTrace
    | LevelFatal
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
instance ToPath       EventSeverity
instance ToQuery      EventSeverity
instance ToHeader     EventSeverity

instance FromXML EventSeverity where
    parseXML = parseXMLText "EventSeverity"

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
instance ToPath       ValidationSeverity
instance ToQuery      ValidationSeverity
instance ToHeader     ValidationSeverity

instance FromXML ValidationSeverity where
    parseXML = parseXMLText "ValidationSeverity"
