{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.Sum where

import Network.AWS.Prelude

data DeploymentType
  = ForceResetDeployment
  | NewDeployment
  | Redeployment
  | ResetDeployment
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentType where
    parser = takeLowerText >>= \case
        "forceresetdeployment" -> pure ForceResetDeployment
        "newdeployment" -> pure NewDeployment
        "redeployment" -> pure Redeployment
        "resetdeployment" -> pure ResetDeployment
        e -> fromTextError $ "Failure parsing DeploymentType from value: '" <> e
           <> "'. Accepted values: forceresetdeployment, newdeployment, redeployment, resetdeployment"

instance ToText DeploymentType where
    toText = \case
        ForceResetDeployment -> "ForceResetDeployment"
        NewDeployment -> "NewDeployment"
        Redeployment -> "Redeployment"
        ResetDeployment -> "ResetDeployment"

instance Hashable     DeploymentType
instance NFData       DeploymentType
instance ToByteString DeploymentType
instance ToQuery      DeploymentType
instance ToHeader     DeploymentType

instance ToJSON DeploymentType where
    toJSON = toJSONText

instance FromJSON DeploymentType where
    parseJSON = parseJSONText "DeploymentType"

data EncodingType
  = Binary
  | JSON
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EncodingType where
    parser = takeLowerText >>= \case
        "binary" -> pure Binary
        "json" -> pure JSON
        e -> fromTextError $ "Failure parsing EncodingType from value: '" <> e
           <> "'. Accepted values: binary, json"

instance ToText EncodingType where
    toText = \case
        Binary -> "binary"
        JSON -> "json"

instance Hashable     EncodingType
instance NFData       EncodingType
instance ToByteString EncodingType
instance ToQuery      EncodingType
instance ToHeader     EncodingType

instance ToJSON EncodingType where
    toJSON = toJSONText

instance FromJSON EncodingType where
    parseJSON = parseJSONText "EncodingType"

data LoggerComponent
  = GreengrassSystem
  | Lambda
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoggerComponent where
    parser = takeLowerText >>= \case
        "greengrasssystem" -> pure GreengrassSystem
        "lambda" -> pure Lambda
        e -> fromTextError $ "Failure parsing LoggerComponent from value: '" <> e
           <> "'. Accepted values: greengrasssystem, lambda"

instance ToText LoggerComponent where
    toText = \case
        GreengrassSystem -> "GreengrassSystem"
        Lambda -> "Lambda"

instance Hashable     LoggerComponent
instance NFData       LoggerComponent
instance ToByteString LoggerComponent
instance ToQuery      LoggerComponent
instance ToHeader     LoggerComponent

instance ToJSON LoggerComponent where
    toJSON = toJSONText

instance FromJSON LoggerComponent where
    parseJSON = parseJSONText "LoggerComponent"

data LoggerLevel
  = Debug
  | Error'
  | Fatal
  | Info
  | Warn
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoggerLevel where
    parser = takeLowerText >>= \case
        "debug" -> pure Debug
        "error" -> pure Error'
        "fatal" -> pure Fatal
        "info" -> pure Info
        "warn" -> pure Warn
        e -> fromTextError $ "Failure parsing LoggerLevel from value: '" <> e
           <> "'. Accepted values: debug, error, fatal, info, warn"

instance ToText LoggerLevel where
    toText = \case
        Debug -> "DEBUG"
        Error' -> "ERROR"
        Fatal -> "FATAL"
        Info -> "INFO"
        Warn -> "WARN"

instance Hashable     LoggerLevel
instance NFData       LoggerLevel
instance ToByteString LoggerLevel
instance ToQuery      LoggerLevel
instance ToHeader     LoggerLevel

instance ToJSON LoggerLevel where
    toJSON = toJSONText

instance FromJSON LoggerLevel where
    parseJSON = parseJSONText "LoggerLevel"

data LoggerType
  = AWSCloudWatch
  | FileSystem
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoggerType where
    parser = takeLowerText >>= \case
        "awscloudwatch" -> pure AWSCloudWatch
        "filesystem" -> pure FileSystem
        e -> fromTextError $ "Failure parsing LoggerType from value: '" <> e
           <> "'. Accepted values: awscloudwatch, filesystem"

instance ToText LoggerType where
    toText = \case
        AWSCloudWatch -> "AWSCloudWatch"
        FileSystem -> "FileSystem"

instance Hashable     LoggerType
instance NFData       LoggerType
instance ToByteString LoggerType
instance ToQuery      LoggerType
instance ToHeader     LoggerType

instance ToJSON LoggerType where
    toJSON = toJSONText

instance FromJSON LoggerType where
    parseJSON = parseJSONText "LoggerType"

-- | The type of permission a function has to access a resource.
data Permission
  = RO
  | RW
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Permission where
    parser = takeLowerText >>= \case
        "ro" -> pure RO
        "rw" -> pure RW
        e -> fromTextError $ "Failure parsing Permission from value: '" <> e
           <> "'. Accepted values: ro, rw"

instance ToText Permission where
    toText = \case
        RO -> "ro"
        RW -> "rw"

instance Hashable     Permission
instance NFData       Permission
instance ToByteString Permission
instance ToQuery      Permission
instance ToHeader     Permission

instance ToJSON Permission where
    toJSON = toJSONText

instance FromJSON Permission where
    parseJSON = parseJSONText "Permission"

-- | The piece of software on the Greengrass core that will be updated.
data SoftwareToUpdate
  = Core
  | OtaAgent
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SoftwareToUpdate where
    parser = takeLowerText >>= \case
        "core" -> pure Core
        "ota_agent" -> pure OtaAgent
        e -> fromTextError $ "Failure parsing SoftwareToUpdate from value: '" <> e
           <> "'. Accepted values: core, ota_agent"

instance ToText SoftwareToUpdate where
    toText = \case
        Core -> "core"
        OtaAgent -> "ota_agent"

instance Hashable     SoftwareToUpdate
instance NFData       SoftwareToUpdate
instance ToByteString SoftwareToUpdate
instance ToQuery      SoftwareToUpdate
instance ToHeader     SoftwareToUpdate

instance ToJSON SoftwareToUpdate where
    toJSON = toJSONText

-- | The minimum level of log statements that should be logged by the OTA Agent during an update.
data UpdateAgentLogLevel
  = UALLDebug
  | UALLError'
  | UALLFatal
  | UALLInfo
  | UALLNone
  | UALLTrace
  | UALLVerbose
  | UALLWarn
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UpdateAgentLogLevel where
    parser = takeLowerText >>= \case
        "debug" -> pure UALLDebug
        "error" -> pure UALLError'
        "fatal" -> pure UALLFatal
        "info" -> pure UALLInfo
        "none" -> pure UALLNone
        "trace" -> pure UALLTrace
        "verbose" -> pure UALLVerbose
        "warn" -> pure UALLWarn
        e -> fromTextError $ "Failure parsing UpdateAgentLogLevel from value: '" <> e
           <> "'. Accepted values: debug, error, fatal, info, none, trace, verbose, warn"

instance ToText UpdateAgentLogLevel where
    toText = \case
        UALLDebug -> "DEBUG"
        UALLError' -> "ERROR"
        UALLFatal -> "FATAL"
        UALLInfo -> "INFO"
        UALLNone -> "NONE"
        UALLTrace -> "TRACE"
        UALLVerbose -> "VERBOSE"
        UALLWarn -> "WARN"

instance Hashable     UpdateAgentLogLevel
instance NFData       UpdateAgentLogLevel
instance ToByteString UpdateAgentLogLevel
instance ToQuery      UpdateAgentLogLevel
instance ToHeader     UpdateAgentLogLevel

instance ToJSON UpdateAgentLogLevel where
    toJSON = toJSONText

-- | The architecture of the cores which are the targets of an update.
data UpdateTargetsArchitecture
  = AARCH64
  | Armv7l
  | X86_64
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UpdateTargetsArchitecture where
    parser = takeLowerText >>= \case
        "aarch64" -> pure AARCH64
        "armv7l" -> pure Armv7l
        "x86_64" -> pure X86_64
        e -> fromTextError $ "Failure parsing UpdateTargetsArchitecture from value: '" <> e
           <> "'. Accepted values: aarch64, armv7l, x86_64"

instance ToText UpdateTargetsArchitecture where
    toText = \case
        AARCH64 -> "aarch64"
        Armv7l -> "armv7l"
        X86_64 -> "x86_64"

instance Hashable     UpdateTargetsArchitecture
instance NFData       UpdateTargetsArchitecture
instance ToByteString UpdateTargetsArchitecture
instance ToQuery      UpdateTargetsArchitecture
instance ToHeader     UpdateTargetsArchitecture

instance ToJSON UpdateTargetsArchitecture where
    toJSON = toJSONText

-- | The operating system of the cores which are the targets of an update.
data UpdateTargetsOperatingSystem
  = AmazonLinux
  | Raspbian
  | Ubuntu
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UpdateTargetsOperatingSystem where
    parser = takeLowerText >>= \case
        "amazon_linux" -> pure AmazonLinux
        "raspbian" -> pure Raspbian
        "ubuntu" -> pure Ubuntu
        e -> fromTextError $ "Failure parsing UpdateTargetsOperatingSystem from value: '" <> e
           <> "'. Accepted values: amazon_linux, raspbian, ubuntu"

instance ToText UpdateTargetsOperatingSystem where
    toText = \case
        AmazonLinux -> "amazon_linux"
        Raspbian -> "raspbian"
        Ubuntu -> "ubuntu"

instance Hashable     UpdateTargetsOperatingSystem
instance NFData       UpdateTargetsOperatingSystem
instance ToByteString UpdateTargetsOperatingSystem
instance ToQuery      UpdateTargetsOperatingSystem
instance ToHeader     UpdateTargetsOperatingSystem

instance ToJSON UpdateTargetsOperatingSystem where
    toJSON = toJSONText
