{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.Sum where

import           Network.AWS.Prelude

data DeploymentType
    = NewDeployment
    | Redeployment
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DeploymentType where
    parser = takeLowerText >>= \case
        "newdeployment" -> pure NewDeployment
        "redeployment" -> pure Redeployment
        e -> fromTextError $ "Failure parsing DeploymentType from value: '" <> e
           <> "'. Accepted values: newdeployment, redeployment"

instance ToText DeploymentType where
    toText = \case
        NewDeployment -> "NewDeployment"
        Redeployment -> "Redeployment"

instance Hashable     DeploymentType
instance NFData       DeploymentType
instance ToByteString DeploymentType
instance ToQuery      DeploymentType
instance ToHeader     DeploymentType

instance ToJSON DeploymentType where
    toJSON = toJSONText

data LoggerComponent
    = GreengrassSystem
    | Lambda
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
