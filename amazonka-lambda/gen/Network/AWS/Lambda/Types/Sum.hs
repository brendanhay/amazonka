{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.Sum where

import Network.AWS.Prelude

data EventSourcePosition
  = AtTimestamp
  | Latest
  | TrimHorizon
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventSourcePosition where
    parser = takeLowerText >>= \case
        "at_timestamp" -> pure AtTimestamp
        "latest" -> pure Latest
        "trim_horizon" -> pure TrimHorizon
        e -> fromTextError $ "Failure parsing EventSourcePosition from value: '" <> e
           <> "'. Accepted values: at_timestamp, latest, trim_horizon"

instance ToText EventSourcePosition where
    toText = \case
        AtTimestamp -> "AT_TIMESTAMP"
        Latest -> "LATEST"
        TrimHorizon -> "TRIM_HORIZON"

instance Hashable     EventSourcePosition
instance NFData       EventSourcePosition
instance ToByteString EventSourcePosition
instance ToQuery      EventSourcePosition
instance ToHeader     EventSourcePosition

instance ToJSON EventSourcePosition where
    toJSON = toJSONText

data FunctionVersion =
  All
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FunctionVersion where
    parser = takeLowerText >>= \case
        "all" -> pure All
        e -> fromTextError $ "Failure parsing FunctionVersion from value: '" <> e
           <> "'. Accepted values: all"

instance ToText FunctionVersion where
    toText = \case
        All -> "ALL"

instance Hashable     FunctionVersion
instance NFData       FunctionVersion
instance ToByteString FunctionVersion
instance ToQuery      FunctionVersion
instance ToHeader     FunctionVersion

instance ToJSON FunctionVersion where
    toJSON = toJSONText

data InvocationType
  = DryRun
  | Event
  | RequestResponse
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InvocationType where
    parser = takeLowerText >>= \case
        "dryrun" -> pure DryRun
        "event" -> pure Event
        "requestresponse" -> pure RequestResponse
        e -> fromTextError $ "Failure parsing InvocationType from value: '" <> e
           <> "'. Accepted values: dryrun, event, requestresponse"

instance ToText InvocationType where
    toText = \case
        DryRun -> "DryRun"
        Event -> "Event"
        RequestResponse -> "RequestResponse"

instance Hashable     InvocationType
instance NFData       InvocationType
instance ToByteString InvocationType
instance ToQuery      InvocationType
instance ToHeader     InvocationType

instance ToJSON InvocationType where
    toJSON = toJSONText

data LogType
  = None
  | Tail
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LogType where
    parser = takeLowerText >>= \case
        "none" -> pure None
        "tail" -> pure Tail
        e -> fromTextError $ "Failure parsing LogType from value: '" <> e
           <> "'. Accepted values: none, tail"

instance ToText LogType where
    toText = \case
        None -> "None"
        Tail -> "Tail"

instance Hashable     LogType
instance NFData       LogType
instance ToByteString LogType
instance ToQuery      LogType
instance ToHeader     LogType

instance ToJSON LogType where
    toJSON = toJSONText

data Runtime
  = DOTNETCORE1_0
  | DOTNETCORE2_0
  | GO1_x
  | JAVA8
  | NODEJS4_3
  | NODEJS4_3Edge
  | NODEJS6_10
  | NODEJS8_10
  | Nodejs
  | PYTHON2_7
  | PYTHON3_6
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Runtime where
    parser = takeLowerText >>= \case
        "dotnetcore1.0" -> pure DOTNETCORE1_0
        "dotnetcore2.0" -> pure DOTNETCORE2_0
        "go1.x" -> pure GO1_x
        "java8" -> pure JAVA8
        "nodejs4.3" -> pure NODEJS4_3
        "nodejs4.3-edge" -> pure NODEJS4_3Edge
        "nodejs6.10" -> pure NODEJS6_10
        "nodejs8.10" -> pure NODEJS8_10
        "nodejs" -> pure Nodejs
        "python2.7" -> pure PYTHON2_7
        "python3.6" -> pure PYTHON3_6
        e -> fromTextError $ "Failure parsing Runtime from value: '" <> e
           <> "'. Accepted values: dotnetcore1.0, dotnetcore2.0, go1.x, java8, nodejs4.3, nodejs4.3-edge, nodejs6.10, nodejs8.10, nodejs, python2.7, python3.6"

instance ToText Runtime where
    toText = \case
        DOTNETCORE1_0 -> "dotnetcore1.0"
        DOTNETCORE2_0 -> "dotnetcore2.0"
        GO1_x -> "go1.x"
        JAVA8 -> "java8"
        NODEJS4_3 -> "nodejs4.3"
        NODEJS4_3Edge -> "nodejs4.3-edge"
        NODEJS6_10 -> "nodejs6.10"
        NODEJS8_10 -> "nodejs8.10"
        Nodejs -> "nodejs"
        PYTHON2_7 -> "python2.7"
        PYTHON3_6 -> "python3.6"

instance Hashable     Runtime
instance NFData       Runtime
instance ToByteString Runtime
instance ToQuery      Runtime
instance ToHeader     Runtime

instance ToJSON Runtime where
    toJSON = toJSONText

instance FromJSON Runtime where
    parseJSON = parseJSONText "Runtime"

data TracingMode
  = Active
  | PassThrough
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TracingMode where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "passthrough" -> pure PassThrough
        e -> fromTextError $ "Failure parsing TracingMode from value: '" <> e
           <> "'. Accepted values: active, passthrough"

instance ToText TracingMode where
    toText = \case
        Active -> "Active"
        PassThrough -> "PassThrough"

instance Hashable     TracingMode
instance NFData       TracingMode
instance ToByteString TracingMode
instance ToQuery      TracingMode
instance ToHeader     TracingMode

instance ToJSON TracingMode where
    toJSON = toJSONText

instance FromJSON TracingMode where
    parseJSON = parseJSONText "TracingMode"
