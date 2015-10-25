{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.Sum where

import           Network.AWS.Prelude

data EventSourcePosition
    = Latest
    | TrimHorizon
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EventSourcePosition where
    parser = takeLowerText >>= \case
        "latest" -> pure Latest
        "trim_horizon" -> pure TrimHorizon
        e -> fromTextError $ "Failure parsing EventSourcePosition from value: '" <> e
           <> "'. Accepted values: LATEST, TRIM_HORIZON"

instance ToText EventSourcePosition where
    toText = \case
        Latest -> "LATEST"
        TrimHorizon -> "TRIM_HORIZON"

instance Hashable     EventSourcePosition
instance ToByteString EventSourcePosition
instance ToQuery      EventSourcePosition
instance ToHeader     EventSourcePosition

instance ToJSON EventSourcePosition where
    toJSON = toJSONText

data InvocationType
    = DryRun
    | Event
    | RequestResponse
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText InvocationType where
    parser = takeLowerText >>= \case
        "dryrun" -> pure DryRun
        "event" -> pure Event
        "requestresponse" -> pure RequestResponse
        e -> fromTextError $ "Failure parsing InvocationType from value: '" <> e
           <> "'. Accepted values: DryRun, Event, RequestResponse"

instance ToText InvocationType where
    toText = \case
        DryRun -> "DryRun"
        Event -> "Event"
        RequestResponse -> "RequestResponse"

instance Hashable     InvocationType
instance ToByteString InvocationType
instance ToQuery      InvocationType
instance ToHeader     InvocationType

instance ToJSON InvocationType where
    toJSON = toJSONText

data LogType
    = None
    | Tail
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText LogType where
    parser = takeLowerText >>= \case
        "none" -> pure None
        "tail" -> pure Tail
        e -> fromTextError $ "Failure parsing LogType from value: '" <> e
           <> "'. Accepted values: None, Tail"

instance ToText LogType where
    toText = \case
        None -> "None"
        Tail -> "Tail"

instance Hashable     LogType
instance ToByteString LogType
instance ToQuery      LogType
instance ToHeader     LogType

instance ToJSON LogType where
    toJSON = toJSONText

data Runtime
    = JAVA8
    | Nodejs
    | PYTHON2_7
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Runtime where
    parser = takeLowerText >>= \case
        "java8" -> pure JAVA8
        "nodejs" -> pure Nodejs
        "python2.7" -> pure PYTHON2_7
        e -> fromTextError $ "Failure parsing Runtime from value: '" <> e
           <> "'. Accepted values: java8, nodejs, python2.7"

instance ToText Runtime where
    toText = \case
        JAVA8 -> "java8"
        Nodejs -> "nodejs"
        PYTHON2_7 -> "python2.7"

instance Hashable     Runtime
instance ToByteString Runtime
instance ToQuery      Runtime
instance ToHeader     Runtime

instance ToJSON Runtime where
    toJSON = toJSONText

instance FromJSON Runtime where
    parseJSON = parseJSONText "Runtime"
