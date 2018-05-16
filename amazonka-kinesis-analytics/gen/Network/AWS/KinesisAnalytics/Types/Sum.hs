{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.Sum where

import Network.AWS.Prelude

data ApplicationStatus
  = Deleting
  | Ready
  | Running
  | Starting
  | Stopping
  | Updating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ApplicationStatus where
    parser = takeLowerText >>= \case
        "deleting" -> pure Deleting
        "ready" -> pure Ready
        "running" -> pure Running
        "starting" -> pure Starting
        "stopping" -> pure Stopping
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing ApplicationStatus from value: '" <> e
           <> "'. Accepted values: deleting, ready, running, starting, stopping, updating"

instance ToText ApplicationStatus where
    toText = \case
        Deleting -> "DELETING"
        Ready -> "READY"
        Running -> "RUNNING"
        Starting -> "STARTING"
        Stopping -> "STOPPING"
        Updating -> "UPDATING"

instance Hashable     ApplicationStatus
instance NFData       ApplicationStatus
instance ToByteString ApplicationStatus
instance ToQuery      ApplicationStatus
instance ToHeader     ApplicationStatus

instance FromJSON ApplicationStatus where
    parseJSON = parseJSONText "ApplicationStatus"

data InputStartingPosition
  = LastStoppedPoint
  | Now
  | TrimHorizon
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputStartingPosition where
    parser = takeLowerText >>= \case
        "last_stopped_point" -> pure LastStoppedPoint
        "now" -> pure Now
        "trim_horizon" -> pure TrimHorizon
        e -> fromTextError $ "Failure parsing InputStartingPosition from value: '" <> e
           <> "'. Accepted values: last_stopped_point, now, trim_horizon"

instance ToText InputStartingPosition where
    toText = \case
        LastStoppedPoint -> "LAST_STOPPED_POINT"
        Now -> "NOW"
        TrimHorizon -> "TRIM_HORIZON"

instance Hashable     InputStartingPosition
instance NFData       InputStartingPosition
instance ToByteString InputStartingPosition
instance ToQuery      InputStartingPosition
instance ToHeader     InputStartingPosition

instance ToJSON InputStartingPosition where
    toJSON = toJSONText

instance FromJSON InputStartingPosition where
    parseJSON = parseJSONText "InputStartingPosition"

data RecordFormatType
  = CSV
  | JSON
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecordFormatType where
    parser = takeLowerText >>= \case
        "csv" -> pure CSV
        "json" -> pure JSON
        e -> fromTextError $ "Failure parsing RecordFormatType from value: '" <> e
           <> "'. Accepted values: csv, json"

instance ToText RecordFormatType where
    toText = \case
        CSV -> "CSV"
        JSON -> "JSON"

instance Hashable     RecordFormatType
instance NFData       RecordFormatType
instance ToByteString RecordFormatType
instance ToQuery      RecordFormatType
instance ToHeader     RecordFormatType

instance ToJSON RecordFormatType where
    toJSON = toJSONText

instance FromJSON RecordFormatType where
    parseJSON = parseJSONText "RecordFormatType"
