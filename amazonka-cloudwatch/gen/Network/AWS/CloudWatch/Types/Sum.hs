{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.Sum where

import           Network.AWS.Prelude

data ComparisonOperator
    = GreaterThanOrEqualToThreshold
    | GreaterThanThreshold
    | LessThanOrEqualToThreshold
    | LessThanThreshold
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "greaterthanorequaltothreshold" -> pure GreaterThanOrEqualToThreshold
        "greaterthanthreshold" -> pure GreaterThanThreshold
        "lessthanorequaltothreshold" -> pure LessThanOrEqualToThreshold
        "lessthanthreshold" -> pure LessThanThreshold
        e -> fromTextError $ "Failure parsing ComparisonOperator from value: '" <> e
           <> "'. Accepted values: greaterthanorequaltothreshold, greaterthanthreshold, lessthanorequaltothreshold, lessthanthreshold"

instance ToText ComparisonOperator where
    toText = \case
        GreaterThanOrEqualToThreshold -> "greaterthanorequaltothreshold"
        GreaterThanThreshold -> "greaterthanthreshold"
        LessThanOrEqualToThreshold -> "lessthanorequaltothreshold"
        LessThanThreshold -> "lessthanthreshold"

instance Hashable     ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance FromXML ComparisonOperator where
    parseXML = parseXMLText "ComparisonOperator"

data HistoryItemType
    = StateUpdate
    | Action
    | ConfigurationUpdate
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText HistoryItemType where
    parser = takeLowerText >>= \case
        "action" -> pure Action
        "configurationupdate" -> pure ConfigurationUpdate
        "stateupdate" -> pure StateUpdate
        e -> fromTextError $ "Failure parsing HistoryItemType from value: '" <> e
           <> "'. Accepted values: action, configurationupdate, stateupdate"

instance ToText HistoryItemType where
    toText = \case
        Action -> "action"
        ConfigurationUpdate -> "configurationupdate"
        StateUpdate -> "stateupdate"

instance Hashable     HistoryItemType
instance ToByteString HistoryItemType
instance ToQuery      HistoryItemType
instance ToHeader     HistoryItemType

instance FromXML HistoryItemType where
    parseXML = parseXMLText "HistoryItemType"

data StandardUnit
    = Bits
    | BitsSecond
    | MegabytesSecond
    | Megabytes
    | None
    | Count
    | Terabytes
    | TerabytesSecond
    | Percent
    | CountSecond
    | TerabitsSecond
    | Terabits
    | Milliseconds
    | GigabytesSecond
    | Microseconds
    | Gigabytes
    | GigabitsSecond
    | Gigabits
    | Megabits
    | MegabitsSecond
    | Kilobits
    | KilobitsSecond
    | Kilobytes
    | KilobytesSecond
    | Seconds
    | BytesSecond
    | Bytes
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StandardUnit where
    parser = takeLowerText >>= \case
        "bits" -> pure Bits
        "bits/second" -> pure BitsSecond
        "bytes" -> pure Bytes
        "bytes/second" -> pure BytesSecond
        "count" -> pure Count
        "count/second" -> pure CountSecond
        "gigabits" -> pure Gigabits
        "gigabits/second" -> pure GigabitsSecond
        "gigabytes" -> pure Gigabytes
        "gigabytes/second" -> pure GigabytesSecond
        "kilobits" -> pure Kilobits
        "kilobits/second" -> pure KilobitsSecond
        "kilobytes" -> pure Kilobytes
        "kilobytes/second" -> pure KilobytesSecond
        "megabits" -> pure Megabits
        "megabits/second" -> pure MegabitsSecond
        "megabytes" -> pure Megabytes
        "megabytes/second" -> pure MegabytesSecond
        "microseconds" -> pure Microseconds
        "milliseconds" -> pure Milliseconds
        "none" -> pure None
        "percent" -> pure Percent
        "seconds" -> pure Seconds
        "terabits" -> pure Terabits
        "terabits/second" -> pure TerabitsSecond
        "terabytes" -> pure Terabytes
        "terabytes/second" -> pure TerabytesSecond
        e -> fromTextError $ "Failure parsing StandardUnit from value: '" <> e
           <> "'. Accepted values: bits, bits/second, bytes, bytes/second, count, count/second, gigabits, gigabits/second, gigabytes, gigabytes/second, kilobits, kilobits/second, kilobytes, kilobytes/second, megabits, megabits/second, megabytes, megabytes/second, microseconds, milliseconds, none, percent, seconds, terabits, terabits/second, terabytes, terabytes/second"

instance ToText StandardUnit where
    toText = \case
        Bits -> "bits"
        BitsSecond -> "bits/second"
        Bytes -> "bytes"
        BytesSecond -> "bytes/second"
        Count -> "count"
        CountSecond -> "count/second"
        Gigabits -> "gigabits"
        GigabitsSecond -> "gigabits/second"
        Gigabytes -> "gigabytes"
        GigabytesSecond -> "gigabytes/second"
        Kilobits -> "kilobits"
        KilobitsSecond -> "kilobits/second"
        Kilobytes -> "kilobytes"
        KilobytesSecond -> "kilobytes/second"
        Megabits -> "megabits"
        MegabitsSecond -> "megabits/second"
        Megabytes -> "megabytes"
        MegabytesSecond -> "megabytes/second"
        Microseconds -> "microseconds"
        Milliseconds -> "milliseconds"
        None -> "none"
        Percent -> "percent"
        Seconds -> "seconds"
        Terabits -> "terabits"
        TerabitsSecond -> "terabits/second"
        Terabytes -> "terabytes"
        TerabytesSecond -> "terabytes/second"

instance Hashable     StandardUnit
instance ToByteString StandardUnit
instance ToQuery      StandardUnit
instance ToHeader     StandardUnit

instance FromXML StandardUnit where
    parseXML = parseXMLText "StandardUnit"

data StateValue
    = OK
    | InsufficientData
    | Alarm
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StateValue where
    parser = takeLowerText >>= \case
        "alarm" -> pure Alarm
        "insufficient_data" -> pure InsufficientData
        "ok" -> pure OK
        e -> fromTextError $ "Failure parsing StateValue from value: '" <> e
           <> "'. Accepted values: alarm, insufficient_data, ok"

instance ToText StateValue where
    toText = \case
        Alarm -> "alarm"
        InsufficientData -> "insufficient_data"
        OK -> "ok"

instance Hashable     StateValue
instance ToByteString StateValue
instance ToQuery      StateValue
instance ToHeader     StateValue

instance FromXML StateValue where
    parseXML = parseXMLText "StateValue"

data Statistic
    = SampleCount
    | Maximum
    | Average
    | Minimum
    | Sum
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Statistic where
    parser = takeLowerText >>= \case
        "average" -> pure Average
        "maximum" -> pure Maximum
        "minimum" -> pure Minimum
        "samplecount" -> pure SampleCount
        "sum" -> pure Sum
        e -> fromTextError $ "Failure parsing Statistic from value: '" <> e
           <> "'. Accepted values: average, maximum, minimum, samplecount, sum"

instance ToText Statistic where
    toText = \case
        Average -> "average"
        Maximum -> "maximum"
        Minimum -> "minimum"
        SampleCount -> "samplecount"
        Sum -> "sum"

instance Hashable     Statistic
instance ToByteString Statistic
instance ToQuery      Statistic
instance ToHeader     Statistic

instance FromXML Statistic where
    parseXML = parseXMLText "Statistic"
