{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.Sum where

import Network.AWS.Prelude

data ComparisonOperator
  = GreaterThanOrEqualToThreshold
  | GreaterThanThreshold
  | LessThanOrEqualToThreshold
  | LessThanThreshold
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
        GreaterThanOrEqualToThreshold -> "GreaterThanOrEqualToThreshold"
        GreaterThanThreshold -> "GreaterThanThreshold"
        LessThanOrEqualToThreshold -> "LessThanOrEqualToThreshold"
        LessThanThreshold -> "LessThanThreshold"

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance FromXML ComparisonOperator where
    parseXML = parseXMLText "ComparisonOperator"

data HistoryItemType
  = Action
  | ConfigurationUpdate
  | StateUpdate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HistoryItemType where
    parser = takeLowerText >>= \case
        "action" -> pure Action
        "configurationupdate" -> pure ConfigurationUpdate
        "stateupdate" -> pure StateUpdate
        e -> fromTextError $ "Failure parsing HistoryItemType from value: '" <> e
           <> "'. Accepted values: action, configurationupdate, stateupdate"

instance ToText HistoryItemType where
    toText = \case
        Action -> "Action"
        ConfigurationUpdate -> "ConfigurationUpdate"
        StateUpdate -> "StateUpdate"

instance Hashable     HistoryItemType
instance NFData       HistoryItemType
instance ToByteString HistoryItemType
instance ToQuery      HistoryItemType
instance ToHeader     HistoryItemType

instance FromXML HistoryItemType where
    parseXML = parseXMLText "HistoryItemType"

data ScanBy
  = TimestampAscending
  | TimestampDescending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScanBy where
    parser = takeLowerText >>= \case
        "timestampascending" -> pure TimestampAscending
        "timestampdescending" -> pure TimestampDescending
        e -> fromTextError $ "Failure parsing ScanBy from value: '" <> e
           <> "'. Accepted values: timestampascending, timestampdescending"

instance ToText ScanBy where
    toText = \case
        TimestampAscending -> "TimestampAscending"
        TimestampDescending -> "TimestampDescending"

instance Hashable     ScanBy
instance NFData       ScanBy
instance ToByteString ScanBy
instance ToQuery      ScanBy
instance ToHeader     ScanBy

data StandardUnit
  = Bits
  | BitsSecond
  | Bytes
  | BytesSecond
  | Count
  | CountSecond
  | Gigabits
  | GigabitsSecond
  | Gigabytes
  | GigabytesSecond
  | Kilobits
  | KilobitsSecond
  | Kilobytes
  | KilobytesSecond
  | Megabits
  | MegabitsSecond
  | Megabytes
  | MegabytesSecond
  | Microseconds
  | Milliseconds
  | None
  | Percent
  | Seconds
  | Terabits
  | TerabitsSecond
  | Terabytes
  | TerabytesSecond
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
        Bits -> "Bits"
        BitsSecond -> "Bits/Second"
        Bytes -> "Bytes"
        BytesSecond -> "Bytes/Second"
        Count -> "Count"
        CountSecond -> "Count/Second"
        Gigabits -> "Gigabits"
        GigabitsSecond -> "Gigabits/Second"
        Gigabytes -> "Gigabytes"
        GigabytesSecond -> "Gigabytes/Second"
        Kilobits -> "Kilobits"
        KilobitsSecond -> "Kilobits/Second"
        Kilobytes -> "Kilobytes"
        KilobytesSecond -> "Kilobytes/Second"
        Megabits -> "Megabits"
        MegabitsSecond -> "Megabits/Second"
        Megabytes -> "Megabytes"
        MegabytesSecond -> "Megabytes/Second"
        Microseconds -> "Microseconds"
        Milliseconds -> "Milliseconds"
        None -> "None"
        Percent -> "Percent"
        Seconds -> "Seconds"
        Terabits -> "Terabits"
        TerabitsSecond -> "Terabits/Second"
        Terabytes -> "Terabytes"
        TerabytesSecond -> "Terabytes/Second"

instance Hashable     StandardUnit
instance NFData       StandardUnit
instance ToByteString StandardUnit
instance ToQuery      StandardUnit
instance ToHeader     StandardUnit

instance FromXML StandardUnit where
    parseXML = parseXMLText "StandardUnit"

data StateValue
  = Alarm
  | InsufficientData
  | OK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StateValue where
    parser = takeLowerText >>= \case
        "alarm" -> pure Alarm
        "insufficient_data" -> pure InsufficientData
        "ok" -> pure OK
        e -> fromTextError $ "Failure parsing StateValue from value: '" <> e
           <> "'. Accepted values: alarm, insufficient_data, ok"

instance ToText StateValue where
    toText = \case
        Alarm -> "ALARM"
        InsufficientData -> "INSUFFICIENT_DATA"
        OK -> "OK"

instance Hashable     StateValue
instance NFData       StateValue
instance ToByteString StateValue
instance ToQuery      StateValue
instance ToHeader     StateValue

instance FromXML StateValue where
    parseXML = parseXMLText "StateValue"

data Statistic
  = Average
  | Maximum
  | Minimum
  | SampleCount
  | Sum
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
        Average -> "Average"
        Maximum -> "Maximum"
        Minimum -> "Minimum"
        SampleCount -> "SampleCount"
        Sum -> "Sum"

instance Hashable     Statistic
instance NFData       Statistic
instance ToByteString Statistic
instance ToQuery      Statistic
instance ToHeader     Statistic

instance FromXML Statistic where
    parseXML = parseXMLText "Statistic"

data StatusCode
  = Complete
  | InternalError
  | PartialData
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StatusCode where
    parser = takeLowerText >>= \case
        "complete" -> pure Complete
        "internalerror" -> pure InternalError
        "partialdata" -> pure PartialData
        e -> fromTextError $ "Failure parsing StatusCode from value: '" <> e
           <> "'. Accepted values: complete, internalerror, partialdata"

instance ToText StatusCode where
    toText = \case
        Complete -> "Complete"
        InternalError -> "InternalError"
        PartialData -> "PartialData"

instance Hashable     StatusCode
instance NFData       StatusCode
instance ToByteString StatusCode
instance ToQuery      StatusCode
instance ToHeader     StatusCode

instance FromXML StatusCode where
    parseXML = parseXMLText "StatusCode"
