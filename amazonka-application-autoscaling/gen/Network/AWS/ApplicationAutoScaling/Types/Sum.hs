{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.Sum where

import           Network.AWS.Prelude

data AdjustmentType
    = ChangeInCapacity
    | ExactCapacity
    | PercentChangeInCapacity
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AdjustmentType where
    parser = takeLowerText >>= \case
        "changeincapacity" -> pure ChangeInCapacity
        "exactcapacity" -> pure ExactCapacity
        "percentchangeincapacity" -> pure PercentChangeInCapacity
        e -> fromTextError $ "Failure parsing AdjustmentType from value: '" <> e
           <> "'. Accepted values: ChangeInCapacity, ExactCapacity, PercentChangeInCapacity"

instance ToText AdjustmentType where
    toText = \case
        ChangeInCapacity -> "ChangeInCapacity"
        ExactCapacity -> "ExactCapacity"
        PercentChangeInCapacity -> "PercentChangeInCapacity"

instance Hashable     AdjustmentType
instance NFData       AdjustmentType
instance ToByteString AdjustmentType
instance ToQuery      AdjustmentType
instance ToHeader     AdjustmentType

instance ToJSON AdjustmentType where
    toJSON = toJSONText

instance FromJSON AdjustmentType where
    parseJSON = parseJSONText "AdjustmentType"

data MetricAggregationType
    = Average
    | Maximum
    | Minimum
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText MetricAggregationType where
    parser = takeLowerText >>= \case
        "average" -> pure Average
        "maximum" -> pure Maximum
        "minimum" -> pure Minimum
        e -> fromTextError $ "Failure parsing MetricAggregationType from value: '" <> e
           <> "'. Accepted values: Average, Maximum, Minimum"

instance ToText MetricAggregationType where
    toText = \case
        Average -> "Average"
        Maximum -> "Maximum"
        Minimum -> "Minimum"

instance Hashable     MetricAggregationType
instance NFData       MetricAggregationType
instance ToByteString MetricAggregationType
instance ToQuery      MetricAggregationType
instance ToHeader     MetricAggregationType

instance ToJSON MetricAggregationType where
    toJSON = toJSONText

instance FromJSON MetricAggregationType where
    parseJSON = parseJSONText "MetricAggregationType"

data PolicyType =
    StepScaling
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText PolicyType where
    parser = takeLowerText >>= \case
        "stepscaling" -> pure StepScaling
        e -> fromTextError $ "Failure parsing PolicyType from value: '" <> e
           <> "'. Accepted values: StepScaling"

instance ToText PolicyType where
    toText = \case
        StepScaling -> "StepScaling"

instance Hashable     PolicyType
instance NFData       PolicyType
instance ToByteString PolicyType
instance ToQuery      PolicyType
instance ToHeader     PolicyType

instance ToJSON PolicyType where
    toJSON = toJSONText

instance FromJSON PolicyType where
    parseJSON = parseJSONText "PolicyType"

data ScalableDimension =
    EcsServiceDesiredCount
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ScalableDimension where
    parser = takeLowerText >>= \case
        "ecs:service:desiredcount" -> pure EcsServiceDesiredCount
        e -> fromTextError $ "Failure parsing ScalableDimension from value: '" <> e
           <> "'. Accepted values: ecs:service:DesiredCount"

instance ToText ScalableDimension where
    toText = \case
        EcsServiceDesiredCount -> "ecs:service:DesiredCount"

instance Hashable     ScalableDimension
instance NFData       ScalableDimension
instance ToByteString ScalableDimension
instance ToQuery      ScalableDimension
instance ToHeader     ScalableDimension

instance ToJSON ScalableDimension where
    toJSON = toJSONText

instance FromJSON ScalableDimension where
    parseJSON = parseJSONText "ScalableDimension"

data ScalingActivityStatusCode
    = Failed
    | InProgress
    | Overridden
    | Pending
    | Successful
    | Unfulfilled
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ScalingActivityStatusCode where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "overridden" -> pure Overridden
        "pending" -> pure Pending
        "successful" -> pure Successful
        "unfulfilled" -> pure Unfulfilled
        e -> fromTextError $ "Failure parsing ScalingActivityStatusCode from value: '" <> e
           <> "'. Accepted values: Failed, InProgress, Overridden, Pending, Successful, Unfulfilled"

instance ToText ScalingActivityStatusCode where
    toText = \case
        Failed -> "Failed"
        InProgress -> "InProgress"
        Overridden -> "Overridden"
        Pending -> "Pending"
        Successful -> "Successful"
        Unfulfilled -> "Unfulfilled"

instance Hashable     ScalingActivityStatusCode
instance NFData       ScalingActivityStatusCode
instance ToByteString ScalingActivityStatusCode
instance ToQuery      ScalingActivityStatusCode
instance ToHeader     ScalingActivityStatusCode

instance FromJSON ScalingActivityStatusCode where
    parseJSON = parseJSONText "ScalingActivityStatusCode"

data ServiceNamespace =
    Ecs
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ServiceNamespace where
    parser = takeLowerText >>= \case
        "ecs" -> pure Ecs
        e -> fromTextError $ "Failure parsing ServiceNamespace from value: '" <> e
           <> "'. Accepted values: ecs"

instance ToText ServiceNamespace where
    toText = \case
        Ecs -> "ecs"

instance Hashable     ServiceNamespace
instance NFData       ServiceNamespace
instance ToByteString ServiceNamespace
instance ToQuery      ServiceNamespace
instance ToHeader     ServiceNamespace

instance ToJSON ServiceNamespace where
    toJSON = toJSONText

instance FromJSON ServiceNamespace where
    parseJSON = parseJSONText "ServiceNamespace"
