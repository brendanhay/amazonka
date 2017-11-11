{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.Sum
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.Sum where

import Network.AWS.Prelude

data AdjustmentType
  = ChangeInCapacity
  | ExactCapacity
  | PercentChangeInCapacity
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AdjustmentType where
    parser = takeLowerText >>= \case
        "changeincapacity" -> pure ChangeInCapacity
        "exactcapacity" -> pure ExactCapacity
        "percentchangeincapacity" -> pure PercentChangeInCapacity
        e -> fromTextError $ "Failure parsing AdjustmentType from value: '" <> e
           <> "'. Accepted values: changeincapacity, exactcapacity, percentchangeincapacity"

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
  = MATAverage
  | MATMaximum
  | MATMinimum
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetricAggregationType where
    parser = takeLowerText >>= \case
        "average" -> pure MATAverage
        "maximum" -> pure MATMaximum
        "minimum" -> pure MATMinimum
        e -> fromTextError $ "Failure parsing MetricAggregationType from value: '" <> e
           <> "'. Accepted values: average, maximum, minimum"

instance ToText MetricAggregationType where
    toText = \case
        MATAverage -> "Average"
        MATMaximum -> "Maximum"
        MATMinimum -> "Minimum"

instance Hashable     MetricAggregationType
instance NFData       MetricAggregationType
instance ToByteString MetricAggregationType
instance ToQuery      MetricAggregationType
instance ToHeader     MetricAggregationType

instance ToJSON MetricAggregationType where
    toJSON = toJSONText

instance FromJSON MetricAggregationType where
    parseJSON = parseJSONText "MetricAggregationType"

data MetricStatistic
  = Average
  | Maximum
  | Minimum
  | SampleCount
  | Sum
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetricStatistic where
    parser = takeLowerText >>= \case
        "average" -> pure Average
        "maximum" -> pure Maximum
        "minimum" -> pure Minimum
        "samplecount" -> pure SampleCount
        "sum" -> pure Sum
        e -> fromTextError $ "Failure parsing MetricStatistic from value: '" <> e
           <> "'. Accepted values: average, maximum, minimum, samplecount, sum"

instance ToText MetricStatistic where
    toText = \case
        Average -> "Average"
        Maximum -> "Maximum"
        Minimum -> "Minimum"
        SampleCount -> "SampleCount"
        Sum -> "Sum"

instance Hashable     MetricStatistic
instance NFData       MetricStatistic
instance ToByteString MetricStatistic
instance ToQuery      MetricStatistic
instance ToHeader     MetricStatistic

instance ToJSON MetricStatistic where
    toJSON = toJSONText

instance FromJSON MetricStatistic where
    parseJSON = parseJSONText "MetricStatistic"

data MetricType
  = DynamoDBReadCapacityUtilization
  | DynamoDBWriteCapacityUtilization
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetricType where
    parser = takeLowerText >>= \case
        "dynamodbreadcapacityutilization" -> pure DynamoDBReadCapacityUtilization
        "dynamodbwritecapacityutilization" -> pure DynamoDBWriteCapacityUtilization
        e -> fromTextError $ "Failure parsing MetricType from value: '" <> e
           <> "'. Accepted values: dynamodbreadcapacityutilization, dynamodbwritecapacityutilization"

instance ToText MetricType where
    toText = \case
        DynamoDBReadCapacityUtilization -> "DynamoDBReadCapacityUtilization"
        DynamoDBWriteCapacityUtilization -> "DynamoDBWriteCapacityUtilization"

instance Hashable     MetricType
instance NFData       MetricType
instance ToByteString MetricType
instance ToQuery      MetricType
instance ToHeader     MetricType

instance ToJSON MetricType where
    toJSON = toJSONText

instance FromJSON MetricType where
    parseJSON = parseJSONText "MetricType"

data PolicyType
  = StepScaling
  | TargetTrackingScaling
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PolicyType where
    parser = takeLowerText >>= \case
        "stepscaling" -> pure StepScaling
        "targettrackingscaling" -> pure TargetTrackingScaling
        e -> fromTextError $ "Failure parsing PolicyType from value: '" <> e
           <> "'. Accepted values: stepscaling, targettrackingscaling"

instance ToText PolicyType where
    toText = \case
        StepScaling -> "StepScaling"
        TargetTrackingScaling -> "TargetTrackingScaling"

instance Hashable     PolicyType
instance NFData       PolicyType
instance ToByteString PolicyType
instance ToQuery      PolicyType
instance ToHeader     PolicyType

instance ToJSON PolicyType where
    toJSON = toJSONText

instance FromJSON PolicyType where
    parseJSON = parseJSONText "PolicyType"

data ScalableDimension
  = AppstreamFleetDesiredCapacity
  | DynamodbIndexReadCapacityUnits
  | DynamodbIndexWriteCapacityUnits
  | DynamodbTableReadCapacityUnits
  | DynamodbTableWriteCapacityUnits
  | EC2SpotFleetRequestTargetCapacity
  | EcsServiceDesiredCount
  | ElasticmapreduceInstancegroupInstanceCount
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalableDimension where
    parser = takeLowerText >>= \case
        "appstream:fleet:desiredcapacity" -> pure AppstreamFleetDesiredCapacity
        "dynamodb:index:readcapacityunits" -> pure DynamodbIndexReadCapacityUnits
        "dynamodb:index:writecapacityunits" -> pure DynamodbIndexWriteCapacityUnits
        "dynamodb:table:readcapacityunits" -> pure DynamodbTableReadCapacityUnits
        "dynamodb:table:writecapacityunits" -> pure DynamodbTableWriteCapacityUnits
        "ec2:spot-fleet-request:targetcapacity" -> pure EC2SpotFleetRequestTargetCapacity
        "ecs:service:desiredcount" -> pure EcsServiceDesiredCount
        "elasticmapreduce:instancegroup:instancecount" -> pure ElasticmapreduceInstancegroupInstanceCount
        e -> fromTextError $ "Failure parsing ScalableDimension from value: '" <> e
           <> "'. Accepted values: appstream:fleet:desiredcapacity, dynamodb:index:readcapacityunits, dynamodb:index:writecapacityunits, dynamodb:table:readcapacityunits, dynamodb:table:writecapacityunits, ec2:spot-fleet-request:targetcapacity, ecs:service:desiredcount, elasticmapreduce:instancegroup:instancecount"

instance ToText ScalableDimension where
    toText = \case
        AppstreamFleetDesiredCapacity -> "appstream:fleet:DesiredCapacity"
        DynamodbIndexReadCapacityUnits -> "dynamodb:index:ReadCapacityUnits"
        DynamodbIndexWriteCapacityUnits -> "dynamodb:index:WriteCapacityUnits"
        DynamodbTableReadCapacityUnits -> "dynamodb:table:ReadCapacityUnits"
        DynamodbTableWriteCapacityUnits -> "dynamodb:table:WriteCapacityUnits"
        EC2SpotFleetRequestTargetCapacity -> "ec2:spot-fleet-request:TargetCapacity"
        EcsServiceDesiredCount -> "ecs:service:DesiredCount"
        ElasticmapreduceInstancegroupInstanceCount -> "elasticmapreduce:instancegroup:InstanceCount"

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalingActivityStatusCode where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "overridden" -> pure Overridden
        "pending" -> pure Pending
        "successful" -> pure Successful
        "unfulfilled" -> pure Unfulfilled
        e -> fromTextError $ "Failure parsing ScalingActivityStatusCode from value: '" <> e
           <> "'. Accepted values: failed, inprogress, overridden, pending, successful, unfulfilled"

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

data ServiceNamespace
  = Appstream
  | Dynamodb
  | EC2
  | Ecs
  | Elasticmapreduce
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceNamespace where
    parser = takeLowerText >>= \case
        "appstream" -> pure Appstream
        "dynamodb" -> pure Dynamodb
        "ec2" -> pure EC2
        "ecs" -> pure Ecs
        "elasticmapreduce" -> pure Elasticmapreduce
        e -> fromTextError $ "Failure parsing ServiceNamespace from value: '" <> e
           <> "'. Accepted values: appstream, dynamodb, ec2, ecs, elasticmapreduce"

instance ToText ServiceNamespace where
    toText = \case
        Appstream -> "appstream"
        Dynamodb -> "dynamodb"
        EC2 -> "ec2"
        Ecs -> "ecs"
        Elasticmapreduce -> "elasticmapreduce"

instance Hashable     ServiceNamespace
instance NFData       ServiceNamespace
instance ToByteString ServiceNamespace
instance ToQuery      ServiceNamespace
instance ToHeader     ServiceNamespace

instance ToJSON ServiceNamespace where
    toJSON = toJSONText

instance FromJSON ServiceNamespace where
    parseJSON = parseJSONText "ServiceNamespace"
