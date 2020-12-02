{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.Sum where

import Network.AWS.Prelude

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

data PolicyType =
  TargetTrackingScaling
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PolicyType where
    parser = takeLowerText >>= \case
        "targettrackingscaling" -> pure TargetTrackingScaling
        e -> fromTextError $ "Failure parsing PolicyType from value: '" <> e
           <> "'. Accepted values: targettrackingscaling"

instance ToText PolicyType where
    toText = \case
        TargetTrackingScaling -> "TargetTrackingScaling"

instance Hashable     PolicyType
instance NFData       PolicyType
instance ToByteString PolicyType
instance ToQuery      PolicyType
instance ToHeader     PolicyType

instance FromJSON PolicyType where
    parseJSON = parseJSONText "PolicyType"

data ScalableDimension
  = AutoscalingAutoScalingGroupDesiredCapacity
  | DynamodbIndexReadCapacityUnits
  | DynamodbIndexWriteCapacityUnits
  | DynamodbTableReadCapacityUnits
  | DynamodbTableWriteCapacityUnits
  | EC2SpotFleetRequestTargetCapacity
  | EcsServiceDesiredCount
  | RDSClusterReadReplicaCount
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalableDimension where
    parser = takeLowerText >>= \case
        "autoscaling:autoscalinggroup:desiredcapacity" -> pure AutoscalingAutoScalingGroupDesiredCapacity
        "dynamodb:index:readcapacityunits" -> pure DynamodbIndexReadCapacityUnits
        "dynamodb:index:writecapacityunits" -> pure DynamodbIndexWriteCapacityUnits
        "dynamodb:table:readcapacityunits" -> pure DynamodbTableReadCapacityUnits
        "dynamodb:table:writecapacityunits" -> pure DynamodbTableWriteCapacityUnits
        "ec2:spot-fleet-request:targetcapacity" -> pure EC2SpotFleetRequestTargetCapacity
        "ecs:service:desiredcount" -> pure EcsServiceDesiredCount
        "rds:cluster:readreplicacount" -> pure RDSClusterReadReplicaCount
        e -> fromTextError $ "Failure parsing ScalableDimension from value: '" <> e
           <> "'. Accepted values: autoscaling:autoscalinggroup:desiredcapacity, dynamodb:index:readcapacityunits, dynamodb:index:writecapacityunits, dynamodb:table:readcapacityunits, dynamodb:table:writecapacityunits, ec2:spot-fleet-request:targetcapacity, ecs:service:desiredcount, rds:cluster:readreplicacount"

instance ToText ScalableDimension where
    toText = \case
        AutoscalingAutoScalingGroupDesiredCapacity -> "autoscaling:autoScalingGroup:DesiredCapacity"
        DynamodbIndexReadCapacityUnits -> "dynamodb:index:ReadCapacityUnits"
        DynamodbIndexWriteCapacityUnits -> "dynamodb:index:WriteCapacityUnits"
        DynamodbTableReadCapacityUnits -> "dynamodb:table:ReadCapacityUnits"
        DynamodbTableWriteCapacityUnits -> "dynamodb:table:WriteCapacityUnits"
        EC2SpotFleetRequestTargetCapacity -> "ec2:spot-fleet-request:TargetCapacity"
        EcsServiceDesiredCount -> "ecs:service:DesiredCount"
        RDSClusterReadReplicaCount -> "rds:cluster:ReadReplicaCount"

instance Hashable     ScalableDimension
instance NFData       ScalableDimension
instance ToByteString ScalableDimension
instance ToQuery      ScalableDimension
instance ToHeader     ScalableDimension

instance ToJSON ScalableDimension where
    toJSON = toJSONText

instance FromJSON ScalableDimension where
    parseJSON = parseJSONText "ScalableDimension"

data ScalingMetricType
  = ALBRequestCountPerTarget
  | ASGAverageCPUUtilization
  | ASGAverageNetworkIn
  | ASGAverageNetworkOut
  | DynamoDBReadCapacityUtilization
  | DynamoDBWriteCapacityUtilization
  | EC2SpotFleetRequestAverageCPUUtilization
  | EC2SpotFleetRequestAverageNetworkIn
  | EC2SpotFleetRequestAverageNetworkOut
  | ECSServiceAverageCPUUtilization
  | ECSServiceAverageMemoryUtilization
  | RDSReaderAverageCPUUtilization
  | RDSReaderAverageDatabaseConnections
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalingMetricType where
    parser = takeLowerText >>= \case
        "albrequestcountpertarget" -> pure ALBRequestCountPerTarget
        "asgaveragecpuutilization" -> pure ASGAverageCPUUtilization
        "asgaveragenetworkin" -> pure ASGAverageNetworkIn
        "asgaveragenetworkout" -> pure ASGAverageNetworkOut
        "dynamodbreadcapacityutilization" -> pure DynamoDBReadCapacityUtilization
        "dynamodbwritecapacityutilization" -> pure DynamoDBWriteCapacityUtilization
        "ec2spotfleetrequestaveragecpuutilization" -> pure EC2SpotFleetRequestAverageCPUUtilization
        "ec2spotfleetrequestaveragenetworkin" -> pure EC2SpotFleetRequestAverageNetworkIn
        "ec2spotfleetrequestaveragenetworkout" -> pure EC2SpotFleetRequestAverageNetworkOut
        "ecsserviceaveragecpuutilization" -> pure ECSServiceAverageCPUUtilization
        "ecsserviceaveragememoryutilization" -> pure ECSServiceAverageMemoryUtilization
        "rdsreaderaveragecpuutilization" -> pure RDSReaderAverageCPUUtilization
        "rdsreaderaveragedatabaseconnections" -> pure RDSReaderAverageDatabaseConnections
        e -> fromTextError $ "Failure parsing ScalingMetricType from value: '" <> e
           <> "'. Accepted values: albrequestcountpertarget, asgaveragecpuutilization, asgaveragenetworkin, asgaveragenetworkout, dynamodbreadcapacityutilization, dynamodbwritecapacityutilization, ec2spotfleetrequestaveragecpuutilization, ec2spotfleetrequestaveragenetworkin, ec2spotfleetrequestaveragenetworkout, ecsserviceaveragecpuutilization, ecsserviceaveragememoryutilization, rdsreaderaveragecpuutilization, rdsreaderaveragedatabaseconnections"

instance ToText ScalingMetricType where
    toText = \case
        ALBRequestCountPerTarget -> "ALBRequestCountPerTarget"
        ASGAverageCPUUtilization -> "ASGAverageCPUUtilization"
        ASGAverageNetworkIn -> "ASGAverageNetworkIn"
        ASGAverageNetworkOut -> "ASGAverageNetworkOut"
        DynamoDBReadCapacityUtilization -> "DynamoDBReadCapacityUtilization"
        DynamoDBWriteCapacityUtilization -> "DynamoDBWriteCapacityUtilization"
        EC2SpotFleetRequestAverageCPUUtilization -> "EC2SpotFleetRequestAverageCPUUtilization"
        EC2SpotFleetRequestAverageNetworkIn -> "EC2SpotFleetRequestAverageNetworkIn"
        EC2SpotFleetRequestAverageNetworkOut -> "EC2SpotFleetRequestAverageNetworkOut"
        ECSServiceAverageCPUUtilization -> "ECSServiceAverageCPUUtilization"
        ECSServiceAverageMemoryUtilization -> "ECSServiceAverageMemoryUtilization"
        RDSReaderAverageCPUUtilization -> "RDSReaderAverageCPUUtilization"
        RDSReaderAverageDatabaseConnections -> "RDSReaderAverageDatabaseConnections"

instance Hashable     ScalingMetricType
instance NFData       ScalingMetricType
instance ToByteString ScalingMetricType
instance ToQuery      ScalingMetricType
instance ToHeader     ScalingMetricType

instance ToJSON ScalingMetricType where
    toJSON = toJSONText

instance FromJSON ScalingMetricType where
    parseJSON = parseJSONText "ScalingMetricType"

data ScalingPlanStatusCode
  = SPSCActive
  | SPSCActiveWithProblems
  | SPSCCreationFailed
  | SPSCCreationInProgress
  | SPSCDeletionFailed
  | SPSCDeletionInProgress
  | SPSCUpdateFailed
  | SPSCUpdateInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalingPlanStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure SPSCActive
        "activewithproblems" -> pure SPSCActiveWithProblems
        "creationfailed" -> pure SPSCCreationFailed
        "creationinprogress" -> pure SPSCCreationInProgress
        "deletionfailed" -> pure SPSCDeletionFailed
        "deletioninprogress" -> pure SPSCDeletionInProgress
        "updatefailed" -> pure SPSCUpdateFailed
        "updateinprogress" -> pure SPSCUpdateInProgress
        e -> fromTextError $ "Failure parsing ScalingPlanStatusCode from value: '" <> e
           <> "'. Accepted values: active, activewithproblems, creationfailed, creationinprogress, deletionfailed, deletioninprogress, updatefailed, updateinprogress"

instance ToText ScalingPlanStatusCode where
    toText = \case
        SPSCActive -> "Active"
        SPSCActiveWithProblems -> "ActiveWithProblems"
        SPSCCreationFailed -> "CreationFailed"
        SPSCCreationInProgress -> "CreationInProgress"
        SPSCDeletionFailed -> "DeletionFailed"
        SPSCDeletionInProgress -> "DeletionInProgress"
        SPSCUpdateFailed -> "UpdateFailed"
        SPSCUpdateInProgress -> "UpdateInProgress"

instance Hashable     ScalingPlanStatusCode
instance NFData       ScalingPlanStatusCode
instance ToByteString ScalingPlanStatusCode
instance ToQuery      ScalingPlanStatusCode
instance ToHeader     ScalingPlanStatusCode

instance FromJSON ScalingPlanStatusCode where
    parseJSON = parseJSONText "ScalingPlanStatusCode"

data ScalingStatusCode
  = Active
  | Inactive
  | PartiallyActive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalingStatusCode where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        "partiallyactive" -> pure PartiallyActive
        e -> fromTextError $ "Failure parsing ScalingStatusCode from value: '" <> e
           <> "'. Accepted values: active, inactive, partiallyactive"

instance ToText ScalingStatusCode where
    toText = \case
        Active -> "Active"
        Inactive -> "Inactive"
        PartiallyActive -> "PartiallyActive"

instance Hashable     ScalingStatusCode
instance NFData       ScalingStatusCode
instance ToByteString ScalingStatusCode
instance ToQuery      ScalingStatusCode
instance ToHeader     ScalingStatusCode

instance FromJSON ScalingStatusCode where
    parseJSON = parseJSONText "ScalingStatusCode"

data ServiceNamespace
  = Autoscaling
  | Dynamodb
  | EC2
  | Ecs
  | RDS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceNamespace where
    parser = takeLowerText >>= \case
        "autoscaling" -> pure Autoscaling
        "dynamodb" -> pure Dynamodb
        "ec2" -> pure EC2
        "ecs" -> pure Ecs
        "rds" -> pure RDS
        e -> fromTextError $ "Failure parsing ServiceNamespace from value: '" <> e
           <> "'. Accepted values: autoscaling, dynamodb, ec2, ecs, rds"

instance ToText ServiceNamespace where
    toText = \case
        Autoscaling -> "autoscaling"
        Dynamodb -> "dynamodb"
        EC2 -> "ec2"
        Ecs -> "ecs"
        RDS -> "rds"

instance Hashable     ServiceNamespace
instance NFData       ServiceNamespace
instance ToByteString ServiceNamespace
instance ToQuery      ServiceNamespace
instance ToHeader     ServiceNamespace

instance ToJSON ServiceNamespace where
    toJSON = toJSONText

instance FromJSON ServiceNamespace where
    parseJSON = parseJSONText "ServiceNamespace"
