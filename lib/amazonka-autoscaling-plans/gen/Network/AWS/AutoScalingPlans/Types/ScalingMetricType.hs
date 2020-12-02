{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingMetricType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingMetricType where

import Network.AWS.Prelude

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
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ScalingMetricType where
  parser =
    takeLowerText >>= \case
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
      e ->
        fromTextError $
          "Failure parsing ScalingMetricType from value: '" <> e
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

instance Hashable ScalingMetricType

instance NFData ScalingMetricType

instance ToByteString ScalingMetricType

instance ToQuery ScalingMetricType

instance ToHeader ScalingMetricType

instance ToJSON ScalingMetricType where
  toJSON = toJSONText

instance FromJSON ScalingMetricType where
  parseJSON = parseJSONText "ScalingMetricType"
