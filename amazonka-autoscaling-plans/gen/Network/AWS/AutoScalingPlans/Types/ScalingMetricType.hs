{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingMetricType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingMetricType
  ( ScalingMetricType
      ( ..,
        ScalingMetricType_ALBRequestCountPerTarget,
        ScalingMetricType_ASGAverageCPUUtilization,
        ScalingMetricType_ASGAverageNetworkIn,
        ScalingMetricType_ASGAverageNetworkOut,
        ScalingMetricType_DynamoDBReadCapacityUtilization,
        ScalingMetricType_DynamoDBWriteCapacityUtilization,
        ScalingMetricType_EC2SpotFleetRequestAverageCPUUtilization,
        ScalingMetricType_EC2SpotFleetRequestAverageNetworkIn,
        ScalingMetricType_EC2SpotFleetRequestAverageNetworkOut,
        ScalingMetricType_ECSServiceAverageCPUUtilization,
        ScalingMetricType_ECSServiceAverageMemoryUtilization,
        ScalingMetricType_RDSReaderAverageCPUUtilization,
        ScalingMetricType_RDSReaderAverageDatabaseConnections
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ScalingMetricType = ScalingMetricType'
  { fromScalingMetricType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ScalingMetricType_ALBRequestCountPerTarget :: ScalingMetricType
pattern ScalingMetricType_ALBRequestCountPerTarget = ScalingMetricType' "ALBRequestCountPerTarget"

pattern ScalingMetricType_ASGAverageCPUUtilization :: ScalingMetricType
pattern ScalingMetricType_ASGAverageCPUUtilization = ScalingMetricType' "ASGAverageCPUUtilization"

pattern ScalingMetricType_ASGAverageNetworkIn :: ScalingMetricType
pattern ScalingMetricType_ASGAverageNetworkIn = ScalingMetricType' "ASGAverageNetworkIn"

pattern ScalingMetricType_ASGAverageNetworkOut :: ScalingMetricType
pattern ScalingMetricType_ASGAverageNetworkOut = ScalingMetricType' "ASGAverageNetworkOut"

pattern ScalingMetricType_DynamoDBReadCapacityUtilization :: ScalingMetricType
pattern ScalingMetricType_DynamoDBReadCapacityUtilization = ScalingMetricType' "DynamoDBReadCapacityUtilization"

pattern ScalingMetricType_DynamoDBWriteCapacityUtilization :: ScalingMetricType
pattern ScalingMetricType_DynamoDBWriteCapacityUtilization = ScalingMetricType' "DynamoDBWriteCapacityUtilization"

pattern ScalingMetricType_EC2SpotFleetRequestAverageCPUUtilization :: ScalingMetricType
pattern ScalingMetricType_EC2SpotFleetRequestAverageCPUUtilization = ScalingMetricType' "EC2SpotFleetRequestAverageCPUUtilization"

pattern ScalingMetricType_EC2SpotFleetRequestAverageNetworkIn :: ScalingMetricType
pattern ScalingMetricType_EC2SpotFleetRequestAverageNetworkIn = ScalingMetricType' "EC2SpotFleetRequestAverageNetworkIn"

pattern ScalingMetricType_EC2SpotFleetRequestAverageNetworkOut :: ScalingMetricType
pattern ScalingMetricType_EC2SpotFleetRequestAverageNetworkOut = ScalingMetricType' "EC2SpotFleetRequestAverageNetworkOut"

pattern ScalingMetricType_ECSServiceAverageCPUUtilization :: ScalingMetricType
pattern ScalingMetricType_ECSServiceAverageCPUUtilization = ScalingMetricType' "ECSServiceAverageCPUUtilization"

pattern ScalingMetricType_ECSServiceAverageMemoryUtilization :: ScalingMetricType
pattern ScalingMetricType_ECSServiceAverageMemoryUtilization = ScalingMetricType' "ECSServiceAverageMemoryUtilization"

pattern ScalingMetricType_RDSReaderAverageCPUUtilization :: ScalingMetricType
pattern ScalingMetricType_RDSReaderAverageCPUUtilization = ScalingMetricType' "RDSReaderAverageCPUUtilization"

pattern ScalingMetricType_RDSReaderAverageDatabaseConnections :: ScalingMetricType
pattern ScalingMetricType_RDSReaderAverageDatabaseConnections = ScalingMetricType' "RDSReaderAverageDatabaseConnections"

{-# COMPLETE
  ScalingMetricType_ALBRequestCountPerTarget,
  ScalingMetricType_ASGAverageCPUUtilization,
  ScalingMetricType_ASGAverageNetworkIn,
  ScalingMetricType_ASGAverageNetworkOut,
  ScalingMetricType_DynamoDBReadCapacityUtilization,
  ScalingMetricType_DynamoDBWriteCapacityUtilization,
  ScalingMetricType_EC2SpotFleetRequestAverageCPUUtilization,
  ScalingMetricType_EC2SpotFleetRequestAverageNetworkIn,
  ScalingMetricType_EC2SpotFleetRequestAverageNetworkOut,
  ScalingMetricType_ECSServiceAverageCPUUtilization,
  ScalingMetricType_ECSServiceAverageMemoryUtilization,
  ScalingMetricType_RDSReaderAverageCPUUtilization,
  ScalingMetricType_RDSReaderAverageDatabaseConnections,
  ScalingMetricType'
  #-}
