{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.Sum where

import Network.AWS.Prelude

data LifecycleState
  = Detached
  | Detaching
  | EnteringStandby
  | InService
  | Pending
  | PendingProceed
  | PendingWait
  | Quarantined
  | Standby
  | Terminated
  | Terminating
  | TerminatingProceed
  | TerminatingWait
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LifecycleState where
    parser = takeLowerText >>= \case
        "detached" -> pure Detached
        "detaching" -> pure Detaching
        "enteringstandby" -> pure EnteringStandby
        "inservice" -> pure InService
        "pending" -> pure Pending
        "pending:proceed" -> pure PendingProceed
        "pending:wait" -> pure PendingWait
        "quarantined" -> pure Quarantined
        "standby" -> pure Standby
        "terminated" -> pure Terminated
        "terminating" -> pure Terminating
        "terminating:proceed" -> pure TerminatingProceed
        "terminating:wait" -> pure TerminatingWait
        e -> fromTextError $ "Failure parsing LifecycleState from value: '" <> e
           <> "'. Accepted values: detached, detaching, enteringstandby, inservice, pending, pending:proceed, pending:wait, quarantined, standby, terminated, terminating, terminating:proceed, terminating:wait"

instance ToText LifecycleState where
    toText = \case
        Detached -> "Detached"
        Detaching -> "Detaching"
        EnteringStandby -> "EnteringStandby"
        InService -> "InService"
        Pending -> "Pending"
        PendingProceed -> "Pending:Proceed"
        PendingWait -> "Pending:Wait"
        Quarantined -> "Quarantined"
        Standby -> "Standby"
        Terminated -> "Terminated"
        Terminating -> "Terminating"
        TerminatingProceed -> "Terminating:Proceed"
        TerminatingWait -> "Terminating:Wait"

instance Hashable     LifecycleState
instance NFData       LifecycleState
instance ToByteString LifecycleState
instance ToQuery      LifecycleState
instance ToHeader     LifecycleState

instance FromXML LifecycleState where
    parseXML = parseXMLText "LifecycleState"

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

instance FromXML MetricStatistic where
    parseXML = parseXMLText "MetricStatistic"

data MetricType
  = ALBRequestCountPerTarget
  | ASGAverageCPUUtilization
  | ASGAverageNetworkIn
  | ASGAverageNetworkOut
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetricType where
    parser = takeLowerText >>= \case
        "albrequestcountpertarget" -> pure ALBRequestCountPerTarget
        "asgaveragecpuutilization" -> pure ASGAverageCPUUtilization
        "asgaveragenetworkin" -> pure ASGAverageNetworkIn
        "asgaveragenetworkout" -> pure ASGAverageNetworkOut
        e -> fromTextError $ "Failure parsing MetricType from value: '" <> e
           <> "'. Accepted values: albrequestcountpertarget, asgaveragecpuutilization, asgaveragenetworkin, asgaveragenetworkout"

instance ToText MetricType where
    toText = \case
        ALBRequestCountPerTarget -> "ALBRequestCountPerTarget"
        ASGAverageCPUUtilization -> "ASGAverageCPUUtilization"
        ASGAverageNetworkIn -> "ASGAverageNetworkIn"
        ASGAverageNetworkOut -> "ASGAverageNetworkOut"

instance Hashable     MetricType
instance NFData       MetricType
instance ToByteString MetricType
instance ToQuery      MetricType
instance ToHeader     MetricType

instance FromXML MetricType where
    parseXML = parseXMLText "MetricType"

data ScalingActivityStatusCode
  = Cancelled
  | Failed
  | InProgress
  | MidLifecycleAction
  | PendingSpotBidPlacement
  | PreInService
  | Successful
  | WaitingForELBConnectionDraining
  | WaitingForInstanceId
  | WaitingForInstanceWarmup
  | WaitingForSpotInstanceId
  | WaitingForSpotInstanceRequestId
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalingActivityStatusCode where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "midlifecycleaction" -> pure MidLifecycleAction
        "pendingspotbidplacement" -> pure PendingSpotBidPlacement
        "preinservice" -> pure PreInService
        "successful" -> pure Successful
        "waitingforelbconnectiondraining" -> pure WaitingForELBConnectionDraining
        "waitingforinstanceid" -> pure WaitingForInstanceId
        "waitingforinstancewarmup" -> pure WaitingForInstanceWarmup
        "waitingforspotinstanceid" -> pure WaitingForSpotInstanceId
        "waitingforspotinstancerequestid" -> pure WaitingForSpotInstanceRequestId
        e -> fromTextError $ "Failure parsing ScalingActivityStatusCode from value: '" <> e
           <> "'. Accepted values: cancelled, failed, inprogress, midlifecycleaction, pendingspotbidplacement, preinservice, successful, waitingforelbconnectiondraining, waitingforinstanceid, waitingforinstancewarmup, waitingforspotinstanceid, waitingforspotinstancerequestid"

instance ToText ScalingActivityStatusCode where
    toText = \case
        Cancelled -> "Cancelled"
        Failed -> "Failed"
        InProgress -> "InProgress"
        MidLifecycleAction -> "MidLifecycleAction"
        PendingSpotBidPlacement -> "PendingSpotBidPlacement"
        PreInService -> "PreInService"
        Successful -> "Successful"
        WaitingForELBConnectionDraining -> "WaitingForELBConnectionDraining"
        WaitingForInstanceId -> "WaitingForInstanceId"
        WaitingForInstanceWarmup -> "WaitingForInstanceWarmup"
        WaitingForSpotInstanceId -> "WaitingForSpotInstanceId"
        WaitingForSpotInstanceRequestId -> "WaitingForSpotInstanceRequestId"

instance Hashable     ScalingActivityStatusCode
instance NFData       ScalingActivityStatusCode
instance ToByteString ScalingActivityStatusCode
instance ToQuery      ScalingActivityStatusCode
instance ToHeader     ScalingActivityStatusCode

instance FromXML ScalingActivityStatusCode where
    parseXML = parseXMLText "ScalingActivityStatusCode"
