{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.Sum where

import           Network.AWS.Prelude

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: Detached, Detaching, EnteringStandby, InService, Pending, Pending:Proceed, Pending:Wait, Quarantined, Standby, Terminated, Terminating, Terminating:Proceed, Terminating:Wait"

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
instance ToByteString LifecycleState
instance ToQuery      LifecycleState
instance ToHeader     LifecycleState

instance FromXML LifecycleState where
    parseXML = parseXMLText "LifecycleState"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: Cancelled, Failed, InProgress, MidLifecycleAction, PendingSpotBidPlacement, PreInService, Successful, WaitingForELBConnectionDraining, WaitingForInstanceId, WaitingForInstanceWarmup, WaitingForSpotInstanceId, WaitingForSpotInstanceRequestId"

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
instance ToByteString ScalingActivityStatusCode
instance ToQuery      ScalingActivityStatusCode
instance ToHeader     ScalingActivityStatusCode

instance FromXML ScalingActivityStatusCode where
    parseXML = parseXMLText "ScalingActivityStatusCode"
