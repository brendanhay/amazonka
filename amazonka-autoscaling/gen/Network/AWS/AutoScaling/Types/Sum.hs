{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.Sum where

import           Network.AWS.Prelude

data LifecycleState
    = PendingWait
    | Terminating
    | TerminatingWait
    | Pending
    | Standby
    | EnteringStandby
    | InService
    | Detached
    | Detaching
    | Quarantined
    | PendingProceed
    | Terminated
    | TerminatingProceed
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        Detached -> "detached"
        Detaching -> "detaching"
        EnteringStandby -> "enteringstandby"
        InService -> "inservice"
        Pending -> "pending"
        PendingProceed -> "pending:proceed"
        PendingWait -> "pending:wait"
        Quarantined -> "quarantined"
        Standby -> "standby"
        Terminated -> "terminated"
        Terminating -> "terminating"
        TerminatingProceed -> "terminating:proceed"
        TerminatingWait -> "terminating:wait"

instance Hashable LifecycleState
instance ToQuery LifecycleState
instance ToHeader LifecycleState

instance FromXML LifecycleState where
    parseXML = parseXMLText "LifecycleState"

data ScalingActivityStatusCode
    = WaitingForSpotInstanceId
    | WaitingForInstanceWarmup
    | WaitingForSpotInstanceRequestId
    | WaitingForInstanceId
    | Successful
    | InProgress
    | PreInService
    | WaitingForELBConnectionDraining
    | MidLifecycleAction
    | Cancelled
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ScalingActivityStatusCode where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "midlifecycleaction" -> pure MidLifecycleAction
        "preinservice" -> pure PreInService
        "successful" -> pure Successful
        "waitingforelbconnectiondraining" -> pure WaitingForELBConnectionDraining
        "waitingforinstanceid" -> pure WaitingForInstanceId
        "waitingforinstancewarmup" -> pure WaitingForInstanceWarmup
        "waitingforspotinstanceid" -> pure WaitingForSpotInstanceId
        "waitingforspotinstancerequestid" -> pure WaitingForSpotInstanceRequestId
        e -> fromTextError $ "Failure parsing ScalingActivityStatusCode from value: '" <> e
           <> "'. Accepted values: cancelled, failed, inprogress, midlifecycleaction, preinservice, successful, waitingforelbconnectiondraining, waitingforinstanceid, waitingforinstancewarmup, waitingforspotinstanceid, waitingforspotinstancerequestid"

instance ToText ScalingActivityStatusCode where
    toText = \case
        Cancelled -> "cancelled"
        Failed -> "failed"
        InProgress -> "inprogress"
        MidLifecycleAction -> "midlifecycleaction"
        PreInService -> "preinservice"
        Successful -> "successful"
        WaitingForELBConnectionDraining -> "waitingforelbconnectiondraining"
        WaitingForInstanceId -> "waitingforinstanceid"
        WaitingForInstanceWarmup -> "waitingforinstancewarmup"
        WaitingForSpotInstanceId -> "waitingforspotinstanceid"
        WaitingForSpotInstanceRequestId -> "waitingforspotinstancerequestid"

instance Hashable ScalingActivityStatusCode
instance ToQuery ScalingActivityStatusCode
instance ToHeader ScalingActivityStatusCode

instance FromXML ScalingActivityStatusCode where
    parseXML = parseXMLText "ScalingActivityStatusCode"
