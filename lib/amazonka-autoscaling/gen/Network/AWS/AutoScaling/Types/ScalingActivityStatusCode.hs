{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScalingActivityStatusCode where

import Network.AWS.Prelude

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

instance FromText ScalingActivityStatusCode where
  parser =
    takeLowerText >>= \case
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
      e ->
        fromTextError $
          "Failure parsing ScalingActivityStatusCode from value: '" <> e
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

instance Hashable ScalingActivityStatusCode

instance NFData ScalingActivityStatusCode

instance ToByteString ScalingActivityStatusCode

instance ToQuery ScalingActivityStatusCode

instance ToHeader ScalingActivityStatusCode

instance FromXML ScalingActivityStatusCode where
  parseXML = parseXMLText "ScalingActivityStatusCode"
