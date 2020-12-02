{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobState where

import Network.AWS.Prelude

data JobState
  = JSCancelled
  | JSComplete
  | JSInProgress
  | JSInTransitToAWS
  | JSInTransitToCustomer
  | JSListing
  | JSNew
  | JSPending
  | JSPreparingAppliance
  | JSPreparingShipment
  | JSWithAWS
  | JSWithAWSSortingFacility
  | JSWithCustomer
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

instance FromText JobState where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure JSCancelled
      "complete" -> pure JSComplete
      "inprogress" -> pure JSInProgress
      "intransittoaws" -> pure JSInTransitToAWS
      "intransittocustomer" -> pure JSInTransitToCustomer
      "listing" -> pure JSListing
      "new" -> pure JSNew
      "pending" -> pure JSPending
      "preparingappliance" -> pure JSPreparingAppliance
      "preparingshipment" -> pure JSPreparingShipment
      "withaws" -> pure JSWithAWS
      "withawssortingfacility" -> pure JSWithAWSSortingFacility
      "withcustomer" -> pure JSWithCustomer
      e ->
        fromTextError $
          "Failure parsing JobState from value: '" <> e
            <> "'. Accepted values: cancelled, complete, inprogress, intransittoaws, intransittocustomer, listing, new, pending, preparingappliance, preparingshipment, withaws, withawssortingfacility, withcustomer"

instance ToText JobState where
  toText = \case
    JSCancelled -> "Cancelled"
    JSComplete -> "Complete"
    JSInProgress -> "InProgress"
    JSInTransitToAWS -> "InTransitToAWS"
    JSInTransitToCustomer -> "InTransitToCustomer"
    JSListing -> "Listing"
    JSNew -> "New"
    JSPending -> "Pending"
    JSPreparingAppliance -> "PreparingAppliance"
    JSPreparingShipment -> "PreparingShipment"
    JSWithAWS -> "WithAWS"
    JSWithAWSSortingFacility -> "WithAWSSortingFacility"
    JSWithCustomer -> "WithCustomer"

instance Hashable JobState

instance NFData JobState

instance ToByteString JobState

instance ToQuery JobState

instance ToHeader JobState

instance ToJSON JobState where
  toJSON = toJSONText

instance FromJSON JobState where
  parseJSON = parseJSONText "JobState"
