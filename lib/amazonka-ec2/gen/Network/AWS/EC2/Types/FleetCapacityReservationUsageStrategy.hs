{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetCapacityReservationUsageStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetCapacityReservationUsageStrategy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetCapacityReservationUsageStrategy = UseCapacityReservationsFirst
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

instance FromText FleetCapacityReservationUsageStrategy where
  parser =
    takeLowerText >>= \case
      "use-capacity-reservations-first" -> pure UseCapacityReservationsFirst
      e ->
        fromTextError $
          "Failure parsing FleetCapacityReservationUsageStrategy from value: '" <> e
            <> "'. Accepted values: use-capacity-reservations-first"

instance ToText FleetCapacityReservationUsageStrategy where
  toText = \case
    UseCapacityReservationsFirst -> "use-capacity-reservations-first"

instance Hashable FleetCapacityReservationUsageStrategy

instance NFData FleetCapacityReservationUsageStrategy

instance ToByteString FleetCapacityReservationUsageStrategy

instance ToQuery FleetCapacityReservationUsageStrategy

instance ToHeader FleetCapacityReservationUsageStrategy

instance FromXML FleetCapacityReservationUsageStrategy where
  parseXML = parseXMLText "FleetCapacityReservationUsageStrategy"
