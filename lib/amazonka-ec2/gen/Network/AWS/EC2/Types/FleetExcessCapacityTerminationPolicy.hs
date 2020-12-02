{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetExcessCapacityTerminationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetExcessCapacityTerminationPolicy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetExcessCapacityTerminationPolicy
  = NoTermination
  | Termination
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

instance FromText FleetExcessCapacityTerminationPolicy where
  parser =
    takeLowerText >>= \case
      "no-termination" -> pure NoTermination
      "termination" -> pure Termination
      e ->
        fromTextError $
          "Failure parsing FleetExcessCapacityTerminationPolicy from value: '" <> e
            <> "'. Accepted values: no-termination, termination"

instance ToText FleetExcessCapacityTerminationPolicy where
  toText = \case
    NoTermination -> "no-termination"
    Termination -> "termination"

instance Hashable FleetExcessCapacityTerminationPolicy

instance NFData FleetExcessCapacityTerminationPolicy

instance ToByteString FleetExcessCapacityTerminationPolicy

instance ToQuery FleetExcessCapacityTerminationPolicy

instance ToHeader FleetExcessCapacityTerminationPolicy

instance FromXML FleetExcessCapacityTerminationPolicy where
  parseXML = parseXMLText "FleetExcessCapacityTerminationPolicy"
