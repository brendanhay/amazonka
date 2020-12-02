{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ComputeLimitsUnitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ComputeLimitsUnitType where

import Network.AWS.Prelude

data ComputeLimitsUnitType
  = InstanceFleetUnits
  | Instances
  | Vcpu
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

instance FromText ComputeLimitsUnitType where
  parser =
    takeLowerText >>= \case
      "instancefleetunits" -> pure InstanceFleetUnits
      "instances" -> pure Instances
      "vcpu" -> pure Vcpu
      e ->
        fromTextError $
          "Failure parsing ComputeLimitsUnitType from value: '" <> e
            <> "'. Accepted values: instancefleetunits, instances, vcpu"

instance ToText ComputeLimitsUnitType where
  toText = \case
    InstanceFleetUnits -> "InstanceFleetUnits"
    Instances -> "Instances"
    Vcpu -> "VCPU"

instance Hashable ComputeLimitsUnitType

instance NFData ComputeLimitsUnitType

instance ToByteString ComputeLimitsUnitType

instance ToQuery ComputeLimitsUnitType

instance ToHeader ComputeLimitsUnitType

instance ToJSON ComputeLimitsUnitType where
  toJSON = toJSONText

instance FromJSON ComputeLimitsUnitType where
  parseJSON = parseJSONText "ComputeLimitsUnitType"
