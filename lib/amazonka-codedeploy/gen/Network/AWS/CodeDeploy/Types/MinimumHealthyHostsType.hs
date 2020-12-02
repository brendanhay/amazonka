{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.MinimumHealthyHostsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.MinimumHealthyHostsType where

import Network.AWS.Prelude

data MinimumHealthyHostsType
  = FleetPercent
  | HostCount
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

instance FromText MinimumHealthyHostsType where
  parser =
    takeLowerText >>= \case
      "fleet_percent" -> pure FleetPercent
      "host_count" -> pure HostCount
      e ->
        fromTextError $
          "Failure parsing MinimumHealthyHostsType from value: '" <> e
            <> "'. Accepted values: fleet_percent, host_count"

instance ToText MinimumHealthyHostsType where
  toText = \case
    FleetPercent -> "FLEET_PERCENT"
    HostCount -> "HOST_COUNT"

instance Hashable MinimumHealthyHostsType

instance NFData MinimumHealthyHostsType

instance ToByteString MinimumHealthyHostsType

instance ToQuery MinimumHealthyHostsType

instance ToHeader MinimumHealthyHostsType

instance ToJSON MinimumHealthyHostsType where
  toJSON = toJSONText

instance FromJSON MinimumHealthyHostsType where
  parseJSON = parseJSONText "MinimumHealthyHostsType"
