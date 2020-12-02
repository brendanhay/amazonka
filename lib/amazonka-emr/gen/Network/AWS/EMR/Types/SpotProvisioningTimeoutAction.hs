{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SpotProvisioningTimeoutAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SpotProvisioningTimeoutAction where

import Network.AWS.Prelude

data SpotProvisioningTimeoutAction
  = SPTASwitchToOnDemand
  | SPTATerminateCluster
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

instance FromText SpotProvisioningTimeoutAction where
  parser =
    takeLowerText >>= \case
      "switch_to_on_demand" -> pure SPTASwitchToOnDemand
      "terminate_cluster" -> pure SPTATerminateCluster
      e ->
        fromTextError $
          "Failure parsing SpotProvisioningTimeoutAction from value: '" <> e
            <> "'. Accepted values: switch_to_on_demand, terminate_cluster"

instance ToText SpotProvisioningTimeoutAction where
  toText = \case
    SPTASwitchToOnDemand -> "SWITCH_TO_ON_DEMAND"
    SPTATerminateCluster -> "TERMINATE_CLUSTER"

instance Hashable SpotProvisioningTimeoutAction

instance NFData SpotProvisioningTimeoutAction

instance ToByteString SpotProvisioningTimeoutAction

instance ToQuery SpotProvisioningTimeoutAction

instance ToHeader SpotProvisioningTimeoutAction

instance ToJSON SpotProvisioningTimeoutAction where
  toJSON = toJSONText

instance FromJSON SpotProvisioningTimeoutAction where
  parseJSON = parseJSONText "SpotProvisioningTimeoutAction"
