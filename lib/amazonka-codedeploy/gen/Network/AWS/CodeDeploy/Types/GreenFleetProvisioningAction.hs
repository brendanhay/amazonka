{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.GreenFleetProvisioningAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GreenFleetProvisioningAction where

import Network.AWS.Prelude

data GreenFleetProvisioningAction
  = CopyAutoScalingGroup
  | DiscoverExisting
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

instance FromText GreenFleetProvisioningAction where
  parser =
    takeLowerText >>= \case
      "copy_auto_scaling_group" -> pure CopyAutoScalingGroup
      "discover_existing" -> pure DiscoverExisting
      e ->
        fromTextError $
          "Failure parsing GreenFleetProvisioningAction from value: '" <> e
            <> "'. Accepted values: copy_auto_scaling_group, discover_existing"

instance ToText GreenFleetProvisioningAction where
  toText = \case
    CopyAutoScalingGroup -> "COPY_AUTO_SCALING_GROUP"
    DiscoverExisting -> "DISCOVER_EXISTING"

instance Hashable GreenFleetProvisioningAction

instance NFData GreenFleetProvisioningAction

instance ToByteString GreenFleetProvisioningAction

instance ToQuery GreenFleetProvisioningAction

instance ToHeader GreenFleetProvisioningAction

instance ToJSON GreenFleetProvisioningAction where
  toJSON = toJSONText

instance FromJSON GreenFleetProvisioningAction where
  parseJSON = parseJSONText "GreenFleetProvisioningAction"
