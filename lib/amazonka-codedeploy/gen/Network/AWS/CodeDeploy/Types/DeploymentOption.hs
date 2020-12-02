{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentOption where

import Network.AWS.Prelude

data DeploymentOption
  = WithTrafficControl
  | WithoutTrafficControl
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

instance FromText DeploymentOption where
  parser =
    takeLowerText >>= \case
      "with_traffic_control" -> pure WithTrafficControl
      "without_traffic_control" -> pure WithoutTrafficControl
      e ->
        fromTextError $
          "Failure parsing DeploymentOption from value: '" <> e
            <> "'. Accepted values: with_traffic_control, without_traffic_control"

instance ToText DeploymentOption where
  toText = \case
    WithTrafficControl -> "WITH_TRAFFIC_CONTROL"
    WithoutTrafficControl -> "WITHOUT_TRAFFIC_CONTROL"

instance Hashable DeploymentOption

instance NFData DeploymentOption

instance ToByteString DeploymentOption

instance ToQuery DeploymentOption

instance ToHeader DeploymentOption

instance ToJSON DeploymentOption where
  toJSON = toJSONText

instance FromJSON DeploymentOption where
  parseJSON = parseJSONText "DeploymentOption"
