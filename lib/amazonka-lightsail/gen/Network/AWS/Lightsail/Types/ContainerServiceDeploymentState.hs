{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceDeploymentState where

import Network.AWS.Prelude

data ContainerServiceDeploymentState
  = Activating
  | Active
  | Failed
  | Inactive
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

instance FromText ContainerServiceDeploymentState where
  parser =
    takeLowerText >>= \case
      "activating" -> pure Activating
      "active" -> pure Active
      "failed" -> pure Failed
      "inactive" -> pure Inactive
      e ->
        fromTextError $
          "Failure parsing ContainerServiceDeploymentState from value: '" <> e
            <> "'. Accepted values: activating, active, failed, inactive"

instance ToText ContainerServiceDeploymentState where
  toText = \case
    Activating -> "ACTIVATING"
    Active -> "ACTIVE"
    Failed -> "FAILED"
    Inactive -> "INACTIVE"

instance Hashable ContainerServiceDeploymentState

instance NFData ContainerServiceDeploymentState

instance ToByteString ContainerServiceDeploymentState

instance ToQuery ContainerServiceDeploymentState

instance ToHeader ContainerServiceDeploymentState

instance FromJSON ContainerServiceDeploymentState where
  parseJSON = parseJSONText "ContainerServiceDeploymentState"
