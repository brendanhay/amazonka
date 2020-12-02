{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeploymentRolloutState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentRolloutState where

import Network.AWS.Prelude

data DeploymentRolloutState
  = Completed
  | Failed
  | InProgress
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

instance FromText DeploymentRolloutState where
  parser =
    takeLowerText >>= \case
      "completed" -> pure Completed
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      e ->
        fromTextError $
          "Failure parsing DeploymentRolloutState from value: '" <> e
            <> "'. Accepted values: completed, failed, in_progress"

instance ToText DeploymentRolloutState where
  toText = \case
    Completed -> "COMPLETED"
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"

instance Hashable DeploymentRolloutState

instance NFData DeploymentRolloutState

instance ToByteString DeploymentRolloutState

instance ToQuery DeploymentRolloutState

instance ToHeader DeploymentRolloutState

instance FromJSON DeploymentRolloutState where
  parseJSON = parseJSONText "DeploymentRolloutState"
