{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeploymentControllerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentControllerType where

import Network.AWS.Prelude

data DeploymentControllerType
  = CodeDeploy
  | Ecs
  | External
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

instance FromText DeploymentControllerType where
  parser =
    takeLowerText >>= \case
      "code_deploy" -> pure CodeDeploy
      "ecs" -> pure Ecs
      "external" -> pure External
      e ->
        fromTextError $
          "Failure parsing DeploymentControllerType from value: '" <> e
            <> "'. Accepted values: code_deploy, ecs, external"

instance ToText DeploymentControllerType where
  toText = \case
    CodeDeploy -> "CODE_DEPLOY"
    Ecs -> "ECS"
    External -> "EXTERNAL"

instance Hashable DeploymentControllerType

instance NFData DeploymentControllerType

instance ToByteString DeploymentControllerType

instance ToQuery DeploymentControllerType

instance ToHeader DeploymentControllerType

instance ToJSON DeploymentControllerType where
  toJSON = toJSONText

instance FromJSON DeploymentControllerType where
  parseJSON = parseJSONText "DeploymentControllerType"
