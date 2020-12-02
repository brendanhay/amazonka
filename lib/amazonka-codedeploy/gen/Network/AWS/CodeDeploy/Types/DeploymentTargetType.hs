{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentTargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentTargetType where

import Network.AWS.Prelude

data DeploymentTargetType
  = CloudFormationTarget
  | ECSTarget
  | InstanceTarget
  | LambdaTarget
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

instance FromText DeploymentTargetType where
  parser =
    takeLowerText >>= \case
      "cloudformationtarget" -> pure CloudFormationTarget
      "ecstarget" -> pure ECSTarget
      "instancetarget" -> pure InstanceTarget
      "lambdatarget" -> pure LambdaTarget
      e ->
        fromTextError $
          "Failure parsing DeploymentTargetType from value: '" <> e
            <> "'. Accepted values: cloudformationtarget, ecstarget, instancetarget, lambdatarget"

instance ToText DeploymentTargetType where
  toText = \case
    CloudFormationTarget -> "CloudFormationTarget"
    ECSTarget -> "ECSTarget"
    InstanceTarget -> "InstanceTarget"
    LambdaTarget -> "LambdaTarget"

instance Hashable DeploymentTargetType

instance NFData DeploymentTargetType

instance ToByteString DeploymentTargetType

instance ToQuery DeploymentTargetType

instance ToHeader DeploymentTargetType

instance FromJSON DeploymentTargetType where
  parseJSON = parseJSONText "DeploymentTargetType"
