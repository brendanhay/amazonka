{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.DeploymentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.DeploymentType where

import Network.AWS.Prelude

-- | The type of deployment. When used for ''CreateDeployment'', only ''NewDeployment'' and ''Redeployment'' are valid.
data DeploymentType
  = ForceResetDeployment
  | NewDeployment
  | Redeployment
  | ResetDeployment
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

instance FromText DeploymentType where
  parser =
    takeLowerText >>= \case
      "forceresetdeployment" -> pure ForceResetDeployment
      "newdeployment" -> pure NewDeployment
      "redeployment" -> pure Redeployment
      "resetdeployment" -> pure ResetDeployment
      e ->
        fromTextError $
          "Failure parsing DeploymentType from value: '" <> e
            <> "'. Accepted values: forceresetdeployment, newdeployment, redeployment, resetdeployment"

instance ToText DeploymentType where
  toText = \case
    ForceResetDeployment -> "ForceResetDeployment"
    NewDeployment -> "NewDeployment"
    Redeployment -> "Redeployment"
    ResetDeployment -> "ResetDeployment"

instance Hashable DeploymentType

instance NFData DeploymentType

instance ToByteString DeploymentType

instance ToQuery DeploymentType

instance ToHeader DeploymentType

instance ToJSON DeploymentType where
  toJSON = toJSONText

instance FromJSON DeploymentType where
  parseJSON = parseJSONText "DeploymentType"
