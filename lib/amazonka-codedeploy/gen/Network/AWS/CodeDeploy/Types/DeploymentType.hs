{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentType where

import Network.AWS.Prelude

data DeploymentType
  = BlueGreen
  | InPlace
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
      "blue_green" -> pure BlueGreen
      "in_place" -> pure InPlace
      e ->
        fromTextError $
          "Failure parsing DeploymentType from value: '" <> e
            <> "'. Accepted values: blue_green, in_place"

instance ToText DeploymentType where
  toText = \case
    BlueGreen -> "BLUE_GREEN"
    InPlace -> "IN_PLACE"

instance Hashable DeploymentType

instance NFData DeploymentType

instance ToByteString DeploymentType

instance ToQuery DeploymentType

instance ToHeader DeploymentType

instance ToJSON DeploymentType where
  toJSON = toJSONText

instance FromJSON DeploymentType where
  parseJSON = parseJSONText "DeploymentType"
