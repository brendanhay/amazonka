{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.DeploymentMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.DeploymentMode where

import Network.AWS.Prelude

-- | The deployment mode of the broker.
data DeploymentMode
  = ActiveStandbyMultiAz
  | ClusterMultiAz
  | SingleInstance
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

instance FromText DeploymentMode where
  parser =
    takeLowerText >>= \case
      "active_standby_multi_az" -> pure ActiveStandbyMultiAz
      "cluster_multi_az" -> pure ClusterMultiAz
      "single_instance" -> pure SingleInstance
      e ->
        fromTextError $
          "Failure parsing DeploymentMode from value: '" <> e
            <> "'. Accepted values: active_standby_multi_az, cluster_multi_az, single_instance"

instance ToText DeploymentMode where
  toText = \case
    ActiveStandbyMultiAz -> "ACTIVE_STANDBY_MULTI_AZ"
    ClusterMultiAz -> "CLUSTER_MULTI_AZ"
    SingleInstance -> "SINGLE_INSTANCE"

instance Hashable DeploymentMode

instance NFData DeploymentMode

instance ToByteString DeploymentMode

instance ToQuery DeploymentMode

instance ToHeader DeploymentMode

instance ToJSON DeploymentMode where
  toJSON = toJSONText

instance FromJSON DeploymentMode where
  parseJSON = parseJSONText "DeploymentMode"
