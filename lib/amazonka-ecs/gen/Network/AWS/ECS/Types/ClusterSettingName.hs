{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ClusterSettingName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ClusterSettingName where

import Network.AWS.Prelude

data ClusterSettingName = ContainerInsights
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

instance FromText ClusterSettingName where
  parser =
    takeLowerText >>= \case
      "containerinsights" -> pure ContainerInsights
      e ->
        fromTextError $
          "Failure parsing ClusterSettingName from value: '" <> e
            <> "'. Accepted values: containerinsights"

instance ToText ClusterSettingName where
  toText = \case
    ContainerInsights -> "containerInsights"

instance Hashable ClusterSettingName

instance NFData ClusterSettingName

instance ToByteString ClusterSettingName

instance ToQuery ClusterSettingName

instance ToHeader ClusterSettingName

instance ToJSON ClusterSettingName where
  toJSON = toJSONText

instance FromJSON ClusterSettingName where
  parseJSON = parseJSONText "ClusterSettingName"
