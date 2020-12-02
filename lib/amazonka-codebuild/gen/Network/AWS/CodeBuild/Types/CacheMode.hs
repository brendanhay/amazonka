{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CacheMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CacheMode where

import Network.AWS.Prelude

data CacheMode
  = LocalCustomCache
  | LocalDockerLayerCache
  | LocalSourceCache
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

instance FromText CacheMode where
  parser =
    takeLowerText >>= \case
      "local_custom_cache" -> pure LocalCustomCache
      "local_docker_layer_cache" -> pure LocalDockerLayerCache
      "local_source_cache" -> pure LocalSourceCache
      e ->
        fromTextError $
          "Failure parsing CacheMode from value: '" <> e
            <> "'. Accepted values: local_custom_cache, local_docker_layer_cache, local_source_cache"

instance ToText CacheMode where
  toText = \case
    LocalCustomCache -> "LOCAL_CUSTOM_CACHE"
    LocalDockerLayerCache -> "LOCAL_DOCKER_LAYER_CACHE"
    LocalSourceCache -> "LOCAL_SOURCE_CACHE"

instance Hashable CacheMode

instance NFData CacheMode

instance ToByteString CacheMode

instance ToQuery CacheMode

instance ToHeader CacheMode

instance ToJSON CacheMode where
  toJSON = toJSONText

instance FromJSON CacheMode where
  parseJSON = parseJSONText "CacheMode"
