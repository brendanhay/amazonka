{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PreferredChannelPipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PreferredChannelPipeline where

import Network.AWS.Prelude

-- | Indicates which pipeline is preferred by the multiplex for program ingest.
--
-- If set to \"PIPELINE_0\" or \"PIPELINE_1\" and an unhealthy ingest causes the multiplex to switch to the non-preferred pipeline,
-- it will switch back once that ingest is healthy again. If set to \"CURRENTLY_ACTIVE\",
-- it will not switch back to the other pipeline based on it recovering to a healthy state,
-- it will only switch if the active pipeline becomes unhealthy.
data PreferredChannelPipeline
  = CurrentlyActive
  | Pipeline0
  | Pipeline1
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

instance FromText PreferredChannelPipeline where
  parser =
    takeLowerText >>= \case
      "currently_active" -> pure CurrentlyActive
      "pipeline_0" -> pure Pipeline0
      "pipeline_1" -> pure Pipeline1
      e ->
        fromTextError $
          "Failure parsing PreferredChannelPipeline from value: '" <> e
            <> "'. Accepted values: currently_active, pipeline_0, pipeline_1"

instance ToText PreferredChannelPipeline where
  toText = \case
    CurrentlyActive -> "CURRENTLY_ACTIVE"
    Pipeline0 -> "PIPELINE_0"
    Pipeline1 -> "PIPELINE_1"

instance Hashable PreferredChannelPipeline

instance NFData PreferredChannelPipeline

instance ToByteString PreferredChannelPipeline

instance ToQuery PreferredChannelPipeline

instance ToHeader PreferredChannelPipeline

instance ToJSON PreferredChannelPipeline where
  toJSON = toJSONText

instance FromJSON PreferredChannelPipeline where
  parseJSON = parseJSONText "PreferredChannelPipeline"
