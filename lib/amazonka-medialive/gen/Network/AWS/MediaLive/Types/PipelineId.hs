{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PipelineId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PipelineId where

import Network.AWS.Prelude

-- | Pipeline ID
data PipelineId
  = PIPipeline0
  | PIPipeline1
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

instance FromText PipelineId where
  parser =
    takeLowerText >>= \case
      "pipeline_0" -> pure PIPipeline0
      "pipeline_1" -> pure PIPipeline1
      e ->
        fromTextError $
          "Failure parsing PipelineId from value: '" <> e
            <> "'. Accepted values: pipeline_0, pipeline_1"

instance ToText PipelineId where
  toText = \case
    PIPipeline0 -> "PIPELINE_0"
    PIPipeline1 -> "PIPELINE_1"

instance Hashable PipelineId

instance NFData PipelineId

instance ToByteString PipelineId

instance ToQuery PipelineId

instance ToHeader PipelineId

instance ToJSON PipelineId where
  toJSON = toJSONText

instance FromJSON PipelineId where
  parseJSON = parseJSONText "PipelineId"
