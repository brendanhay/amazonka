{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265DynamicSubGop
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265DynamicSubGop where

import Network.AWS.Prelude

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
data H265DynamicSubGop
  = Adaptive
  | Static
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

instance FromText H265DynamicSubGop where
  parser =
    takeLowerText >>= \case
      "adaptive" -> pure Adaptive
      "static" -> pure Static
      e ->
        fromTextError $
          "Failure parsing H265DynamicSubGop from value: '" <> e
            <> "'. Accepted values: adaptive, static"

instance ToText H265DynamicSubGop where
  toText = \case
    Adaptive -> "ADAPTIVE"
    Static -> "STATIC"

instance Hashable H265DynamicSubGop

instance NFData H265DynamicSubGop

instance ToByteString H265DynamicSubGop

instance ToQuery H265DynamicSubGop

instance ToHeader H265DynamicSubGop

instance ToJSON H265DynamicSubGop where
  toJSON = toJSONText

instance FromJSON H265DynamicSubGop where
  parseJSON = parseJSONText "H265DynamicSubGop"
