{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264DynamicSubGop
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264DynamicSubGop where

import Network.AWS.Prelude

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
data H264DynamicSubGop
  = HDSGAdaptive
  | HDSGStatic
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

instance FromText H264DynamicSubGop where
  parser =
    takeLowerText >>= \case
      "adaptive" -> pure HDSGAdaptive
      "static" -> pure HDSGStatic
      e ->
        fromTextError $
          "Failure parsing H264DynamicSubGop from value: '" <> e
            <> "'. Accepted values: adaptive, static"

instance ToText H264DynamicSubGop where
  toText = \case
    HDSGAdaptive -> "ADAPTIVE"
    HDSGStatic -> "STATIC"

instance Hashable H264DynamicSubGop

instance NFData H264DynamicSubGop

instance ToByteString H264DynamicSubGop

instance ToQuery H264DynamicSubGop

instance ToHeader H264DynamicSubGop

instance ToJSON H264DynamicSubGop where
  toJSON = toJSONText

instance FromJSON H264DynamicSubGop where
  parseJSON = parseJSONText "H264DynamicSubGop"
