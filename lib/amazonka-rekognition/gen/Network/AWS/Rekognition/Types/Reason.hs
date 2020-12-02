{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Reason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Reason where

import Network.AWS.Prelude

data Reason
  = ExceedsMaxFaces
  | ExtremePose
  | LowBrightness
  | LowConfidence
  | LowFaceQuality
  | LowSharpness
  | SmallBoundingBox
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

instance FromText Reason where
  parser =
    takeLowerText >>= \case
      "exceeds_max_faces" -> pure ExceedsMaxFaces
      "extreme_pose" -> pure ExtremePose
      "low_brightness" -> pure LowBrightness
      "low_confidence" -> pure LowConfidence
      "low_face_quality" -> pure LowFaceQuality
      "low_sharpness" -> pure LowSharpness
      "small_bounding_box" -> pure SmallBoundingBox
      e ->
        fromTextError $
          "Failure parsing Reason from value: '" <> e
            <> "'. Accepted values: exceeds_max_faces, extreme_pose, low_brightness, low_confidence, low_face_quality, low_sharpness, small_bounding_box"

instance ToText Reason where
  toText = \case
    ExceedsMaxFaces -> "EXCEEDS_MAX_FACES"
    ExtremePose -> "EXTREME_POSE"
    LowBrightness -> "LOW_BRIGHTNESS"
    LowConfidence -> "LOW_CONFIDENCE"
    LowFaceQuality -> "LOW_FACE_QUALITY"
    LowSharpness -> "LOW_SHARPNESS"
    SmallBoundingBox -> "SMALL_BOUNDING_BOX"

instance Hashable Reason

instance NFData Reason

instance ToByteString Reason

instance ToQuery Reason

instance ToHeader Reason

instance FromJSON Reason where
  parseJSON = parseJSONText "Reason"
