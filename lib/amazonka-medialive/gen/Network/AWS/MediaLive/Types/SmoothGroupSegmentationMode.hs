{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode where

import Network.AWS.Prelude

-- | Smooth Group Segmentation Mode
data SmoothGroupSegmentationMode
  = UseInputSegmentation
  | UseSegmentDuration
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

instance FromText SmoothGroupSegmentationMode where
  parser =
    takeLowerText >>= \case
      "use_input_segmentation" -> pure UseInputSegmentation
      "use_segment_duration" -> pure UseSegmentDuration
      e ->
        fromTextError $
          "Failure parsing SmoothGroupSegmentationMode from value: '" <> e
            <> "'. Accepted values: use_input_segmentation, use_segment_duration"

instance ToText SmoothGroupSegmentationMode where
  toText = \case
    UseInputSegmentation -> "USE_INPUT_SEGMENTATION"
    UseSegmentDuration -> "USE_SEGMENT_DURATION"

instance Hashable SmoothGroupSegmentationMode

instance NFData SmoothGroupSegmentationMode

instance ToByteString SmoothGroupSegmentationMode

instance ToQuery SmoothGroupSegmentationMode

instance ToHeader SmoothGroupSegmentationMode

instance ToJSON SmoothGroupSegmentationMode where
  toJSON = toJSONText

instance FromJSON SmoothGroupSegmentationMode where
  parseJSON = parseJSONText "SmoothGroupSegmentationMode"
