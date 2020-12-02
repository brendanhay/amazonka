{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsSegmentationMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsSegmentationMode where

import Network.AWS.Prelude

-- | Hls Segmentation Mode
data HlsSegmentationMode
  = HSMUseInputSegmentation
  | HSMUseSegmentDuration
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

instance FromText HlsSegmentationMode where
  parser =
    takeLowerText >>= \case
      "use_input_segmentation" -> pure HSMUseInputSegmentation
      "use_segment_duration" -> pure HSMUseSegmentDuration
      e ->
        fromTextError $
          "Failure parsing HlsSegmentationMode from value: '" <> e
            <> "'. Accepted values: use_input_segmentation, use_segment_duration"

instance ToText HlsSegmentationMode where
  toText = \case
    HSMUseInputSegmentation -> "USE_INPUT_SEGMENTATION"
    HSMUseSegmentDuration -> "USE_SEGMENT_DURATION"

instance Hashable HlsSegmentationMode

instance NFData HlsSegmentationMode

instance ToByteString HlsSegmentationMode

instance ToQuery HlsSegmentationMode

instance ToHeader HlsSegmentationMode

instance ToJSON HlsSegmentationMode where
  toJSON = toJSONText

instance FromJSON HlsSegmentationMode where
  parseJSON = parseJSONText "HlsSegmentationMode"
