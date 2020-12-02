{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoCodec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoCodec where

import Network.AWS.Prelude

-- | Type of video codec
data VideoCodec
  = AV1
  | AvcIntra
  | FrameCapture
  | H264
  | H265
  | MPEG2
  | Prores
  | VC3
  | VP8
  | VP9
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

instance FromText VideoCodec where
  parser =
    takeLowerText >>= \case
      "av1" -> pure AV1
      "avc_intra" -> pure AvcIntra
      "frame_capture" -> pure FrameCapture
      "h_264" -> pure H264
      "h_265" -> pure H265
      "mpeg2" -> pure MPEG2
      "prores" -> pure Prores
      "vc3" -> pure VC3
      "vp8" -> pure VP8
      "vp9" -> pure VP9
      e ->
        fromTextError $
          "Failure parsing VideoCodec from value: '" <> e
            <> "'. Accepted values: av1, avc_intra, frame_capture, h_264, h_265, mpeg2, prores, vc3, vp8, vp9"

instance ToText VideoCodec where
  toText = \case
    AV1 -> "AV1"
    AvcIntra -> "AVC_INTRA"
    FrameCapture -> "FRAME_CAPTURE"
    H264 -> "H_264"
    H265 -> "H_265"
    MPEG2 -> "MPEG2"
    Prores -> "PRORES"
    VC3 -> "VC3"
    VP8 -> "VP8"
    VP9 -> "VP9"

instance Hashable VideoCodec

instance NFData VideoCodec

instance ToByteString VideoCodec

instance ToQuery VideoCodec

instance ToHeader VideoCodec

instance ToJSON VideoCodec where
  toJSON = toJSONText

instance FromJSON VideoCodec where
  parseJSON = parseJSONText "VideoCodec"
