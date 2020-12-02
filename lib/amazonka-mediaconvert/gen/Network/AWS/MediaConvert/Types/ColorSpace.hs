{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorSpace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorSpace where

import Network.AWS.Prelude

-- | If your input video has accurate color space metadata, or if you don't know about color space, leave this set to the default value Follow (FOLLOW). The service will automatically detect your input color space. If your input video has metadata indicating the wrong color space, specify the accurate color space here. If your input video is HDR 10 and the SMPTE ST 2086 Mastering Display Color Volume static metadata isn't present in your video stream, or if that metadata is present but not accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct values in the input HDR 10 metadata (Hdr10Metadata) settings. For more information about MediaConvert HDR jobs, see https://docs.aws.amazon.com/console/mediaconvert/hdr.
data ColorSpace
  = Follow
  | HDR10
  | Hlg2020
  | Rec601
  | Rec709
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

instance FromText ColorSpace where
  parser =
    takeLowerText >>= \case
      "follow" -> pure Follow
      "hdr10" -> pure HDR10
      "hlg_2020" -> pure Hlg2020
      "rec_601" -> pure Rec601
      "rec_709" -> pure Rec709
      e ->
        fromTextError $
          "Failure parsing ColorSpace from value: '" <> e
            <> "'. Accepted values: follow, hdr10, hlg_2020, rec_601, rec_709"

instance ToText ColorSpace where
  toText = \case
    Follow -> "FOLLOW"
    HDR10 -> "HDR10"
    Hlg2020 -> "HLG_2020"
    Rec601 -> "REC_601"
    Rec709 -> "REC_709"

instance Hashable ColorSpace

instance NFData ColorSpace

instance ToByteString ColorSpace

instance ToQuery ColorSpace

instance ToHeader ColorSpace

instance ToJSON ColorSpace where
  toJSON = toJSONText

instance FromJSON ColorSpace where
  parseJSON = parseJSONText "ColorSpace"
