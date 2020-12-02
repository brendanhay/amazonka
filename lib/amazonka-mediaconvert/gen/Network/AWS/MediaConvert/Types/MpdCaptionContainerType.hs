{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdCaptionContainerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdCaptionContainerType where

import Network.AWS.Prelude

-- | Use this setting only in DASH output groups that include sidecar TTML or IMSC captions.  You specify sidecar captions in a separate output from your audio and video. Choose Raw (RAW) for captions in a single XML file in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for captions in XML format contained within fragmented MP4 files. This set of fragmented MP4 files is separate from your video and audio fragmented MP4 files.
data MpdCaptionContainerType
  = FragmentedMP4
  | Raw
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

instance FromText MpdCaptionContainerType where
  parser =
    takeLowerText >>= \case
      "fragmented_mp4" -> pure FragmentedMP4
      "raw" -> pure Raw
      e ->
        fromTextError $
          "Failure parsing MpdCaptionContainerType from value: '" <> e
            <> "'. Accepted values: fragmented_mp4, raw"

instance ToText MpdCaptionContainerType where
  toText = \case
    FragmentedMP4 -> "FRAGMENTED_MP4"
    Raw -> "RAW"

instance Hashable MpdCaptionContainerType

instance NFData MpdCaptionContainerType

instance ToByteString MpdCaptionContainerType

instance ToQuery MpdCaptionContainerType

instance ToHeader MpdCaptionContainerType

instance ToJSON MpdCaptionContainerType where
  toJSON = toJSONText

instance FromJSON MpdCaptionContainerType where
  parseJSON = parseJSONText "MpdCaptionContainerType"
