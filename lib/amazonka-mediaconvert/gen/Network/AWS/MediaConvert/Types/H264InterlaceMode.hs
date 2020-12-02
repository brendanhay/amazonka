{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264InterlaceMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264InterlaceMode where

import Network.AWS.Prelude

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
data H264InterlaceMode
  = HIMBottomField
  | HIMFollowBottomField
  | HIMFollowTopField
  | HIMProgressive
  | HIMTopField
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

instance FromText H264InterlaceMode where
  parser =
    takeLowerText >>= \case
      "bottom_field" -> pure HIMBottomField
      "follow_bottom_field" -> pure HIMFollowBottomField
      "follow_top_field" -> pure HIMFollowTopField
      "progressive" -> pure HIMProgressive
      "top_field" -> pure HIMTopField
      e ->
        fromTextError $
          "Failure parsing H264InterlaceMode from value: '" <> e
            <> "'. Accepted values: bottom_field, follow_bottom_field, follow_top_field, progressive, top_field"

instance ToText H264InterlaceMode where
  toText = \case
    HIMBottomField -> "BOTTOM_FIELD"
    HIMFollowBottomField -> "FOLLOW_BOTTOM_FIELD"
    HIMFollowTopField -> "FOLLOW_TOP_FIELD"
    HIMProgressive -> "PROGRESSIVE"
    HIMTopField -> "TOP_FIELD"

instance Hashable H264InterlaceMode

instance NFData H264InterlaceMode

instance ToByteString H264InterlaceMode

instance ToQuery H264InterlaceMode

instance ToHeader H264InterlaceMode

instance ToJSON H264InterlaceMode where
  toJSON = toJSONText

instance FromJSON H264InterlaceMode where
  parseJSON = parseJSONText "H264InterlaceMode"
