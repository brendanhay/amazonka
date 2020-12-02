{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2InterlaceMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2InterlaceMode where

import Network.AWS.Prelude

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
data Mpeg2InterlaceMode
  = MIMBottomField
  | MIMFollowBottomField
  | MIMFollowTopField
  | MIMProgressive
  | MIMTopField
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

instance FromText Mpeg2InterlaceMode where
  parser =
    takeLowerText >>= \case
      "bottom_field" -> pure MIMBottomField
      "follow_bottom_field" -> pure MIMFollowBottomField
      "follow_top_field" -> pure MIMFollowTopField
      "progressive" -> pure MIMProgressive
      "top_field" -> pure MIMTopField
      e ->
        fromTextError $
          "Failure parsing Mpeg2InterlaceMode from value: '" <> e
            <> "'. Accepted values: bottom_field, follow_bottom_field, follow_top_field, progressive, top_field"

instance ToText Mpeg2InterlaceMode where
  toText = \case
    MIMBottomField -> "BOTTOM_FIELD"
    MIMFollowBottomField -> "FOLLOW_BOTTOM_FIELD"
    MIMFollowTopField -> "FOLLOW_TOP_FIELD"
    MIMProgressive -> "PROGRESSIVE"
    MIMTopField -> "TOP_FIELD"

instance Hashable Mpeg2InterlaceMode

instance NFData Mpeg2InterlaceMode

instance ToByteString Mpeg2InterlaceMode

instance ToQuery Mpeg2InterlaceMode

instance ToHeader Mpeg2InterlaceMode

instance ToJSON Mpeg2InterlaceMode where
  toJSON = toJSONText

instance FromJSON Mpeg2InterlaceMode where
  parseJSON = parseJSONText "Mpeg2InterlaceMode"
