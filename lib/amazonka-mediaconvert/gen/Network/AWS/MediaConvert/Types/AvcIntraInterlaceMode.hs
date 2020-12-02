{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraInterlaceMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvcIntraInterlaceMode where

import Network.AWS.Prelude

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
data AvcIntraInterlaceMode
  = AIIMBottomField
  | AIIMFollowBottomField
  | AIIMFollowTopField
  | AIIMProgressive
  | AIIMTopField
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

instance FromText AvcIntraInterlaceMode where
  parser =
    takeLowerText >>= \case
      "bottom_field" -> pure AIIMBottomField
      "follow_bottom_field" -> pure AIIMFollowBottomField
      "follow_top_field" -> pure AIIMFollowTopField
      "progressive" -> pure AIIMProgressive
      "top_field" -> pure AIIMTopField
      e ->
        fromTextError $
          "Failure parsing AvcIntraInterlaceMode from value: '" <> e
            <> "'. Accepted values: bottom_field, follow_bottom_field, follow_top_field, progressive, top_field"

instance ToText AvcIntraInterlaceMode where
  toText = \case
    AIIMBottomField -> "BOTTOM_FIELD"
    AIIMFollowBottomField -> "FOLLOW_BOTTOM_FIELD"
    AIIMFollowTopField -> "FOLLOW_TOP_FIELD"
    AIIMProgressive -> "PROGRESSIVE"
    AIIMTopField -> "TOP_FIELD"

instance Hashable AvcIntraInterlaceMode

instance NFData AvcIntraInterlaceMode

instance ToByteString AvcIntraInterlaceMode

instance ToQuery AvcIntraInterlaceMode

instance ToHeader AvcIntraInterlaceMode

instance ToJSON AvcIntraInterlaceMode where
  toJSON = toJSONText

instance FromJSON AvcIntraInterlaceMode where
  parseJSON = parseJSONText "AvcIntraInterlaceMode"
