{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.SegmentTemplateFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.SegmentTemplateFormat where

import Network.AWS.Prelude

data SegmentTemplateFormat
  = NumberWithDuration
  | NumberWithTimeline
  | TimeWithTimeline
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

instance FromText SegmentTemplateFormat where
  parser =
    takeLowerText >>= \case
      "number_with_duration" -> pure NumberWithDuration
      "number_with_timeline" -> pure NumberWithTimeline
      "time_with_timeline" -> pure TimeWithTimeline
      e ->
        fromTextError $
          "Failure parsing SegmentTemplateFormat from value: '" <> e
            <> "'. Accepted values: number_with_duration, number_with_timeline, time_with_timeline"

instance ToText SegmentTemplateFormat where
  toText = \case
    NumberWithDuration -> "NUMBER_WITH_DURATION"
    NumberWithTimeline -> "NUMBER_WITH_TIMELINE"
    TimeWithTimeline -> "TIME_WITH_TIMELINE"

instance Hashable SegmentTemplateFormat

instance NFData SegmentTemplateFormat

instance ToByteString SegmentTemplateFormat

instance ToQuery SegmentTemplateFormat

instance ToHeader SegmentTemplateFormat

instance ToJSON SegmentTemplateFormat where
  toJSON = toJSONText

instance FromJSON SegmentTemplateFormat where
  parseJSON = parseJSONText "SegmentTemplateFormat"
