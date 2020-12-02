{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimecodeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeSource where

import Network.AWS.Prelude

-- | Use Source (TimecodeSource) to set how timecodes are handled within this job. To make sure that your video, audio, captions, and markers are synchronized and that time-based features, such as image inserter, work correctly, choose the Timecode source option that matches your assets. All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). * Embedded (EMBEDDED) - Use the timecode that is in the input video. If no embedded timecode is in the source, the service will use Start at 0 (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set the timecode of the initial frame to a value other than zero. You use Start timecode (Start) to provide this value.
data TimecodeSource
  = TSEmbedded
  | TSSpecifiedstart
  | TSZerobased
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

instance FromText TimecodeSource where
  parser =
    takeLowerText >>= \case
      "embedded" -> pure TSEmbedded
      "specifiedstart" -> pure TSSpecifiedstart
      "zerobased" -> pure TSZerobased
      e ->
        fromTextError $
          "Failure parsing TimecodeSource from value: '" <> e
            <> "'. Accepted values: embedded, specifiedstart, zerobased"

instance ToText TimecodeSource where
  toText = \case
    TSEmbedded -> "EMBEDDED"
    TSSpecifiedstart -> "SPECIFIEDSTART"
    TSZerobased -> "ZEROBASED"

instance Hashable TimecodeSource

instance NFData TimecodeSource

instance ToByteString TimecodeSource

instance ToQuery TimecodeSource

instance ToHeader TimecodeSource

instance ToJSON TimecodeSource where
  toJSON = toJSONText

instance FromJSON TimecodeSource where
  parseJSON = parseJSONText "TimecodeSource"
