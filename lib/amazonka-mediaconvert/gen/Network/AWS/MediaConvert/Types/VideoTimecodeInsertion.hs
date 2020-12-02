{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoTimecodeInsertion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoTimecodeInsertion where

import Network.AWS.Prelude

-- | Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable Timecode insertion when the input frame rate is identical to the output frame rate. To include timecodes in this output, set Timecode insertion (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to DISABLED. Default is DISABLED. When the service inserts timecodes in an output, by default, it uses any embedded timecodes from the input. If none are present, the service will set the timecode for the first output frame to zero. To change this default behavior, adjust the settings under Timecode configuration (TimecodeConfig). In the console, these settings are located under Job > Job settings > Timecode configuration. Note - Timecode source under input settings (InputTimecodeSource) does not affect the timecodes that are inserted in the output. Source under Job settings > Timecode configuration (TimecodeSource) does.
data VideoTimecodeInsertion
  = VTIDisabled
  | VTIPicTimingSei
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

instance FromText VideoTimecodeInsertion where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure VTIDisabled
      "pic_timing_sei" -> pure VTIPicTimingSei
      e ->
        fromTextError $
          "Failure parsing VideoTimecodeInsertion from value: '" <> e
            <> "'. Accepted values: disabled, pic_timing_sei"

instance ToText VideoTimecodeInsertion where
  toText = \case
    VTIDisabled -> "DISABLED"
    VTIPicTimingSei -> "PIC_TIMING_SEI"

instance Hashable VideoTimecodeInsertion

instance NFData VideoTimecodeInsertion

instance ToByteString VideoTimecodeInsertion

instance ToQuery VideoTimecodeInsertion

instance ToHeader VideoTimecodeInsertion

instance ToJSON VideoTimecodeInsertion where
  toJSON = toJSONText

instance FromJSON VideoTimecodeInsertion where
  parseJSON = parseJSONText "VideoTimecodeInsertion"
