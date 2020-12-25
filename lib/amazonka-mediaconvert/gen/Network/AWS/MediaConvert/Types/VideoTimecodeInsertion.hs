{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoTimecodeInsertion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoTimecodeInsertion
  ( VideoTimecodeInsertion
      ( VideoTimecodeInsertion',
        VideoTimecodeInsertionDisabled,
        VideoTimecodeInsertionPicTimingSei,
        fromVideoTimecodeInsertion
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable Timecode insertion when the input frame rate is identical to the output frame rate. To include timecodes in this output, set Timecode insertion (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to DISABLED. Default is DISABLED. When the service inserts timecodes in an output, by default, it uses any embedded timecodes from the input. If none are present, the service will set the timecode for the first output frame to zero. To change this default behavior, adjust the settings under Timecode configuration (TimecodeConfig). In the console, these settings are located under Job > Job settings > Timecode configuration. Note - Timecode source under input settings (InputTimecodeSource) does not affect the timecodes that are inserted in the output. Source under Job settings > Timecode configuration (TimecodeSource) does.
newtype VideoTimecodeInsertion = VideoTimecodeInsertion'
  { fromVideoTimecodeInsertion ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern VideoTimecodeInsertionDisabled :: VideoTimecodeInsertion
pattern VideoTimecodeInsertionDisabled = VideoTimecodeInsertion' "DISABLED"

pattern VideoTimecodeInsertionPicTimingSei :: VideoTimecodeInsertion
pattern VideoTimecodeInsertionPicTimingSei = VideoTimecodeInsertion' "PIC_TIMING_SEI"

{-# COMPLETE
  VideoTimecodeInsertionDisabled,
  VideoTimecodeInsertionPicTimingSei,
  VideoTimecodeInsertion'
  #-}
