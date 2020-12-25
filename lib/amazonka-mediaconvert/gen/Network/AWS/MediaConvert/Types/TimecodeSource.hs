{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimecodeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeSource
  ( TimecodeSource
      ( TimecodeSource',
        TimecodeSourceEmbedded,
        TimecodeSourceZerobased,
        TimecodeSourceSpecifiedstart,
        fromTimecodeSource
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Use Source (TimecodeSource) to set how timecodes are handled within this job. To make sure that your video, audio, captions, and markers are synchronized and that time-based features, such as image inserter, work correctly, choose the Timecode source option that matches your assets. All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). * Embedded (EMBEDDED) - Use the timecode that is in the input video. If no embedded timecode is in the source, the service will use Start at 0 (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set the timecode of the initial frame to a value other than zero. You use Start timecode (Start) to provide this value.
newtype TimecodeSource = TimecodeSource'
  { fromTimecodeSource ::
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

pattern TimecodeSourceEmbedded :: TimecodeSource
pattern TimecodeSourceEmbedded = TimecodeSource' "EMBEDDED"

pattern TimecodeSourceZerobased :: TimecodeSource
pattern TimecodeSourceZerobased = TimecodeSource' "ZEROBASED"

pattern TimecodeSourceSpecifiedstart :: TimecodeSource
pattern TimecodeSourceSpecifiedstart = TimecodeSource' "SPECIFIEDSTART"

{-# COMPLETE
  TimecodeSourceEmbedded,
  TimecodeSourceZerobased,
  TimecodeSourceSpecifiedstart,
  TimecodeSource'
  #-}
