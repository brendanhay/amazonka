{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputTimecodeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.InputTimecodeSource
  ( InputTimecodeSource
    ( InputTimecodeSource'
    , InputTimecodeSourceEmbedded
    , InputTimecodeSourceZerobased
    , InputTimecodeSourceSpecifiedstart
    , fromInputTimecodeSource
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
newtype InputTimecodeSource = InputTimecodeSource'{fromInputTimecodeSource
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern InputTimecodeSourceEmbedded :: InputTimecodeSource
pattern InputTimecodeSourceEmbedded = InputTimecodeSource' "EMBEDDED"

pattern InputTimecodeSourceZerobased :: InputTimecodeSource
pattern InputTimecodeSourceZerobased = InputTimecodeSource' "ZEROBASED"

pattern InputTimecodeSourceSpecifiedstart :: InputTimecodeSource
pattern InputTimecodeSourceSpecifiedstart = InputTimecodeSource' "SPECIFIEDSTART"

{-# COMPLETE 
  InputTimecodeSourceEmbedded,

  InputTimecodeSourceZerobased,

  InputTimecodeSourceSpecifiedstart,
  InputTimecodeSource'
  #-}
