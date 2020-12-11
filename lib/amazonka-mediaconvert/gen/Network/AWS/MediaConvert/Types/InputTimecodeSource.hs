-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputTimecodeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputTimecodeSource
  ( InputTimecodeSource
      ( InputTimecodeSource',
        ITSEmbedded,
        ITSSpecifiedstart,
        ITSZerobased
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
newtype InputTimecodeSource = InputTimecodeSource' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ITSEmbedded :: InputTimecodeSource
pattern ITSEmbedded = InputTimecodeSource' "EMBEDDED"

pattern ITSSpecifiedstart :: InputTimecodeSource
pattern ITSSpecifiedstart = InputTimecodeSource' "SPECIFIEDSTART"

pattern ITSZerobased :: InputTimecodeSource
pattern ITSZerobased = InputTimecodeSource' "ZEROBASED"

{-# COMPLETE
  ITSEmbedded,
  ITSSpecifiedstart,
  ITSZerobased,
  InputTimecodeSource'
  #-}
