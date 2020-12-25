{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SccDestinationFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SccDestinationFramerate
  ( SccDestinationFramerate
      ( SccDestinationFramerate',
        SccDestinationFramerateFramerate2397,
        SccDestinationFramerateFramerate24,
        SccDestinationFramerateFramerate25,
        SccDestinationFramerateFramerate2997Dropframe,
        SccDestinationFramerateFramerate2997NonDropframe,
        fromSccDestinationFramerate
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a frame rate that matches the frame rate of the associated video. If the video frame rate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
newtype SccDestinationFramerate = SccDestinationFramerate'
  { fromSccDestinationFramerate ::
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

pattern SccDestinationFramerateFramerate2397 :: SccDestinationFramerate
pattern SccDestinationFramerateFramerate2397 = SccDestinationFramerate' "FRAMERATE_23_97"

pattern SccDestinationFramerateFramerate24 :: SccDestinationFramerate
pattern SccDestinationFramerateFramerate24 = SccDestinationFramerate' "FRAMERATE_24"

pattern SccDestinationFramerateFramerate25 :: SccDestinationFramerate
pattern SccDestinationFramerateFramerate25 = SccDestinationFramerate' "FRAMERATE_25"

pattern SccDestinationFramerateFramerate2997Dropframe :: SccDestinationFramerate
pattern SccDestinationFramerateFramerate2997Dropframe = SccDestinationFramerate' "FRAMERATE_29_97_DROPFRAME"

pattern SccDestinationFramerateFramerate2997NonDropframe :: SccDestinationFramerate
pattern SccDestinationFramerateFramerate2997NonDropframe = SccDestinationFramerate' "FRAMERATE_29_97_NON_DROPFRAME"

{-# COMPLETE
  SccDestinationFramerateFramerate2397,
  SccDestinationFramerateFramerate24,
  SccDestinationFramerateFramerate25,
  SccDestinationFramerateFramerate2997Dropframe,
  SccDestinationFramerateFramerate2997NonDropframe,
  SccDestinationFramerate'
  #-}
