{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264Telecine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264Telecine
  ( H264Telecine
      ( H264Telecine',
        H264TelecineNone,
        H264TelecineSoft,
        H264TelecineHard,
        fromH264Telecine
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
newtype H264Telecine = H264Telecine' {fromH264Telecine :: Core.Text}
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

pattern H264TelecineNone :: H264Telecine
pattern H264TelecineNone = H264Telecine' "NONE"

pattern H264TelecineSoft :: H264Telecine
pattern H264TelecineSoft = H264Telecine' "SOFT"

pattern H264TelecineHard :: H264Telecine
pattern H264TelecineHard = H264Telecine' "HARD"

{-# COMPLETE
  H264TelecineNone,
  H264TelecineSoft,
  H264TelecineHard,
  H264Telecine'
  #-}
