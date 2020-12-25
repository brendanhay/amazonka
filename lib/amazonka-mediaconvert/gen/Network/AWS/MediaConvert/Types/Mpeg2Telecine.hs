{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Telecine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2Telecine
  ( Mpeg2Telecine
      ( Mpeg2Telecine',
        Mpeg2TelecineNone,
        Mpeg2TelecineSoft,
        Mpeg2TelecineHard,
        fromMpeg2Telecine
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
newtype Mpeg2Telecine = Mpeg2Telecine'
  { fromMpeg2Telecine ::
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

pattern Mpeg2TelecineNone :: Mpeg2Telecine
pattern Mpeg2TelecineNone = Mpeg2Telecine' "NONE"

pattern Mpeg2TelecineSoft :: Mpeg2Telecine
pattern Mpeg2TelecineSoft = Mpeg2Telecine' "SOFT"

pattern Mpeg2TelecineHard :: Mpeg2Telecine
pattern Mpeg2TelecineHard = Mpeg2Telecine' "HARD"

{-# COMPLETE
  Mpeg2TelecineNone,
  Mpeg2TelecineSoft,
  Mpeg2TelecineHard,
  Mpeg2Telecine'
  #-}
