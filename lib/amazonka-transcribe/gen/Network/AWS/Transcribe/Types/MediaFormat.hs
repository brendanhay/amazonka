{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MediaFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MediaFormat
  ( MediaFormat
      ( MediaFormat',
        MediaFormatMP3,
        MediaFormatMP4,
        MediaFormatWav,
        MediaFormatFlac,
        MediaFormatOgg,
        MediaFormatAmr,
        MediaFormatWebm,
        fromMediaFormat
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MediaFormat = MediaFormat' {fromMediaFormat :: Core.Text}
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

pattern MediaFormatMP3 :: MediaFormat
pattern MediaFormatMP3 = MediaFormat' "mp3"

pattern MediaFormatMP4 :: MediaFormat
pattern MediaFormatMP4 = MediaFormat' "mp4"

pattern MediaFormatWav :: MediaFormat
pattern MediaFormatWav = MediaFormat' "wav"

pattern MediaFormatFlac :: MediaFormat
pattern MediaFormatFlac = MediaFormat' "flac"

pattern MediaFormatOgg :: MediaFormat
pattern MediaFormatOgg = MediaFormat' "ogg"

pattern MediaFormatAmr :: MediaFormat
pattern MediaFormatAmr = MediaFormat' "amr"

pattern MediaFormatWebm :: MediaFormat
pattern MediaFormatWebm = MediaFormat' "webm"

{-# COMPLETE
  MediaFormatMP3,
  MediaFormatMP4,
  MediaFormatWav,
  MediaFormatFlac,
  MediaFormatOgg,
  MediaFormatAmr,
  MediaFormatWebm,
  MediaFormat'
  #-}
