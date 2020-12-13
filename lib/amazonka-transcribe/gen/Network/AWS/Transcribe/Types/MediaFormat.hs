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
        MP3,
        MP4,
        Wav,
        Flac,
        Ogg,
        Amr,
        Webm
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MediaFormat = MediaFormat' Lude.Text
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

pattern MP3 :: MediaFormat
pattern MP3 = MediaFormat' "mp3"

pattern MP4 :: MediaFormat
pattern MP4 = MediaFormat' "mp4"

pattern Wav :: MediaFormat
pattern Wav = MediaFormat' "wav"

pattern Flac :: MediaFormat
pattern Flac = MediaFormat' "flac"

pattern Ogg :: MediaFormat
pattern Ogg = MediaFormat' "ogg"

pattern Amr :: MediaFormat
pattern Amr = MediaFormat' "amr"

pattern Webm :: MediaFormat
pattern Webm = MediaFormat' "webm"

{-# COMPLETE
  MP3,
  MP4,
  Wav,
  Flac,
  Ogg,
  Amr,
  Webm,
  MediaFormat'
  #-}
