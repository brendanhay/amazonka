{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioCodec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioCodec
  ( AudioCodec
      ( AudioCodec',
        AudioCodecAac,
        AudioCodecMP2,
        AudioCodecMP3,
        AudioCodecWav,
        AudioCodecAiff,
        AudioCodecAC3,
        AudioCodecEAC3,
        AudioCodecEAC3Atmos,
        AudioCodecVorbis,
        AudioCodecOpus,
        AudioCodecPassthrough,
        fromAudioCodec
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Type of Audio codec.
newtype AudioCodec = AudioCodec' {fromAudioCodec :: Core.Text}
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

pattern AudioCodecAac :: AudioCodec
pattern AudioCodecAac = AudioCodec' "AAC"

pattern AudioCodecMP2 :: AudioCodec
pattern AudioCodecMP2 = AudioCodec' "MP2"

pattern AudioCodecMP3 :: AudioCodec
pattern AudioCodecMP3 = AudioCodec' "MP3"

pattern AudioCodecWav :: AudioCodec
pattern AudioCodecWav = AudioCodec' "WAV"

pattern AudioCodecAiff :: AudioCodec
pattern AudioCodecAiff = AudioCodec' "AIFF"

pattern AudioCodecAC3 :: AudioCodec
pattern AudioCodecAC3 = AudioCodec' "AC3"

pattern AudioCodecEAC3 :: AudioCodec
pattern AudioCodecEAC3 = AudioCodec' "EAC3"

pattern AudioCodecEAC3Atmos :: AudioCodec
pattern AudioCodecEAC3Atmos = AudioCodec' "EAC3_ATMOS"

pattern AudioCodecVorbis :: AudioCodec
pattern AudioCodecVorbis = AudioCodec' "VORBIS"

pattern AudioCodecOpus :: AudioCodec
pattern AudioCodecOpus = AudioCodec' "OPUS"

pattern AudioCodecPassthrough :: AudioCodec
pattern AudioCodecPassthrough = AudioCodec' "PASSTHROUGH"

{-# COMPLETE
  AudioCodecAac,
  AudioCodecMP2,
  AudioCodecMP3,
  AudioCodecWav,
  AudioCodecAiff,
  AudioCodecAC3,
  AudioCodecEAC3,
  AudioCodecEAC3Atmos,
  AudioCodecVorbis,
  AudioCodecOpus,
  AudioCodecPassthrough,
  AudioCodec'
  #-}
