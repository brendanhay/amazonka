{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioCodec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioCodec
  ( AudioCodec
      ( ..,
        AudioCodec_AAC,
        AudioCodec_AC3,
        AudioCodec_AIFF,
        AudioCodec_EAC3,
        AudioCodec_EAC3_ATMOS,
        AudioCodec_MP2,
        AudioCodec_MP3,
        AudioCodec_OPUS,
        AudioCodec_PASSTHROUGH,
        AudioCodec_VORBIS,
        AudioCodec_WAV
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Type of Audio codec.
newtype AudioCodec = AudioCodec'
  { fromAudioCodec ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AudioCodec_AAC :: AudioCodec
pattern AudioCodec_AAC = AudioCodec' "AAC"

pattern AudioCodec_AC3 :: AudioCodec
pattern AudioCodec_AC3 = AudioCodec' "AC3"

pattern AudioCodec_AIFF :: AudioCodec
pattern AudioCodec_AIFF = AudioCodec' "AIFF"

pattern AudioCodec_EAC3 :: AudioCodec
pattern AudioCodec_EAC3 = AudioCodec' "EAC3"

pattern AudioCodec_EAC3_ATMOS :: AudioCodec
pattern AudioCodec_EAC3_ATMOS = AudioCodec' "EAC3_ATMOS"

pattern AudioCodec_MP2 :: AudioCodec
pattern AudioCodec_MP2 = AudioCodec' "MP2"

pattern AudioCodec_MP3 :: AudioCodec
pattern AudioCodec_MP3 = AudioCodec' "MP3"

pattern AudioCodec_OPUS :: AudioCodec
pattern AudioCodec_OPUS = AudioCodec' "OPUS"

pattern AudioCodec_PASSTHROUGH :: AudioCodec
pattern AudioCodec_PASSTHROUGH = AudioCodec' "PASSTHROUGH"

pattern AudioCodec_VORBIS :: AudioCodec
pattern AudioCodec_VORBIS = AudioCodec' "VORBIS"

pattern AudioCodec_WAV :: AudioCodec
pattern AudioCodec_WAV = AudioCodec' "WAV"

{-# COMPLETE
  AudioCodec_AAC,
  AudioCodec_AC3,
  AudioCodec_AIFF,
  AudioCodec_EAC3,
  AudioCodec_EAC3_ATMOS,
  AudioCodec_MP2,
  AudioCodec_MP3,
  AudioCodec_OPUS,
  AudioCodec_PASSTHROUGH,
  AudioCodec_VORBIS,
  AudioCodec_WAV,
  AudioCodec'
  #-}
