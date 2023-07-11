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
-- Module      : Amazonka.MediaConvert.Types.AudioCodec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioCodec
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose the audio codec for this output. Note that the option Dolby
-- Digital passthrough (PASSTHROUGH) applies only to Dolby Digital and
-- Dolby Digital Plus audio inputs. Make sure that you choose a codec
-- that\'s supported with your output container:
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#reference-codecs-containers-output-audio
-- For audio-only outputs, make sure that both your input audio codec and
-- your output audio codec are supported for audio-only workflows. For more
-- information, see:
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers-input.html#reference-codecs-containers-input-audio-only
-- and
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#audio-only-output
newtype AudioCodec = AudioCodec'
  { fromAudioCodec ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
