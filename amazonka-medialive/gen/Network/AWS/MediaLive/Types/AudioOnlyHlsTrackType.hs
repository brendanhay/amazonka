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
-- Module      : Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType
  ( AudioOnlyHlsTrackType
      ( ..,
        AudioOnlyHlsTrackType_ALTERNATE_AUDIO_AUTO_SELECT,
        AudioOnlyHlsTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT,
        AudioOnlyHlsTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT,
        AudioOnlyHlsTrackType_AUDIO_ONLY_VARIANT_STREAM
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Audio Only Hls Track Type
newtype AudioOnlyHlsTrackType = AudioOnlyHlsTrackType'
  { fromAudioOnlyHlsTrackType ::
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

pattern AudioOnlyHlsTrackType_ALTERNATE_AUDIO_AUTO_SELECT :: AudioOnlyHlsTrackType
pattern AudioOnlyHlsTrackType_ALTERNATE_AUDIO_AUTO_SELECT = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_AUTO_SELECT"

pattern AudioOnlyHlsTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT :: AudioOnlyHlsTrackType
pattern AudioOnlyHlsTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"

pattern AudioOnlyHlsTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT :: AudioOnlyHlsTrackType
pattern AudioOnlyHlsTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_NOT_AUTO_SELECT"

pattern AudioOnlyHlsTrackType_AUDIO_ONLY_VARIANT_STREAM :: AudioOnlyHlsTrackType
pattern AudioOnlyHlsTrackType_AUDIO_ONLY_VARIANT_STREAM = AudioOnlyHlsTrackType' "AUDIO_ONLY_VARIANT_STREAM"

{-# COMPLETE
  AudioOnlyHlsTrackType_ALTERNATE_AUDIO_AUTO_SELECT,
  AudioOnlyHlsTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT,
  AudioOnlyHlsTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT,
  AudioOnlyHlsTrackType_AUDIO_ONLY_VARIANT_STREAM,
  AudioOnlyHlsTrackType'
  #-}
