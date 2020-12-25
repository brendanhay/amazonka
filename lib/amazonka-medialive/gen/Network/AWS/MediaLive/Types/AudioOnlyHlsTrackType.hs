{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType
  ( AudioOnlyHlsTrackType
      ( AudioOnlyHlsTrackType',
        AudioOnlyHlsTrackTypeAlternateAudioAutoSelect,
        AudioOnlyHlsTrackTypeAlternateAudioAutoSelectDefault,
        AudioOnlyHlsTrackTypeAlternateAudioNotAutoSelect,
        AudioOnlyHlsTrackTypeAudioOnlyVariantStream,
        fromAudioOnlyHlsTrackType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Audio Only Hls Track Type
newtype AudioOnlyHlsTrackType = AudioOnlyHlsTrackType'
  { fromAudioOnlyHlsTrackType ::
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

pattern AudioOnlyHlsTrackTypeAlternateAudioAutoSelect :: AudioOnlyHlsTrackType
pattern AudioOnlyHlsTrackTypeAlternateAudioAutoSelect = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_AUTO_SELECT"

pattern AudioOnlyHlsTrackTypeAlternateAudioAutoSelectDefault :: AudioOnlyHlsTrackType
pattern AudioOnlyHlsTrackTypeAlternateAudioAutoSelectDefault = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"

pattern AudioOnlyHlsTrackTypeAlternateAudioNotAutoSelect :: AudioOnlyHlsTrackType
pattern AudioOnlyHlsTrackTypeAlternateAudioNotAutoSelect = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_NOT_AUTO_SELECT"

pattern AudioOnlyHlsTrackTypeAudioOnlyVariantStream :: AudioOnlyHlsTrackType
pattern AudioOnlyHlsTrackTypeAudioOnlyVariantStream = AudioOnlyHlsTrackType' "AUDIO_ONLY_VARIANT_STREAM"

{-# COMPLETE
  AudioOnlyHlsTrackTypeAlternateAudioAutoSelect,
  AudioOnlyHlsTrackTypeAlternateAudioAutoSelectDefault,
  AudioOnlyHlsTrackTypeAlternateAudioNotAutoSelect,
  AudioOnlyHlsTrackTypeAudioOnlyVariantStream,
  AudioOnlyHlsTrackType'
  #-}
