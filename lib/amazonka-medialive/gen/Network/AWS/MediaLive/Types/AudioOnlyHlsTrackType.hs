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
        AlternateAudioAutoSelect,
        AlternateAudioAutoSelectDefault,
        AlternateAudioNotAutoSelect,
        AudioOnlyVariantStream
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Audio Only Hls Track Type
newtype AudioOnlyHlsTrackType = AudioOnlyHlsTrackType' Lude.Text
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

pattern AlternateAudioAutoSelect :: AudioOnlyHlsTrackType
pattern AlternateAudioAutoSelect = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_AUTO_SELECT"

pattern AlternateAudioAutoSelectDefault :: AudioOnlyHlsTrackType
pattern AlternateAudioAutoSelectDefault = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"

pattern AlternateAudioNotAutoSelect :: AudioOnlyHlsTrackType
pattern AlternateAudioNotAutoSelect = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_NOT_AUTO_SELECT"

pattern AudioOnlyVariantStream :: AudioOnlyHlsTrackType
pattern AudioOnlyVariantStream = AudioOnlyHlsTrackType' "AUDIO_ONLY_VARIANT_STREAM"

{-# COMPLETE
  AlternateAudioAutoSelect,
  AlternateAudioAutoSelectDefault,
  AlternateAudioNotAutoSelect,
  AudioOnlyVariantStream,
  AudioOnlyHlsTrackType'
  #-}
