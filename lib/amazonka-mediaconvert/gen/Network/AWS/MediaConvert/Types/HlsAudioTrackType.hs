{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAudioTrackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAudioTrackType
  ( HlsAudioTrackType
      ( HlsAudioTrackType',
        AlternateAudioAutoSelect,
        AlternateAudioAutoSelectDefault,
        AlternateAudioNotAutoSelect,
        AudioOnlyVariantStream
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
newtype HlsAudioTrackType = HlsAudioTrackType' Lude.Text
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

pattern AlternateAudioAutoSelect :: HlsAudioTrackType
pattern AlternateAudioAutoSelect = HlsAudioTrackType' "ALTERNATE_AUDIO_AUTO_SELECT"

pattern AlternateAudioAutoSelectDefault :: HlsAudioTrackType
pattern AlternateAudioAutoSelectDefault = HlsAudioTrackType' "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"

pattern AlternateAudioNotAutoSelect :: HlsAudioTrackType
pattern AlternateAudioNotAutoSelect = HlsAudioTrackType' "ALTERNATE_AUDIO_NOT_AUTO_SELECT"

pattern AudioOnlyVariantStream :: HlsAudioTrackType
pattern AudioOnlyVariantStream = HlsAudioTrackType' "AUDIO_ONLY_VARIANT_STREAM"

{-# COMPLETE
  AlternateAudioAutoSelect,
  AlternateAudioAutoSelectDefault,
  AlternateAudioNotAutoSelect,
  AudioOnlyVariantStream,
  HlsAudioTrackType'
  #-}
