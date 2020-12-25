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
        HlsAudioTrackTypeAlternateAudioAutoSelectDefault,
        HlsAudioTrackTypeAlternateAudioAutoSelect,
        HlsAudioTrackTypeAlternateAudioNotAutoSelect,
        HlsAudioTrackTypeAudioOnlyVariantStream,
        fromHlsAudioTrackType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
newtype HlsAudioTrackType = HlsAudioTrackType'
  { fromHlsAudioTrackType ::
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

pattern HlsAudioTrackTypeAlternateAudioAutoSelectDefault :: HlsAudioTrackType
pattern HlsAudioTrackTypeAlternateAudioAutoSelectDefault = HlsAudioTrackType' "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"

pattern HlsAudioTrackTypeAlternateAudioAutoSelect :: HlsAudioTrackType
pattern HlsAudioTrackTypeAlternateAudioAutoSelect = HlsAudioTrackType' "ALTERNATE_AUDIO_AUTO_SELECT"

pattern HlsAudioTrackTypeAlternateAudioNotAutoSelect :: HlsAudioTrackType
pattern HlsAudioTrackTypeAlternateAudioNotAutoSelect = HlsAudioTrackType' "ALTERNATE_AUDIO_NOT_AUTO_SELECT"

pattern HlsAudioTrackTypeAudioOnlyVariantStream :: HlsAudioTrackType
pattern HlsAudioTrackTypeAudioOnlyVariantStream = HlsAudioTrackType' "AUDIO_ONLY_VARIANT_STREAM"

{-# COMPLETE
  HlsAudioTrackTypeAlternateAudioAutoSelectDefault,
  HlsAudioTrackTypeAlternateAudioAutoSelect,
  HlsAudioTrackTypeAlternateAudioNotAutoSelect,
  HlsAudioTrackTypeAudioOnlyVariantStream,
  HlsAudioTrackType'
  #-}
