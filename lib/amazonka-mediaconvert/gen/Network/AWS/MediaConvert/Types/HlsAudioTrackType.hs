{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAudioTrackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAudioTrackType where

import Network.AWS.Prelude

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
data HlsAudioTrackType
  = AlternateAudioAutoSelect
  | AlternateAudioAutoSelectDefault
  | AlternateAudioNotAutoSelect
  | AudioOnlyVariantStream
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText HlsAudioTrackType where
  parser =
    takeLowerText >>= \case
      "alternate_audio_auto_select" -> pure AlternateAudioAutoSelect
      "alternate_audio_auto_select_default" -> pure AlternateAudioAutoSelectDefault
      "alternate_audio_not_auto_select" -> pure AlternateAudioNotAutoSelect
      "audio_only_variant_stream" -> pure AudioOnlyVariantStream
      e ->
        fromTextError $
          "Failure parsing HlsAudioTrackType from value: '" <> e
            <> "'. Accepted values: alternate_audio_auto_select, alternate_audio_auto_select_default, alternate_audio_not_auto_select, audio_only_variant_stream"

instance ToText HlsAudioTrackType where
  toText = \case
    AlternateAudioAutoSelect -> "ALTERNATE_AUDIO_AUTO_SELECT"
    AlternateAudioAutoSelectDefault -> "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"
    AlternateAudioNotAutoSelect -> "ALTERNATE_AUDIO_NOT_AUTO_SELECT"
    AudioOnlyVariantStream -> "AUDIO_ONLY_VARIANT_STREAM"

instance Hashable HlsAudioTrackType

instance NFData HlsAudioTrackType

instance ToByteString HlsAudioTrackType

instance ToQuery HlsAudioTrackType

instance ToHeader HlsAudioTrackType

instance ToJSON HlsAudioTrackType where
  toJSON = toJSONText

instance FromJSON HlsAudioTrackType where
  parseJSON = parseJSONText "HlsAudioTrackType"
