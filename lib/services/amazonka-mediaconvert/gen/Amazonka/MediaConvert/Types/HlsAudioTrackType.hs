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
-- Module      : Amazonka.MediaConvert.Types.HlsAudioTrackType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsAudioTrackType
  ( HlsAudioTrackType
      ( ..,
        HlsAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT,
        HlsAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT,
        HlsAudioTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT,
        HlsAudioTrackType_AUDIO_ONLY_VARIANT_STREAM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream
-- The client can play back this audio-only stream instead of video in
-- low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS
-- manifest. Alternate Audio, Auto Select, Default Alternate rendition that
-- the client should try to play back by default. Represented as an
-- EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES
-- Alternate Audio, Auto Select, Not Default Alternate rendition that the
-- client may try to play back by default. Represented as an EXT-X-MEDIA in
-- the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not
-- Auto Select Alternate rendition that the client will not try to play
-- back by default. Represented as an EXT-X-MEDIA in the HLS manifest with
-- DEFAULT=NO, AUTOSELECT=NO
newtype HlsAudioTrackType = HlsAudioTrackType'
  { fromHlsAudioTrackType ::
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

pattern HlsAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT :: HlsAudioTrackType
pattern HlsAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT = HlsAudioTrackType' "ALTERNATE_AUDIO_AUTO_SELECT"

pattern HlsAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT :: HlsAudioTrackType
pattern HlsAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT = HlsAudioTrackType' "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"

pattern HlsAudioTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT :: HlsAudioTrackType
pattern HlsAudioTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT = HlsAudioTrackType' "ALTERNATE_AUDIO_NOT_AUTO_SELECT"

pattern HlsAudioTrackType_AUDIO_ONLY_VARIANT_STREAM :: HlsAudioTrackType
pattern HlsAudioTrackType_AUDIO_ONLY_VARIANT_STREAM = HlsAudioTrackType' "AUDIO_ONLY_VARIANT_STREAM"

{-# COMPLETE
  HlsAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT,
  HlsAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT,
  HlsAudioTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT,
  HlsAudioTrackType_AUDIO_ONLY_VARIANT_STREAM,
  HlsAudioTrackType'
  #-}
