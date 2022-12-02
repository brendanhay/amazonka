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
-- Module      : Amazonka.MediaConvert.Types.CmfcAudioTrackType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmfcAudioTrackType
  ( CmfcAudioTrackType
      ( ..,
        CmfcAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT,
        CmfcAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT,
        CmfcAudioTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this setting to control the values that MediaConvert puts in your
-- HLS parent playlist to control how the client player selects which audio
-- track to play. The other options for this setting determine the values
-- that MediaConvert writes for the DEFAULT and AUTOSELECT attributes of
-- the EXT-X-MEDIA entry for the audio variant. For more information about
-- these attributes, see the Apple documentation article
-- https:\/\/developer.apple.com\/documentation\/http_live_streaming\/example_playlists_for_http_live_streaming\/adding_alternate_media_to_a_playlist.
-- Choose Alternate audio, auto select, default
-- (ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT) to set DEFAULT=YES and
-- AUTOSELECT=YES. Choose this value for only one variant in your output
-- group. Choose Alternate audio, auto select, not default
-- (ALTERNATE_AUDIO_AUTO_SELECT) to set DEFAULT=NO and AUTOSELECT=YES.
-- Choose Alternate Audio, Not Auto Select to set DEFAULT=NO and
-- AUTOSELECT=NO. When you don\'t specify a value for this setting,
-- MediaConvert defaults to Alternate audio, auto select, default. When
-- there is more than one variant in your output group, you must explicitly
-- choose a value for this setting.
newtype CmfcAudioTrackType = CmfcAudioTrackType'
  { fromCmfcAudioTrackType ::
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

pattern CmfcAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT :: CmfcAudioTrackType
pattern CmfcAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT = CmfcAudioTrackType' "ALTERNATE_AUDIO_AUTO_SELECT"

pattern CmfcAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT :: CmfcAudioTrackType
pattern CmfcAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT = CmfcAudioTrackType' "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"

pattern CmfcAudioTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT :: CmfcAudioTrackType
pattern CmfcAudioTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT = CmfcAudioTrackType' "ALTERNATE_AUDIO_NOT_AUTO_SELECT"

{-# COMPLETE
  CmfcAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT,
  CmfcAudioTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT,
  CmfcAudioTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT,
  CmfcAudioTrackType'
  #-}
