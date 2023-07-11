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
-- Module      : Amazonka.MediaLive.Types.AudioOnlyHlsTrackType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioOnlyHlsTrackType
  ( AudioOnlyHlsTrackType
      ( ..,
        AudioOnlyHlsTrackType_ALTERNATE_AUDIO_AUTO_SELECT,
        AudioOnlyHlsTrackType_ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT,
        AudioOnlyHlsTrackType_ALTERNATE_AUDIO_NOT_AUTO_SELECT,
        AudioOnlyHlsTrackType_AUDIO_ONLY_VARIANT_STREAM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Audio Only Hls Track Type
newtype AudioOnlyHlsTrackType = AudioOnlyHlsTrackType'
  { fromAudioOnlyHlsTrackType ::
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
