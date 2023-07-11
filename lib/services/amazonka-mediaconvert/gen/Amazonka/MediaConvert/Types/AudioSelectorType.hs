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
-- Module      : Amazonka.MediaConvert.Types.AudioSelectorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioSelectorType
  ( AudioSelectorType
      ( ..,
        AudioSelectorType_HLS_RENDITION_GROUP,
        AudioSelectorType_LANGUAGE_CODE,
        AudioSelectorType_PID,
        AudioSelectorType_TRACK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the type of the audio selector.
newtype AudioSelectorType = AudioSelectorType'
  { fromAudioSelectorType ::
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

pattern AudioSelectorType_HLS_RENDITION_GROUP :: AudioSelectorType
pattern AudioSelectorType_HLS_RENDITION_GROUP = AudioSelectorType' "HLS_RENDITION_GROUP"

pattern AudioSelectorType_LANGUAGE_CODE :: AudioSelectorType
pattern AudioSelectorType_LANGUAGE_CODE = AudioSelectorType' "LANGUAGE_CODE"

pattern AudioSelectorType_PID :: AudioSelectorType
pattern AudioSelectorType_PID = AudioSelectorType' "PID"

pattern AudioSelectorType_TRACK :: AudioSelectorType
pattern AudioSelectorType_TRACK = AudioSelectorType' "TRACK"

{-# COMPLETE
  AudioSelectorType_HLS_RENDITION_GROUP,
  AudioSelectorType_LANGUAGE_CODE,
  AudioSelectorType_PID,
  AudioSelectorType_TRACK,
  AudioSelectorType'
  #-}
