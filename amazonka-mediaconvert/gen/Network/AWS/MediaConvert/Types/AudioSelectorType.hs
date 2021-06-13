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
-- Module      : Network.AWS.MediaConvert.Types.AudioSelectorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioSelectorType
  ( AudioSelectorType
      ( ..,
        AudioSelectorType_LANGUAGE_CODE,
        AudioSelectorType_PID,
        AudioSelectorType_TRACK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the type of the audio selector.
newtype AudioSelectorType = AudioSelectorType'
  { fromAudioSelectorType ::
      Core.Text
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

pattern AudioSelectorType_LANGUAGE_CODE :: AudioSelectorType
pattern AudioSelectorType_LANGUAGE_CODE = AudioSelectorType' "LANGUAGE_CODE"

pattern AudioSelectorType_PID :: AudioSelectorType
pattern AudioSelectorType_PID = AudioSelectorType' "PID"

pattern AudioSelectorType_TRACK :: AudioSelectorType
pattern AudioSelectorType_TRACK = AudioSelectorType' "TRACK"

{-# COMPLETE
  AudioSelectorType_LANGUAGE_CODE,
  AudioSelectorType_PID,
  AudioSelectorType_TRACK,
  AudioSelectorType'
  #-}
