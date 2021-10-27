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
-- Module      : Network.AWS.LexV2Runtime.Types.PlaybackInterruptionReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.PlaybackInterruptionReason
  ( PlaybackInterruptionReason
      ( ..,
        PlaybackInterruptionReason_DTMF_START_DETECTED,
        PlaybackInterruptionReason_TEXT_DETECTED,
        PlaybackInterruptionReason_VOICE_START_DETECTED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PlaybackInterruptionReason = PlaybackInterruptionReason'
  { fromPlaybackInterruptionReason ::
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

pattern PlaybackInterruptionReason_DTMF_START_DETECTED :: PlaybackInterruptionReason
pattern PlaybackInterruptionReason_DTMF_START_DETECTED = PlaybackInterruptionReason' "DTMF_START_DETECTED"

pattern PlaybackInterruptionReason_TEXT_DETECTED :: PlaybackInterruptionReason
pattern PlaybackInterruptionReason_TEXT_DETECTED = PlaybackInterruptionReason' "TEXT_DETECTED"

pattern PlaybackInterruptionReason_VOICE_START_DETECTED :: PlaybackInterruptionReason
pattern PlaybackInterruptionReason_VOICE_START_DETECTED = PlaybackInterruptionReason' "VOICE_START_DETECTED"

{-# COMPLETE
  PlaybackInterruptionReason_DTMF_START_DETECTED,
  PlaybackInterruptionReason_TEXT_DETECTED,
  PlaybackInterruptionReason_VOICE_START_DETECTED,
  PlaybackInterruptionReason'
  #-}
