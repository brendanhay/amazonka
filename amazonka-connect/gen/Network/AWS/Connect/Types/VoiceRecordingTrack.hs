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
-- Module      : Network.AWS.Connect.Types.VoiceRecordingTrack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.VoiceRecordingTrack
  ( VoiceRecordingTrack
      ( ..,
        VoiceRecordingTrack_ALL,
        VoiceRecordingTrack_FROM_AGENT,
        VoiceRecordingTrack_TO_AGENT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype VoiceRecordingTrack = VoiceRecordingTrack'
  { fromVoiceRecordingTrack ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern VoiceRecordingTrack_ALL :: VoiceRecordingTrack
pattern VoiceRecordingTrack_ALL = VoiceRecordingTrack' "ALL"

pattern VoiceRecordingTrack_FROM_AGENT :: VoiceRecordingTrack
pattern VoiceRecordingTrack_FROM_AGENT = VoiceRecordingTrack' "FROM_AGENT"

pattern VoiceRecordingTrack_TO_AGENT :: VoiceRecordingTrack
pattern VoiceRecordingTrack_TO_AGENT = VoiceRecordingTrack' "TO_AGENT"

{-# COMPLETE
  VoiceRecordingTrack_ALL,
  VoiceRecordingTrack_FROM_AGENT,
  VoiceRecordingTrack_TO_AGENT,
  VoiceRecordingTrack'
  #-}
