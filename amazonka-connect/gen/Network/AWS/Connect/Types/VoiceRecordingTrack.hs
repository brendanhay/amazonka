{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype VoiceRecordingTrack = VoiceRecordingTrack'
  { fromVoiceRecordingTrack ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
