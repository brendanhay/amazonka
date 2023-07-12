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
-- Module      : Amazonka.Connect.Types.VoiceRecordingTrack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.VoiceRecordingTrack
  ( VoiceRecordingTrack
      ( ..,
        VoiceRecordingTrack_ALL,
        VoiceRecordingTrack_FROM_AGENT,
        VoiceRecordingTrack_TO_AGENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VoiceRecordingTrack = VoiceRecordingTrack'
  { fromVoiceRecordingTrack ::
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
