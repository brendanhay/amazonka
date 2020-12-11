-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.VoiceRecordingTrack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.VoiceRecordingTrack
  ( VoiceRecordingTrack
      ( VoiceRecordingTrack',
        All,
        FromAgent,
        ToAgent
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VoiceRecordingTrack = VoiceRecordingTrack' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern All :: VoiceRecordingTrack
pattern All = VoiceRecordingTrack' "ALL"

pattern FromAgent :: VoiceRecordingTrack
pattern FromAgent = VoiceRecordingTrack' "FROM_AGENT"

pattern ToAgent :: VoiceRecordingTrack
pattern ToAgent = VoiceRecordingTrack' "TO_AGENT"

{-# COMPLETE
  All,
  FromAgent,
  ToAgent,
  VoiceRecordingTrack'
  #-}
