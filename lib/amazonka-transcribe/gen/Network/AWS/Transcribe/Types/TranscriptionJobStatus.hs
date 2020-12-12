{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.TranscriptionJobStatus
  ( TranscriptionJobStatus
      ( TranscriptionJobStatus',
        TJSCompleted,
        TJSFailed,
        TJSInProgress,
        TJSQueued
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TranscriptionJobStatus = TranscriptionJobStatus' Lude.Text
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

pattern TJSCompleted :: TranscriptionJobStatus
pattern TJSCompleted = TranscriptionJobStatus' "COMPLETED"

pattern TJSFailed :: TranscriptionJobStatus
pattern TJSFailed = TranscriptionJobStatus' "FAILED"

pattern TJSInProgress :: TranscriptionJobStatus
pattern TJSInProgress = TranscriptionJobStatus' "IN_PROGRESS"

pattern TJSQueued :: TranscriptionJobStatus
pattern TJSQueued = TranscriptionJobStatus' "QUEUED"

{-# COMPLETE
  TJSCompleted,
  TJSFailed,
  TJSInProgress,
  TJSQueued,
  TranscriptionJobStatus'
  #-}
