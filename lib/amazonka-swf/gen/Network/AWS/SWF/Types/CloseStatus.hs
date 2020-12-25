{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CloseStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CloseStatus
  ( CloseStatus
      ( CloseStatus',
        CloseStatusCompleted,
        CloseStatusFailed,
        CloseStatusCanceled,
        CloseStatusTerminated,
        CloseStatusContinuedAsNew,
        CloseStatusTimedOut,
        fromCloseStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CloseStatus = CloseStatus' {fromCloseStatus :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern CloseStatusCompleted :: CloseStatus
pattern CloseStatusCompleted = CloseStatus' "COMPLETED"

pattern CloseStatusFailed :: CloseStatus
pattern CloseStatusFailed = CloseStatus' "FAILED"

pattern CloseStatusCanceled :: CloseStatus
pattern CloseStatusCanceled = CloseStatus' "CANCELED"

pattern CloseStatusTerminated :: CloseStatus
pattern CloseStatusTerminated = CloseStatus' "TERMINATED"

pattern CloseStatusContinuedAsNew :: CloseStatus
pattern CloseStatusContinuedAsNew = CloseStatus' "CONTINUED_AS_NEW"

pattern CloseStatusTimedOut :: CloseStatus
pattern CloseStatusTimedOut = CloseStatus' "TIMED_OUT"

{-# COMPLETE
  CloseStatusCompleted,
  CloseStatusFailed,
  CloseStatusCanceled,
  CloseStatusTerminated,
  CloseStatusContinuedAsNew,
  CloseStatusTimedOut,
  CloseStatus'
  #-}
