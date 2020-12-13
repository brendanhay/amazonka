{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionStatus
  ( ExecutionStatus
      ( ExecutionStatus',
        ESRunning,
        ESSucceeded,
        ESFailed,
        ESTimedOut,
        ESAborted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ExecutionStatus = ExecutionStatus' Lude.Text
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

pattern ESRunning :: ExecutionStatus
pattern ESRunning = ExecutionStatus' "RUNNING"

pattern ESSucceeded :: ExecutionStatus
pattern ESSucceeded = ExecutionStatus' "SUCCEEDED"

pattern ESFailed :: ExecutionStatus
pattern ESFailed = ExecutionStatus' "FAILED"

pattern ESTimedOut :: ExecutionStatus
pattern ESTimedOut = ExecutionStatus' "TIMED_OUT"

pattern ESAborted :: ExecutionStatus
pattern ESAborted = ExecutionStatus' "ABORTED"

{-# COMPLETE
  ESRunning,
  ESSucceeded,
  ESFailed,
  ESTimedOut,
  ESAborted,
  ExecutionStatus'
  #-}
