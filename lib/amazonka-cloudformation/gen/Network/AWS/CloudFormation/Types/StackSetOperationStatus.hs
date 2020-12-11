-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationStatus
  ( StackSetOperationStatus
      ( StackSetOperationStatus',
        SSOSFailed,
        SSOSQueued,
        SSOSRunning,
        SSOSStopped,
        SSOSStopping,
        SSOSSucceeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StackSetOperationStatus = StackSetOperationStatus' Lude.Text
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

pattern SSOSFailed :: StackSetOperationStatus
pattern SSOSFailed = StackSetOperationStatus' "FAILED"

pattern SSOSQueued :: StackSetOperationStatus
pattern SSOSQueued = StackSetOperationStatus' "QUEUED"

pattern SSOSRunning :: StackSetOperationStatus
pattern SSOSRunning = StackSetOperationStatus' "RUNNING"

pattern SSOSStopped :: StackSetOperationStatus
pattern SSOSStopped = StackSetOperationStatus' "STOPPED"

pattern SSOSStopping :: StackSetOperationStatus
pattern SSOSStopping = StackSetOperationStatus' "STOPPING"

pattern SSOSSucceeded :: StackSetOperationStatus
pattern SSOSSucceeded = StackSetOperationStatus' "SUCCEEDED"

{-# COMPLETE
  SSOSFailed,
  SSOSQueued,
  SSOSRunning,
  SSOSStopped,
  SSOSStopping,
  SSOSSucceeded,
  StackSetOperationStatus'
  #-}
