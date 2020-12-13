{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        SSOSRunning,
        SSOSSucceeded,
        SSOSFailed,
        SSOSStopping,
        SSOSStopped,
        SSOSQueued
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

pattern SSOSRunning :: StackSetOperationStatus
pattern SSOSRunning = StackSetOperationStatus' "RUNNING"

pattern SSOSSucceeded :: StackSetOperationStatus
pattern SSOSSucceeded = StackSetOperationStatus' "SUCCEEDED"

pattern SSOSFailed :: StackSetOperationStatus
pattern SSOSFailed = StackSetOperationStatus' "FAILED"

pattern SSOSStopping :: StackSetOperationStatus
pattern SSOSStopping = StackSetOperationStatus' "STOPPING"

pattern SSOSStopped :: StackSetOperationStatus
pattern SSOSStopped = StackSetOperationStatus' "STOPPED"

pattern SSOSQueued :: StackSetOperationStatus
pattern SSOSQueued = StackSetOperationStatus' "QUEUED"

{-# COMPLETE
  SSOSRunning,
  SSOSSucceeded,
  SSOSFailed,
  SSOSStopping,
  SSOSStopped,
  SSOSQueued,
  StackSetOperationStatus'
  #-}
