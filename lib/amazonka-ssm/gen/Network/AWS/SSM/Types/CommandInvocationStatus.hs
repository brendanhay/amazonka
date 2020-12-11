-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandInvocationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandInvocationStatus
  ( CommandInvocationStatus
      ( CommandInvocationStatus',
        CISCancelled,
        CISCancelling,
        CISDelayed,
        CISFailed,
        CISInProgress,
        CISPending,
        CISSuccess,
        CISTimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CommandInvocationStatus = CommandInvocationStatus' Lude.Text
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

pattern CISCancelled :: CommandInvocationStatus
pattern CISCancelled = CommandInvocationStatus' "Cancelled"

pattern CISCancelling :: CommandInvocationStatus
pattern CISCancelling = CommandInvocationStatus' "Cancelling"

pattern CISDelayed :: CommandInvocationStatus
pattern CISDelayed = CommandInvocationStatus' "Delayed"

pattern CISFailed :: CommandInvocationStatus
pattern CISFailed = CommandInvocationStatus' "Failed"

pattern CISInProgress :: CommandInvocationStatus
pattern CISInProgress = CommandInvocationStatus' "InProgress"

pattern CISPending :: CommandInvocationStatus
pattern CISPending = CommandInvocationStatus' "Pending"

pattern CISSuccess :: CommandInvocationStatus
pattern CISSuccess = CommandInvocationStatus' "Success"

pattern CISTimedOut :: CommandInvocationStatus
pattern CISTimedOut = CommandInvocationStatus' "TimedOut"

{-# COMPLETE
  CISCancelled,
  CISCancelling,
  CISDelayed,
  CISFailed,
  CISInProgress,
  CISPending,
  CISSuccess,
  CISTimedOut,
  CommandInvocationStatus'
  #-}
