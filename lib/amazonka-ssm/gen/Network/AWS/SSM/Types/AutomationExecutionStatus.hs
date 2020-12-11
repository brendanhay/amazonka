-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionStatus
  ( AutomationExecutionStatus
      ( AutomationExecutionStatus',
        AESCancelled,
        AESCancelling,
        AESFailed,
        AESInProgress,
        AESPending,
        AESSuccess,
        AESTimedOut,
        AESWaiting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutomationExecutionStatus = AutomationExecutionStatus' Lude.Text
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

pattern AESCancelled :: AutomationExecutionStatus
pattern AESCancelled = AutomationExecutionStatus' "Cancelled"

pattern AESCancelling :: AutomationExecutionStatus
pattern AESCancelling = AutomationExecutionStatus' "Cancelling"

pattern AESFailed :: AutomationExecutionStatus
pattern AESFailed = AutomationExecutionStatus' "Failed"

pattern AESInProgress :: AutomationExecutionStatus
pattern AESInProgress = AutomationExecutionStatus' "InProgress"

pattern AESPending :: AutomationExecutionStatus
pattern AESPending = AutomationExecutionStatus' "Pending"

pattern AESSuccess :: AutomationExecutionStatus
pattern AESSuccess = AutomationExecutionStatus' "Success"

pattern AESTimedOut :: AutomationExecutionStatus
pattern AESTimedOut = AutomationExecutionStatus' "TimedOut"

pattern AESWaiting :: AutomationExecutionStatus
pattern AESWaiting = AutomationExecutionStatus' "Waiting"

{-# COMPLETE
  AESCancelled,
  AESCancelling,
  AESFailed,
  AESInProgress,
  AESPending,
  AESSuccess,
  AESTimedOut,
  AESWaiting,
  AutomationExecutionStatus'
  #-}
