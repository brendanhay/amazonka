-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandStatus
  ( CommandStatus
      ( CommandStatus',
        CSCancelled,
        CSCancelling,
        CSFailed,
        CSInProgress,
        CSPending,
        CSSuccess,
        CSTimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CommandStatus = CommandStatus' Lude.Text
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

pattern CSCancelled :: CommandStatus
pattern CSCancelled = CommandStatus' "Cancelled"

pattern CSCancelling :: CommandStatus
pattern CSCancelling = CommandStatus' "Cancelling"

pattern CSFailed :: CommandStatus
pattern CSFailed = CommandStatus' "Failed"

pattern CSInProgress :: CommandStatus
pattern CSInProgress = CommandStatus' "InProgress"

pattern CSPending :: CommandStatus
pattern CSPending = CommandStatus' "Pending"

pattern CSSuccess :: CommandStatus
pattern CSSuccess = CommandStatus' "Success"

pattern CSTimedOut :: CommandStatus
pattern CSTimedOut = CommandStatus' "TimedOut"

{-# COMPLETE
  CSCancelled,
  CSCancelling,
  CSFailed,
  CSInProgress,
  CSPending,
  CSSuccess,
  CSTimedOut,
  CommandStatus'
  #-}
