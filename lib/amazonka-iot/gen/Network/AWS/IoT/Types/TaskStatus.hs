-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TaskStatus
  ( TaskStatus
      ( TaskStatus',
        TSCancelled,
        TSCancelling,
        TSCompleted,
        TSFailed,
        TSInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TaskStatus = TaskStatus' Lude.Text
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

pattern TSCancelled :: TaskStatus
pattern TSCancelled = TaskStatus' "Cancelled"

pattern TSCancelling :: TaskStatus
pattern TSCancelling = TaskStatus' "Cancelling"

pattern TSCompleted :: TaskStatus
pattern TSCompleted = TaskStatus' "Completed"

pattern TSFailed :: TaskStatus
pattern TSFailed = TaskStatus' "Failed"

pattern TSInProgress :: TaskStatus
pattern TSInProgress = TaskStatus' "InProgress"

{-# COMPLETE
  TSCancelled,
  TSCancelling,
  TSCompleted,
  TSFailed,
  TSInProgress,
  TaskStatus'
  #-}
