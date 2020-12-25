{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        CommandStatusPending,
        CommandStatusInProgress,
        CommandStatusSuccess,
        CommandStatusCancelled,
        CommandStatusFailed,
        CommandStatusTimedOut,
        CommandStatusCancelling,
        fromCommandStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CommandStatus = CommandStatus'
  { fromCommandStatus ::
      Core.Text
  }
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

pattern CommandStatusPending :: CommandStatus
pattern CommandStatusPending = CommandStatus' "Pending"

pattern CommandStatusInProgress :: CommandStatus
pattern CommandStatusInProgress = CommandStatus' "InProgress"

pattern CommandStatusSuccess :: CommandStatus
pattern CommandStatusSuccess = CommandStatus' "Success"

pattern CommandStatusCancelled :: CommandStatus
pattern CommandStatusCancelled = CommandStatus' "Cancelled"

pattern CommandStatusFailed :: CommandStatus
pattern CommandStatusFailed = CommandStatus' "Failed"

pattern CommandStatusTimedOut :: CommandStatus
pattern CommandStatusTimedOut = CommandStatus' "TimedOut"

pattern CommandStatusCancelling :: CommandStatus
pattern CommandStatusCancelling = CommandStatus' "Cancelling"

{-# COMPLETE
  CommandStatusPending,
  CommandStatusInProgress,
  CommandStatusSuccess,
  CommandStatusCancelled,
  CommandStatusFailed,
  CommandStatusTimedOut,
  CommandStatusCancelling,
  CommandStatus'
  #-}
