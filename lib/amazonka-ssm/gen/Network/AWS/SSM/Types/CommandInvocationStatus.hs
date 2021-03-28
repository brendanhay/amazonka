{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandInvocationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.CommandInvocationStatus
  ( CommandInvocationStatus
    ( CommandInvocationStatus'
    , CommandInvocationStatusPending
    , CommandInvocationStatusInProgress
    , CommandInvocationStatusDelayed
    , CommandInvocationStatusSuccess
    , CommandInvocationStatusCancelled
    , CommandInvocationStatusTimedOut
    , CommandInvocationStatusFailed
    , CommandInvocationStatusCancelling
    , fromCommandInvocationStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CommandInvocationStatus = CommandInvocationStatus'{fromCommandInvocationStatus
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern CommandInvocationStatusPending :: CommandInvocationStatus
pattern CommandInvocationStatusPending = CommandInvocationStatus' "Pending"

pattern CommandInvocationStatusInProgress :: CommandInvocationStatus
pattern CommandInvocationStatusInProgress = CommandInvocationStatus' "InProgress"

pattern CommandInvocationStatusDelayed :: CommandInvocationStatus
pattern CommandInvocationStatusDelayed = CommandInvocationStatus' "Delayed"

pattern CommandInvocationStatusSuccess :: CommandInvocationStatus
pattern CommandInvocationStatusSuccess = CommandInvocationStatus' "Success"

pattern CommandInvocationStatusCancelled :: CommandInvocationStatus
pattern CommandInvocationStatusCancelled = CommandInvocationStatus' "Cancelled"

pattern CommandInvocationStatusTimedOut :: CommandInvocationStatus
pattern CommandInvocationStatusTimedOut = CommandInvocationStatus' "TimedOut"

pattern CommandInvocationStatusFailed :: CommandInvocationStatus
pattern CommandInvocationStatusFailed = CommandInvocationStatus' "Failed"

pattern CommandInvocationStatusCancelling :: CommandInvocationStatus
pattern CommandInvocationStatusCancelling = CommandInvocationStatus' "Cancelling"

{-# COMPLETE 
  CommandInvocationStatusPending,

  CommandInvocationStatusInProgress,

  CommandInvocationStatusDelayed,

  CommandInvocationStatusSuccess,

  CommandInvocationStatusCancelled,

  CommandInvocationStatusTimedOut,

  CommandInvocationStatusFailed,

  CommandInvocationStatusCancelling,
  CommandInvocationStatus'
  #-}
