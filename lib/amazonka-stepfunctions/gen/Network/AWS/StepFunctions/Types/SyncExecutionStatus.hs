{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.SyncExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.SyncExecutionStatus
  ( SyncExecutionStatus
    ( SyncExecutionStatus'
    , SyncExecutionStatusSucceeded
    , SyncExecutionStatusFailed
    , SyncExecutionStatusTimedOut
    , fromSyncExecutionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SyncExecutionStatus = SyncExecutionStatus'{fromSyncExecutionStatus
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern SyncExecutionStatusSucceeded :: SyncExecutionStatus
pattern SyncExecutionStatusSucceeded = SyncExecutionStatus' "SUCCEEDED"

pattern SyncExecutionStatusFailed :: SyncExecutionStatus
pattern SyncExecutionStatusFailed = SyncExecutionStatus' "FAILED"

pattern SyncExecutionStatusTimedOut :: SyncExecutionStatus
pattern SyncExecutionStatusTimedOut = SyncExecutionStatus' "TIMED_OUT"

{-# COMPLETE 
  SyncExecutionStatusSucceeded,

  SyncExecutionStatusFailed,

  SyncExecutionStatusTimedOut,
  SyncExecutionStatus'
  #-}
