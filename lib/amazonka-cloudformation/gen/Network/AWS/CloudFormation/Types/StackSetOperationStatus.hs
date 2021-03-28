{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSetOperationStatus
  ( StackSetOperationStatus
    ( StackSetOperationStatus'
    , StackSetOperationStatusRunning
    , StackSetOperationStatusSucceeded
    , StackSetOperationStatusFailed
    , StackSetOperationStatusStopping
    , StackSetOperationStatusStopped
    , StackSetOperationStatusQueued
    , fromStackSetOperationStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype StackSetOperationStatus = StackSetOperationStatus'{fromStackSetOperationStatus
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern StackSetOperationStatusRunning :: StackSetOperationStatus
pattern StackSetOperationStatusRunning = StackSetOperationStatus' "RUNNING"

pattern StackSetOperationStatusSucceeded :: StackSetOperationStatus
pattern StackSetOperationStatusSucceeded = StackSetOperationStatus' "SUCCEEDED"

pattern StackSetOperationStatusFailed :: StackSetOperationStatus
pattern StackSetOperationStatusFailed = StackSetOperationStatus' "FAILED"

pattern StackSetOperationStatusStopping :: StackSetOperationStatus
pattern StackSetOperationStatusStopping = StackSetOperationStatus' "STOPPING"

pattern StackSetOperationStatusStopped :: StackSetOperationStatus
pattern StackSetOperationStatusStopped = StackSetOperationStatus' "STOPPED"

pattern StackSetOperationStatusQueued :: StackSetOperationStatus
pattern StackSetOperationStatusQueued = StackSetOperationStatus' "QUEUED"

{-# COMPLETE 
  StackSetOperationStatusRunning,

  StackSetOperationStatusSucceeded,

  StackSetOperationStatusFailed,

  StackSetOperationStatusStopping,

  StackSetOperationStatusStopped,

  StackSetOperationStatusQueued,
  StackSetOperationStatus'
  #-}
