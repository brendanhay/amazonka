{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleEventStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.LifecycleEventStatus
  ( LifecycleEventStatus
    ( LifecycleEventStatus'
    , LifecycleEventStatusPending
    , LifecycleEventStatusInProgress
    , LifecycleEventStatusSucceeded
    , LifecycleEventStatusFailed
    , LifecycleEventStatusSkipped
    , LifecycleEventStatusUnknown
    , fromLifecycleEventStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LifecycleEventStatus = LifecycleEventStatus'{fromLifecycleEventStatus
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern LifecycleEventStatusPending :: LifecycleEventStatus
pattern LifecycleEventStatusPending = LifecycleEventStatus' "Pending"

pattern LifecycleEventStatusInProgress :: LifecycleEventStatus
pattern LifecycleEventStatusInProgress = LifecycleEventStatus' "InProgress"

pattern LifecycleEventStatusSucceeded :: LifecycleEventStatus
pattern LifecycleEventStatusSucceeded = LifecycleEventStatus' "Succeeded"

pattern LifecycleEventStatusFailed :: LifecycleEventStatus
pattern LifecycleEventStatusFailed = LifecycleEventStatus' "Failed"

pattern LifecycleEventStatusSkipped :: LifecycleEventStatus
pattern LifecycleEventStatusSkipped = LifecycleEventStatus' "Skipped"

pattern LifecycleEventStatusUnknown :: LifecycleEventStatus
pattern LifecycleEventStatusUnknown = LifecycleEventStatus' "Unknown"

{-# COMPLETE 
  LifecycleEventStatusPending,

  LifecycleEventStatusInProgress,

  LifecycleEventStatusSucceeded,

  LifecycleEventStatusFailed,

  LifecycleEventStatusSkipped,

  LifecycleEventStatusUnknown,
  LifecycleEventStatus'
  #-}
