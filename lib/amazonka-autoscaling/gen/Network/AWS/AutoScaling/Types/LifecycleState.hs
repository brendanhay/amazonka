{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.LifecycleState
  ( LifecycleState
    ( LifecycleState'
    , LifecycleStatePending
    , LifecycleStatePendingWait
    , LifecycleStatePendingProceed
    , LifecycleStateQuarantined
    , LifecycleStateInService
    , LifecycleStateTerminating
    , LifecycleStateTerminatingWait
    , LifecycleStateTerminatingProceed
    , LifecycleStateTerminated
    , LifecycleStateDetaching
    , LifecycleStateDetached
    , LifecycleStateEnteringStandby
    , LifecycleStateStandby
    , fromLifecycleState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LifecycleState = LifecycleState'{fromLifecycleState ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern LifecycleStatePending :: LifecycleState
pattern LifecycleStatePending = LifecycleState' "Pending"

pattern LifecycleStatePendingWait :: LifecycleState
pattern LifecycleStatePendingWait = LifecycleState' "Pending:Wait"

pattern LifecycleStatePendingProceed :: LifecycleState
pattern LifecycleStatePendingProceed = LifecycleState' "Pending:Proceed"

pattern LifecycleStateQuarantined :: LifecycleState
pattern LifecycleStateQuarantined = LifecycleState' "Quarantined"

pattern LifecycleStateInService :: LifecycleState
pattern LifecycleStateInService = LifecycleState' "InService"

pattern LifecycleStateTerminating :: LifecycleState
pattern LifecycleStateTerminating = LifecycleState' "Terminating"

pattern LifecycleStateTerminatingWait :: LifecycleState
pattern LifecycleStateTerminatingWait = LifecycleState' "Terminating:Wait"

pattern LifecycleStateTerminatingProceed :: LifecycleState
pattern LifecycleStateTerminatingProceed = LifecycleState' "Terminating:Proceed"

pattern LifecycleStateTerminated :: LifecycleState
pattern LifecycleStateTerminated = LifecycleState' "Terminated"

pattern LifecycleStateDetaching :: LifecycleState
pattern LifecycleStateDetaching = LifecycleState' "Detaching"

pattern LifecycleStateDetached :: LifecycleState
pattern LifecycleStateDetached = LifecycleState' "Detached"

pattern LifecycleStateEnteringStandby :: LifecycleState
pattern LifecycleStateEnteringStandby = LifecycleState' "EnteringStandby"

pattern LifecycleStateStandby :: LifecycleState
pattern LifecycleStateStandby = LifecycleState' "Standby"

{-# COMPLETE 
  LifecycleStatePending,

  LifecycleStatePendingWait,

  LifecycleStatePendingProceed,

  LifecycleStateQuarantined,

  LifecycleStateInService,

  LifecycleStateTerminating,

  LifecycleStateTerminatingWait,

  LifecycleStateTerminatingProceed,

  LifecycleStateTerminated,

  LifecycleStateDetaching,

  LifecycleStateDetached,

  LifecycleStateEnteringStandby,

  LifecycleStateStandby,
  LifecycleState'
  #-}
