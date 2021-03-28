{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceGroupState
  ( InstanceGroupState
    ( InstanceGroupState'
    , InstanceGroupStateProvisioning
    , InstanceGroupStateBootstrapping
    , InstanceGroupStateRunning
    , InstanceGroupStateReconfiguring
    , InstanceGroupStateResizing
    , InstanceGroupStateSuspended
    , InstanceGroupStateTerminating
    , InstanceGroupStateTerminated
    , InstanceGroupStateArrested
    , InstanceGroupStateShuttingDown
    , InstanceGroupStateEnded
    , fromInstanceGroupState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InstanceGroupState = InstanceGroupState'{fromInstanceGroupState
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern InstanceGroupStateProvisioning :: InstanceGroupState
pattern InstanceGroupStateProvisioning = InstanceGroupState' "PROVISIONING"

pattern InstanceGroupStateBootstrapping :: InstanceGroupState
pattern InstanceGroupStateBootstrapping = InstanceGroupState' "BOOTSTRAPPING"

pattern InstanceGroupStateRunning :: InstanceGroupState
pattern InstanceGroupStateRunning = InstanceGroupState' "RUNNING"

pattern InstanceGroupStateReconfiguring :: InstanceGroupState
pattern InstanceGroupStateReconfiguring = InstanceGroupState' "RECONFIGURING"

pattern InstanceGroupStateResizing :: InstanceGroupState
pattern InstanceGroupStateResizing = InstanceGroupState' "RESIZING"

pattern InstanceGroupStateSuspended :: InstanceGroupState
pattern InstanceGroupStateSuspended = InstanceGroupState' "SUSPENDED"

pattern InstanceGroupStateTerminating :: InstanceGroupState
pattern InstanceGroupStateTerminating = InstanceGroupState' "TERMINATING"

pattern InstanceGroupStateTerminated :: InstanceGroupState
pattern InstanceGroupStateTerminated = InstanceGroupState' "TERMINATED"

pattern InstanceGroupStateArrested :: InstanceGroupState
pattern InstanceGroupStateArrested = InstanceGroupState' "ARRESTED"

pattern InstanceGroupStateShuttingDown :: InstanceGroupState
pattern InstanceGroupStateShuttingDown = InstanceGroupState' "SHUTTING_DOWN"

pattern InstanceGroupStateEnded :: InstanceGroupState
pattern InstanceGroupStateEnded = InstanceGroupState' "ENDED"

{-# COMPLETE 
  InstanceGroupStateProvisioning,

  InstanceGroupStateBootstrapping,

  InstanceGroupStateRunning,

  InstanceGroupStateReconfiguring,

  InstanceGroupStateResizing,

  InstanceGroupStateSuspended,

  InstanceGroupStateTerminating,

  InstanceGroupStateTerminated,

  InstanceGroupStateArrested,

  InstanceGroupStateShuttingDown,

  InstanceGroupStateEnded,
  InstanceGroupState'
  #-}
