{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStateName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceStateName
  ( InstanceStateName
    ( InstanceStateName'
    , InstanceStateNamePending
    , InstanceStateNameRunning
    , InstanceStateNameShuttingDown
    , InstanceStateNameTerminated
    , InstanceStateNameStopping
    , InstanceStateNameStopped
    , fromInstanceStateName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InstanceStateName = InstanceStateName'{fromInstanceStateName
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern InstanceStateNamePending :: InstanceStateName
pattern InstanceStateNamePending = InstanceStateName' "pending"

pattern InstanceStateNameRunning :: InstanceStateName
pattern InstanceStateNameRunning = InstanceStateName' "running"

pattern InstanceStateNameShuttingDown :: InstanceStateName
pattern InstanceStateNameShuttingDown = InstanceStateName' "shutting-down"

pattern InstanceStateNameTerminated :: InstanceStateName
pattern InstanceStateNameTerminated = InstanceStateName' "terminated"

pattern InstanceStateNameStopping :: InstanceStateName
pattern InstanceStateNameStopping = InstanceStateName' "stopping"

pattern InstanceStateNameStopped :: InstanceStateName
pattern InstanceStateNameStopped = InstanceStateName' "stopped"

{-# COMPLETE 
  InstanceStateNamePending,

  InstanceStateNameRunning,

  InstanceStateNameShuttingDown,

  InstanceStateNameTerminated,

  InstanceStateNameStopping,

  InstanceStateNameStopped,
  InstanceStateName'
  #-}
