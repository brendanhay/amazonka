{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ClusterState
  ( ClusterState
    ( ClusterState'
    , ClusterStateStarting
    , ClusterStateBootstrapping
    , ClusterStateRunning
    , ClusterStateWaiting
    , ClusterStateTerminating
    , ClusterStateTerminated
    , ClusterStateTerminatedWithErrors
    , fromClusterState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ClusterState = ClusterState'{fromClusterState :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern ClusterStateStarting :: ClusterState
pattern ClusterStateStarting = ClusterState' "STARTING"

pattern ClusterStateBootstrapping :: ClusterState
pattern ClusterStateBootstrapping = ClusterState' "BOOTSTRAPPING"

pattern ClusterStateRunning :: ClusterState
pattern ClusterStateRunning = ClusterState' "RUNNING"

pattern ClusterStateWaiting :: ClusterState
pattern ClusterStateWaiting = ClusterState' "WAITING"

pattern ClusterStateTerminating :: ClusterState
pattern ClusterStateTerminating = ClusterState' "TERMINATING"

pattern ClusterStateTerminated :: ClusterState
pattern ClusterStateTerminated = ClusterState' "TERMINATED"

pattern ClusterStateTerminatedWithErrors :: ClusterState
pattern ClusterStateTerminatedWithErrors = ClusterState' "TERMINATED_WITH_ERRORS"

{-# COMPLETE 
  ClusterStateStarting,

  ClusterStateBootstrapping,

  ClusterStateRunning,

  ClusterStateWaiting,

  ClusterStateTerminating,

  ClusterStateTerminated,

  ClusterStateTerminatedWithErrors,
  ClusterState'
  #-}
