{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.ClusterState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSMv2.Types.ClusterState
  ( ClusterState
    ( ClusterState'
    , ClusterStateCreateInProgress
    , ClusterStateUninitialized
    , ClusterStateInitializeInProgress
    , ClusterStateInitialized
    , ClusterStateActive
    , ClusterStateUpdateInProgress
    , ClusterStateDeleteInProgress
    , ClusterStateDeleted
    , ClusterStateDegraded
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

pattern ClusterStateCreateInProgress :: ClusterState
pattern ClusterStateCreateInProgress = ClusterState' "CREATE_IN_PROGRESS"

pattern ClusterStateUninitialized :: ClusterState
pattern ClusterStateUninitialized = ClusterState' "UNINITIALIZED"

pattern ClusterStateInitializeInProgress :: ClusterState
pattern ClusterStateInitializeInProgress = ClusterState' "INITIALIZE_IN_PROGRESS"

pattern ClusterStateInitialized :: ClusterState
pattern ClusterStateInitialized = ClusterState' "INITIALIZED"

pattern ClusterStateActive :: ClusterState
pattern ClusterStateActive = ClusterState' "ACTIVE"

pattern ClusterStateUpdateInProgress :: ClusterState
pattern ClusterStateUpdateInProgress = ClusterState' "UPDATE_IN_PROGRESS"

pattern ClusterStateDeleteInProgress :: ClusterState
pattern ClusterStateDeleteInProgress = ClusterState' "DELETE_IN_PROGRESS"

pattern ClusterStateDeleted :: ClusterState
pattern ClusterStateDeleted = ClusterState' "DELETED"

pattern ClusterStateDegraded :: ClusterState
pattern ClusterStateDegraded = ClusterState' "DEGRADED"

{-# COMPLETE 
  ClusterStateCreateInProgress,

  ClusterStateUninitialized,

  ClusterStateInitializeInProgress,

  ClusterStateInitialized,

  ClusterStateActive,

  ClusterStateUpdateInProgress,

  ClusterStateDeleteInProgress,

  ClusterStateDeleted,

  ClusterStateDegraded,
  ClusterState'
  #-}
