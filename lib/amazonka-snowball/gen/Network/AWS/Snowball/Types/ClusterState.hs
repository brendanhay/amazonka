{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ClusterState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ClusterState
  ( ClusterState
      ( ClusterState',
        ClusterStateAwaitingQuorum,
        ClusterStatePending,
        ClusterStateInUse,
        ClusterStateComplete,
        ClusterStateCancelled,
        fromClusterState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ClusterState = ClusterState' {fromClusterState :: Core.Text}
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

pattern ClusterStateAwaitingQuorum :: ClusterState
pattern ClusterStateAwaitingQuorum = ClusterState' "AwaitingQuorum"

pattern ClusterStatePending :: ClusterState
pattern ClusterStatePending = ClusterState' "Pending"

pattern ClusterStateInUse :: ClusterState
pattern ClusterStateInUse = ClusterState' "InUse"

pattern ClusterStateComplete :: ClusterState
pattern ClusterStateComplete = ClusterState' "Complete"

pattern ClusterStateCancelled :: ClusterState
pattern ClusterStateCancelled = ClusterState' "Cancelled"

{-# COMPLETE
  ClusterStateAwaitingQuorum,
  ClusterStatePending,
  ClusterStateInUse,
  ClusterStateComplete,
  ClusterStateCancelled,
  ClusterState'
  #-}
