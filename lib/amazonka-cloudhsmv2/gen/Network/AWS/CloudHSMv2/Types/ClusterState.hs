-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.ClusterState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.ClusterState
  ( ClusterState
      ( ClusterState',
        CSActive,
        CSCreateInProgress,
        CSDegraded,
        CSDeleteInProgress,
        CSDeleted,
        CSInitializeInProgress,
        CSInitialized,
        CSUninitialized,
        CSUpdateInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ClusterState = ClusterState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CSActive :: ClusterState
pattern CSActive = ClusterState' "ACTIVE"

pattern CSCreateInProgress :: ClusterState
pattern CSCreateInProgress = ClusterState' "CREATE_IN_PROGRESS"

pattern CSDegraded :: ClusterState
pattern CSDegraded = ClusterState' "DEGRADED"

pattern CSDeleteInProgress :: ClusterState
pattern CSDeleteInProgress = ClusterState' "DELETE_IN_PROGRESS"

pattern CSDeleted :: ClusterState
pattern CSDeleted = ClusterState' "DELETED"

pattern CSInitializeInProgress :: ClusterState
pattern CSInitializeInProgress = ClusterState' "INITIALIZE_IN_PROGRESS"

pattern CSInitialized :: ClusterState
pattern CSInitialized = ClusterState' "INITIALIZED"

pattern CSUninitialized :: ClusterState
pattern CSUninitialized = ClusterState' "UNINITIALIZED"

pattern CSUpdateInProgress :: ClusterState
pattern CSUpdateInProgress = ClusterState' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  CSActive,
  CSCreateInProgress,
  CSDegraded,
  CSDeleteInProgress,
  CSDeleted,
  CSInitializeInProgress,
  CSInitialized,
  CSUninitialized,
  CSUpdateInProgress,
  ClusterState'
  #-}
