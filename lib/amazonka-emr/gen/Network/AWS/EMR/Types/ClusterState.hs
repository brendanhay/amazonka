{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterState
  ( ClusterState
      ( ClusterState',
        CSBootstrapping,
        CSRunning,
        CSStarting,
        CSTerminated,
        CSTerminatedWithErrors,
        CSTerminating,
        CSWaiting
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

pattern CSBootstrapping :: ClusterState
pattern CSBootstrapping = ClusterState' "BOOTSTRAPPING"

pattern CSRunning :: ClusterState
pattern CSRunning = ClusterState' "RUNNING"

pattern CSStarting :: ClusterState
pattern CSStarting = ClusterState' "STARTING"

pattern CSTerminated :: ClusterState
pattern CSTerminated = ClusterState' "TERMINATED"

pattern CSTerminatedWithErrors :: ClusterState
pattern CSTerminatedWithErrors = ClusterState' "TERMINATED_WITH_ERRORS"

pattern CSTerminating :: ClusterState
pattern CSTerminating = ClusterState' "TERMINATING"

pattern CSWaiting :: ClusterState
pattern CSWaiting = ClusterState' "WAITING"

{-# COMPLETE
  CSBootstrapping,
  CSRunning,
  CSStarting,
  CSTerminated,
  CSTerminatedWithErrors,
  CSTerminating,
  CSWaiting,
  ClusterState'
  #-}
