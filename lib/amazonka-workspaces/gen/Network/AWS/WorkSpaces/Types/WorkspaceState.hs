{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceState
  ( WorkspaceState
      ( WorkspaceState',
        WorkspaceStatePending,
        WorkspaceStateAvailable,
        WorkspaceStateImpaired,
        WorkspaceStateUnhealthy,
        WorkspaceStateRebooting,
        WorkspaceStateStarting,
        WorkspaceStateRebuilding,
        WorkspaceStateRestoring,
        WorkspaceStateMaintenance,
        WorkspaceStateAdminMaintenance,
        WorkspaceStateTerminating,
        WorkspaceStateTerminated,
        WorkspaceStateSuspended,
        WorkspaceStateUpdating,
        WorkspaceStateStopping,
        WorkspaceStateStopped,
        WorkspaceStateError,
        fromWorkspaceState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype WorkspaceState = WorkspaceState'
  { fromWorkspaceState ::
      Core.Text
  }
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

pattern WorkspaceStatePending :: WorkspaceState
pattern WorkspaceStatePending = WorkspaceState' "PENDING"

pattern WorkspaceStateAvailable :: WorkspaceState
pattern WorkspaceStateAvailable = WorkspaceState' "AVAILABLE"

pattern WorkspaceStateImpaired :: WorkspaceState
pattern WorkspaceStateImpaired = WorkspaceState' "IMPAIRED"

pattern WorkspaceStateUnhealthy :: WorkspaceState
pattern WorkspaceStateUnhealthy = WorkspaceState' "UNHEALTHY"

pattern WorkspaceStateRebooting :: WorkspaceState
pattern WorkspaceStateRebooting = WorkspaceState' "REBOOTING"

pattern WorkspaceStateStarting :: WorkspaceState
pattern WorkspaceStateStarting = WorkspaceState' "STARTING"

pattern WorkspaceStateRebuilding :: WorkspaceState
pattern WorkspaceStateRebuilding = WorkspaceState' "REBUILDING"

pattern WorkspaceStateRestoring :: WorkspaceState
pattern WorkspaceStateRestoring = WorkspaceState' "RESTORING"

pattern WorkspaceStateMaintenance :: WorkspaceState
pattern WorkspaceStateMaintenance = WorkspaceState' "MAINTENANCE"

pattern WorkspaceStateAdminMaintenance :: WorkspaceState
pattern WorkspaceStateAdminMaintenance = WorkspaceState' "ADMIN_MAINTENANCE"

pattern WorkspaceStateTerminating :: WorkspaceState
pattern WorkspaceStateTerminating = WorkspaceState' "TERMINATING"

pattern WorkspaceStateTerminated :: WorkspaceState
pattern WorkspaceStateTerminated = WorkspaceState' "TERMINATED"

pattern WorkspaceStateSuspended :: WorkspaceState
pattern WorkspaceStateSuspended = WorkspaceState' "SUSPENDED"

pattern WorkspaceStateUpdating :: WorkspaceState
pattern WorkspaceStateUpdating = WorkspaceState' "UPDATING"

pattern WorkspaceStateStopping :: WorkspaceState
pattern WorkspaceStateStopping = WorkspaceState' "STOPPING"

pattern WorkspaceStateStopped :: WorkspaceState
pattern WorkspaceStateStopped = WorkspaceState' "STOPPED"

pattern WorkspaceStateError :: WorkspaceState
pattern WorkspaceStateError = WorkspaceState' "ERROR"

{-# COMPLETE
  WorkspaceStatePending,
  WorkspaceStateAvailable,
  WorkspaceStateImpaired,
  WorkspaceStateUnhealthy,
  WorkspaceStateRebooting,
  WorkspaceStateStarting,
  WorkspaceStateRebuilding,
  WorkspaceStateRestoring,
  WorkspaceStateMaintenance,
  WorkspaceStateAdminMaintenance,
  WorkspaceStateTerminating,
  WorkspaceStateTerminated,
  WorkspaceStateSuspended,
  WorkspaceStateUpdating,
  WorkspaceStateStopping,
  WorkspaceStateStopped,
  WorkspaceStateError,
  WorkspaceState'
  #-}
