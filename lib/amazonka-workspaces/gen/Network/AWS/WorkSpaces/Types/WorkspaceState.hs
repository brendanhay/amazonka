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
        WSAdminMaintenance,
        WSAvailable,
        WSError,
        WSImpaired,
        WSMaintenance,
        WSPending,
        WSRebooting,
        WSRebuilding,
        WSRestoring,
        WSStarting,
        WSStopped,
        WSStopping,
        WSSuspended,
        WSTerminated,
        WSTerminating,
        WSUnhealthy,
        WSUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype WorkspaceState = WorkspaceState' Lude.Text
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

pattern WSAdminMaintenance :: WorkspaceState
pattern WSAdminMaintenance = WorkspaceState' "ADMIN_MAINTENANCE"

pattern WSAvailable :: WorkspaceState
pattern WSAvailable = WorkspaceState' "AVAILABLE"

pattern WSError :: WorkspaceState
pattern WSError = WorkspaceState' "ERROR"

pattern WSImpaired :: WorkspaceState
pattern WSImpaired = WorkspaceState' "IMPAIRED"

pattern WSMaintenance :: WorkspaceState
pattern WSMaintenance = WorkspaceState' "MAINTENANCE"

pattern WSPending :: WorkspaceState
pattern WSPending = WorkspaceState' "PENDING"

pattern WSRebooting :: WorkspaceState
pattern WSRebooting = WorkspaceState' "REBOOTING"

pattern WSRebuilding :: WorkspaceState
pattern WSRebuilding = WorkspaceState' "REBUILDING"

pattern WSRestoring :: WorkspaceState
pattern WSRestoring = WorkspaceState' "RESTORING"

pattern WSStarting :: WorkspaceState
pattern WSStarting = WorkspaceState' "STARTING"

pattern WSStopped :: WorkspaceState
pattern WSStopped = WorkspaceState' "STOPPED"

pattern WSStopping :: WorkspaceState
pattern WSStopping = WorkspaceState' "STOPPING"

pattern WSSuspended :: WorkspaceState
pattern WSSuspended = WorkspaceState' "SUSPENDED"

pattern WSTerminated :: WorkspaceState
pattern WSTerminated = WorkspaceState' "TERMINATED"

pattern WSTerminating :: WorkspaceState
pattern WSTerminating = WorkspaceState' "TERMINATING"

pattern WSUnhealthy :: WorkspaceState
pattern WSUnhealthy = WorkspaceState' "UNHEALTHY"

pattern WSUpdating :: WorkspaceState
pattern WSUpdating = WorkspaceState' "UPDATING"

{-# COMPLETE
  WSAdminMaintenance,
  WSAvailable,
  WSError,
  WSImpaired,
  WSMaintenance,
  WSPending,
  WSRebooting,
  WSRebuilding,
  WSRestoring,
  WSStarting,
  WSStopped,
  WSStopping,
  WSSuspended,
  WSTerminated,
  WSTerminating,
  WSUnhealthy,
  WSUpdating,
  WorkspaceState'
  #-}
