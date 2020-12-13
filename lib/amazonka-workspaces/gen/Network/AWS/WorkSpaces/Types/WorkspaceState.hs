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
        WSPending,
        WSAvailable,
        WSImpaired,
        WSUnhealthy,
        WSRebooting,
        WSStarting,
        WSRebuilding,
        WSRestoring,
        WSMaintenance,
        WSAdminMaintenance,
        WSTerminating,
        WSTerminated,
        WSSuspended,
        WSUpdating,
        WSStopping,
        WSStopped,
        WSError
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

pattern WSPending :: WorkspaceState
pattern WSPending = WorkspaceState' "PENDING"

pattern WSAvailable :: WorkspaceState
pattern WSAvailable = WorkspaceState' "AVAILABLE"

pattern WSImpaired :: WorkspaceState
pattern WSImpaired = WorkspaceState' "IMPAIRED"

pattern WSUnhealthy :: WorkspaceState
pattern WSUnhealthy = WorkspaceState' "UNHEALTHY"

pattern WSRebooting :: WorkspaceState
pattern WSRebooting = WorkspaceState' "REBOOTING"

pattern WSStarting :: WorkspaceState
pattern WSStarting = WorkspaceState' "STARTING"

pattern WSRebuilding :: WorkspaceState
pattern WSRebuilding = WorkspaceState' "REBUILDING"

pattern WSRestoring :: WorkspaceState
pattern WSRestoring = WorkspaceState' "RESTORING"

pattern WSMaintenance :: WorkspaceState
pattern WSMaintenance = WorkspaceState' "MAINTENANCE"

pattern WSAdminMaintenance :: WorkspaceState
pattern WSAdminMaintenance = WorkspaceState' "ADMIN_MAINTENANCE"

pattern WSTerminating :: WorkspaceState
pattern WSTerminating = WorkspaceState' "TERMINATING"

pattern WSTerminated :: WorkspaceState
pattern WSTerminated = WorkspaceState' "TERMINATED"

pattern WSSuspended :: WorkspaceState
pattern WSSuspended = WorkspaceState' "SUSPENDED"

pattern WSUpdating :: WorkspaceState
pattern WSUpdating = WorkspaceState' "UPDATING"

pattern WSStopping :: WorkspaceState
pattern WSStopping = WorkspaceState' "STOPPING"

pattern WSStopped :: WorkspaceState
pattern WSStopped = WorkspaceState' "STOPPED"

pattern WSError :: WorkspaceState
pattern WSError = WorkspaceState' "ERROR"

{-# COMPLETE
  WSPending,
  WSAvailable,
  WSImpaired,
  WSUnhealthy,
  WSRebooting,
  WSStarting,
  WSRebuilding,
  WSRestoring,
  WSMaintenance,
  WSAdminMaintenance,
  WSTerminating,
  WSTerminated,
  WSSuspended,
  WSUpdating,
  WSStopping,
  WSStopped,
  WSError,
  WorkspaceState'
  #-}
