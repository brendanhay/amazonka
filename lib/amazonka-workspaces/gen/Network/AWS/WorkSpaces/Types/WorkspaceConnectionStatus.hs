-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
  ( WorkspaceConnectionStatus (..),

    -- * Smart constructor
    mkWorkspaceConnectionStatus,

    -- * Lenses
    wcsLastKnownUserConnectionTimestamp,
    wcsConnectionStateCheckTimestamp,
    wcsWorkspaceId,
    wcsConnectionState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.ConnectionState

-- | Describes the connection status of a WorkSpace.
--
-- /See:/ 'mkWorkspaceConnectionStatus' smart constructor.
data WorkspaceConnectionStatus = WorkspaceConnectionStatus'
  { lastKnownUserConnectionTimestamp ::
      Lude.Maybe Lude.Timestamp,
    connectionStateCheckTimestamp ::
      Lude.Maybe Lude.Timestamp,
    workspaceId :: Lude.Maybe Lude.Text,
    connectionState ::
      Lude.Maybe ConnectionState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkspaceConnectionStatus' with the minimum fields required to make a request.
--
-- * 'connectionState' - The connection state of the WorkSpace. The connection state is unknown if the WorkSpace is stopped.
-- * 'connectionStateCheckTimestamp' - The timestamp of the connection status check.
-- * 'lastKnownUserConnectionTimestamp' - The timestamp of the last known user connection.
-- * 'workspaceId' - The identifier of the WorkSpace.
mkWorkspaceConnectionStatus ::
  WorkspaceConnectionStatus
mkWorkspaceConnectionStatus =
  WorkspaceConnectionStatus'
    { lastKnownUserConnectionTimestamp =
        Lude.Nothing,
      connectionStateCheckTimestamp = Lude.Nothing,
      workspaceId = Lude.Nothing,
      connectionState = Lude.Nothing
    }

-- | The timestamp of the last known user connection.
--
-- /Note:/ Consider using 'lastKnownUserConnectionTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcsLastKnownUserConnectionTimestamp :: Lens.Lens' WorkspaceConnectionStatus (Lude.Maybe Lude.Timestamp)
wcsLastKnownUserConnectionTimestamp = Lens.lens (lastKnownUserConnectionTimestamp :: WorkspaceConnectionStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastKnownUserConnectionTimestamp = a} :: WorkspaceConnectionStatus)
{-# DEPRECATED wcsLastKnownUserConnectionTimestamp "Use generic-lens or generic-optics with 'lastKnownUserConnectionTimestamp' instead." #-}

-- | The timestamp of the connection status check.
--
-- /Note:/ Consider using 'connectionStateCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcsConnectionStateCheckTimestamp :: Lens.Lens' WorkspaceConnectionStatus (Lude.Maybe Lude.Timestamp)
wcsConnectionStateCheckTimestamp = Lens.lens (connectionStateCheckTimestamp :: WorkspaceConnectionStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {connectionStateCheckTimestamp = a} :: WorkspaceConnectionStatus)
{-# DEPRECATED wcsConnectionStateCheckTimestamp "Use generic-lens or generic-optics with 'connectionStateCheckTimestamp' instead." #-}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcsWorkspaceId :: Lens.Lens' WorkspaceConnectionStatus (Lude.Maybe Lude.Text)
wcsWorkspaceId = Lens.lens (workspaceId :: WorkspaceConnectionStatus -> Lude.Maybe Lude.Text) (\s a -> s {workspaceId = a} :: WorkspaceConnectionStatus)
{-# DEPRECATED wcsWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

-- | The connection state of the WorkSpace. The connection state is unknown if the WorkSpace is stopped.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcsConnectionState :: Lens.Lens' WorkspaceConnectionStatus (Lude.Maybe ConnectionState)
wcsConnectionState = Lens.lens (connectionState :: WorkspaceConnectionStatus -> Lude.Maybe ConnectionState) (\s a -> s {connectionState = a} :: WorkspaceConnectionStatus)
{-# DEPRECATED wcsConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

instance Lude.FromJSON WorkspaceConnectionStatus where
  parseJSON =
    Lude.withObject
      "WorkspaceConnectionStatus"
      ( \x ->
          WorkspaceConnectionStatus'
            Lude.<$> (x Lude..:? "LastKnownUserConnectionTimestamp")
            Lude.<*> (x Lude..:? "ConnectionStateCheckTimestamp")
            Lude.<*> (x Lude..:? "WorkspaceId")
            Lude.<*> (x Lude..:? "ConnectionState")
      )
