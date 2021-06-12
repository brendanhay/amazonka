{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkSpaces.Types.ConnectionState

-- | Describes the connection status of a WorkSpace.
--
-- /See:/ 'newWorkspaceConnectionStatus' smart constructor.
data WorkspaceConnectionStatus = WorkspaceConnectionStatus'
  { -- | The connection state of the WorkSpace. The connection state is unknown
    -- if the WorkSpace is stopped.
    connectionState :: Core.Maybe ConnectionState,
    -- | The identifier of the WorkSpace.
    workspaceId :: Core.Maybe Core.Text,
    -- | The timestamp of the last known user connection.
    lastKnownUserConnectionTimestamp :: Core.Maybe Core.POSIX,
    -- | The timestamp of the connection status check.
    connectionStateCheckTimestamp :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkspaceConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionState', 'workspaceConnectionStatus_connectionState' - The connection state of the WorkSpace. The connection state is unknown
-- if the WorkSpace is stopped.
--
-- 'workspaceId', 'workspaceConnectionStatus_workspaceId' - The identifier of the WorkSpace.
--
-- 'lastKnownUserConnectionTimestamp', 'workspaceConnectionStatus_lastKnownUserConnectionTimestamp' - The timestamp of the last known user connection.
--
-- 'connectionStateCheckTimestamp', 'workspaceConnectionStatus_connectionStateCheckTimestamp' - The timestamp of the connection status check.
newWorkspaceConnectionStatus ::
  WorkspaceConnectionStatus
newWorkspaceConnectionStatus =
  WorkspaceConnectionStatus'
    { connectionState =
        Core.Nothing,
      workspaceId = Core.Nothing,
      lastKnownUserConnectionTimestamp = Core.Nothing,
      connectionStateCheckTimestamp = Core.Nothing
    }

-- | The connection state of the WorkSpace. The connection state is unknown
-- if the WorkSpace is stopped.
workspaceConnectionStatus_connectionState :: Lens.Lens' WorkspaceConnectionStatus (Core.Maybe ConnectionState)
workspaceConnectionStatus_connectionState = Lens.lens (\WorkspaceConnectionStatus' {connectionState} -> connectionState) (\s@WorkspaceConnectionStatus' {} a -> s {connectionState = a} :: WorkspaceConnectionStatus)

-- | The identifier of the WorkSpace.
workspaceConnectionStatus_workspaceId :: Lens.Lens' WorkspaceConnectionStatus (Core.Maybe Core.Text)
workspaceConnectionStatus_workspaceId = Lens.lens (\WorkspaceConnectionStatus' {workspaceId} -> workspaceId) (\s@WorkspaceConnectionStatus' {} a -> s {workspaceId = a} :: WorkspaceConnectionStatus)

-- | The timestamp of the last known user connection.
workspaceConnectionStatus_lastKnownUserConnectionTimestamp :: Lens.Lens' WorkspaceConnectionStatus (Core.Maybe Core.UTCTime)
workspaceConnectionStatus_lastKnownUserConnectionTimestamp = Lens.lens (\WorkspaceConnectionStatus' {lastKnownUserConnectionTimestamp} -> lastKnownUserConnectionTimestamp) (\s@WorkspaceConnectionStatus' {} a -> s {lastKnownUserConnectionTimestamp = a} :: WorkspaceConnectionStatus) Core.. Lens.mapping Core._Time

-- | The timestamp of the connection status check.
workspaceConnectionStatus_connectionStateCheckTimestamp :: Lens.Lens' WorkspaceConnectionStatus (Core.Maybe Core.UTCTime)
workspaceConnectionStatus_connectionStateCheckTimestamp = Lens.lens (\WorkspaceConnectionStatus' {connectionStateCheckTimestamp} -> connectionStateCheckTimestamp) (\s@WorkspaceConnectionStatus' {} a -> s {connectionStateCheckTimestamp = a} :: WorkspaceConnectionStatus) Core.. Lens.mapping Core._Time

instance Core.FromJSON WorkspaceConnectionStatus where
  parseJSON =
    Core.withObject
      "WorkspaceConnectionStatus"
      ( \x ->
          WorkspaceConnectionStatus'
            Core.<$> (x Core..:? "ConnectionState")
            Core.<*> (x Core..:? "WorkspaceId")
            Core.<*> (x Core..:? "LastKnownUserConnectionTimestamp")
            Core.<*> (x Core..:? "ConnectionStateCheckTimestamp")
      )

instance Core.Hashable WorkspaceConnectionStatus

instance Core.NFData WorkspaceConnectionStatus
