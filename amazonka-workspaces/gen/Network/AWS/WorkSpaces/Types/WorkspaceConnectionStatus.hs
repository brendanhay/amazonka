{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.ConnectionState

-- | Describes the connection status of a WorkSpace.
--
-- /See:/ 'newWorkspaceConnectionStatus' smart constructor.
data WorkspaceConnectionStatus = WorkspaceConnectionStatus'
  { -- | The connection state of the WorkSpace. The connection state is unknown
    -- if the WorkSpace is stopped.
    connectionState :: Prelude.Maybe ConnectionState,
    -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last known user connection.
    lastKnownUserConnectionTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The timestamp of the connection status check.
    connectionStateCheckTimestamp :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      workspaceId = Prelude.Nothing,
      lastKnownUserConnectionTimestamp =
        Prelude.Nothing,
      connectionStateCheckTimestamp = Prelude.Nothing
    }

-- | The connection state of the WorkSpace. The connection state is unknown
-- if the WorkSpace is stopped.
workspaceConnectionStatus_connectionState :: Lens.Lens' WorkspaceConnectionStatus (Prelude.Maybe ConnectionState)
workspaceConnectionStatus_connectionState = Lens.lens (\WorkspaceConnectionStatus' {connectionState} -> connectionState) (\s@WorkspaceConnectionStatus' {} a -> s {connectionState = a} :: WorkspaceConnectionStatus)

-- | The identifier of the WorkSpace.
workspaceConnectionStatus_workspaceId :: Lens.Lens' WorkspaceConnectionStatus (Prelude.Maybe Prelude.Text)
workspaceConnectionStatus_workspaceId = Lens.lens (\WorkspaceConnectionStatus' {workspaceId} -> workspaceId) (\s@WorkspaceConnectionStatus' {} a -> s {workspaceId = a} :: WorkspaceConnectionStatus)

-- | The timestamp of the last known user connection.
workspaceConnectionStatus_lastKnownUserConnectionTimestamp :: Lens.Lens' WorkspaceConnectionStatus (Prelude.Maybe Prelude.UTCTime)
workspaceConnectionStatus_lastKnownUserConnectionTimestamp = Lens.lens (\WorkspaceConnectionStatus' {lastKnownUserConnectionTimestamp} -> lastKnownUserConnectionTimestamp) (\s@WorkspaceConnectionStatus' {} a -> s {lastKnownUserConnectionTimestamp = a} :: WorkspaceConnectionStatus) Prelude.. Lens.mapping Prelude._Time

-- | The timestamp of the connection status check.
workspaceConnectionStatus_connectionStateCheckTimestamp :: Lens.Lens' WorkspaceConnectionStatus (Prelude.Maybe Prelude.UTCTime)
workspaceConnectionStatus_connectionStateCheckTimestamp = Lens.lens (\WorkspaceConnectionStatus' {connectionStateCheckTimestamp} -> connectionStateCheckTimestamp) (\s@WorkspaceConnectionStatus' {} a -> s {connectionStateCheckTimestamp = a} :: WorkspaceConnectionStatus) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON WorkspaceConnectionStatus where
  parseJSON =
    Prelude.withObject
      "WorkspaceConnectionStatus"
      ( \x ->
          WorkspaceConnectionStatus'
            Prelude.<$> (x Prelude..:? "ConnectionState")
            Prelude.<*> (x Prelude..:? "WorkspaceId")
            Prelude.<*> (x Prelude..:? "LastKnownUserConnectionTimestamp")
            Prelude.<*> (x Prelude..:? "ConnectionStateCheckTimestamp")
      )

instance Prelude.Hashable WorkspaceConnectionStatus

instance Prelude.NFData WorkspaceConnectionStatus
