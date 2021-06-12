{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.UpdateServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a server.
--
-- This operation is synchronous.
module Network.AWS.OpsWorksCM.UpdateServer
  ( -- * Creating a Request
    UpdateServer (..),
    newUpdateServer,

    -- * Request Lenses
    updateServer_preferredBackupWindow,
    updateServer_disableAutomatedBackup,
    updateServer_preferredMaintenanceWindow,
    updateServer_backupRetentionCount,
    updateServer_serverName,

    -- * Destructuring the Response
    UpdateServerResponse (..),
    newUpdateServerResponse,

    -- * Response Lenses
    updateServerResponse_server,
    updateServerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateServer' smart constructor.
data UpdateServer = UpdateServer'
  { preferredBackupWindow :: Core.Maybe Core.Text,
    -- | Setting DisableAutomatedBackup to @true@ disables automated or scheduled
    -- backups. Automated backups are enabled by default.
    disableAutomatedBackup :: Core.Maybe Core.Bool,
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | Sets the number of automated backups that you want to keep.
    backupRetentionCount :: Core.Maybe Core.Int,
    -- | The name of the server to update.
    serverName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredBackupWindow', 'updateServer_preferredBackupWindow' - Undocumented member.
--
-- 'disableAutomatedBackup', 'updateServer_disableAutomatedBackup' - Setting DisableAutomatedBackup to @true@ disables automated or scheduled
-- backups. Automated backups are enabled by default.
--
-- 'preferredMaintenanceWindow', 'updateServer_preferredMaintenanceWindow' - Undocumented member.
--
-- 'backupRetentionCount', 'updateServer_backupRetentionCount' - Sets the number of automated backups that you want to keep.
--
-- 'serverName', 'updateServer_serverName' - The name of the server to update.
newUpdateServer ::
  -- | 'serverName'
  Core.Text ->
  UpdateServer
newUpdateServer pServerName_ =
  UpdateServer'
    { preferredBackupWindow = Core.Nothing,
      disableAutomatedBackup = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      backupRetentionCount = Core.Nothing,
      serverName = pServerName_
    }

-- | Undocumented member.
updateServer_preferredBackupWindow :: Lens.Lens' UpdateServer (Core.Maybe Core.Text)
updateServer_preferredBackupWindow = Lens.lens (\UpdateServer' {preferredBackupWindow} -> preferredBackupWindow) (\s@UpdateServer' {} a -> s {preferredBackupWindow = a} :: UpdateServer)

-- | Setting DisableAutomatedBackup to @true@ disables automated or scheduled
-- backups. Automated backups are enabled by default.
updateServer_disableAutomatedBackup :: Lens.Lens' UpdateServer (Core.Maybe Core.Bool)
updateServer_disableAutomatedBackup = Lens.lens (\UpdateServer' {disableAutomatedBackup} -> disableAutomatedBackup) (\s@UpdateServer' {} a -> s {disableAutomatedBackup = a} :: UpdateServer)

-- | Undocumented member.
updateServer_preferredMaintenanceWindow :: Lens.Lens' UpdateServer (Core.Maybe Core.Text)
updateServer_preferredMaintenanceWindow = Lens.lens (\UpdateServer' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@UpdateServer' {} a -> s {preferredMaintenanceWindow = a} :: UpdateServer)

-- | Sets the number of automated backups that you want to keep.
updateServer_backupRetentionCount :: Lens.Lens' UpdateServer (Core.Maybe Core.Int)
updateServer_backupRetentionCount = Lens.lens (\UpdateServer' {backupRetentionCount} -> backupRetentionCount) (\s@UpdateServer' {} a -> s {backupRetentionCount = a} :: UpdateServer)

-- | The name of the server to update.
updateServer_serverName :: Lens.Lens' UpdateServer Core.Text
updateServer_serverName = Lens.lens (\UpdateServer' {serverName} -> serverName) (\s@UpdateServer' {} a -> s {serverName = a} :: UpdateServer)

instance Core.AWSRequest UpdateServer where
  type AWSResponse UpdateServer = UpdateServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServerResponse'
            Core.<$> (x Core..?> "Server")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateServer

instance Core.NFData UpdateServer

instance Core.ToHeaders UpdateServer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorksCM_V2016_11_01.UpdateServer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateServer where
  toJSON UpdateServer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PreferredBackupWindow" Core..=)
              Core.<$> preferredBackupWindow,
            ("DisableAutomatedBackup" Core..=)
              Core.<$> disableAutomatedBackup,
            ("PreferredMaintenanceWindow" Core..=)
              Core.<$> preferredMaintenanceWindow,
            ("BackupRetentionCount" Core..=)
              Core.<$> backupRetentionCount,
            Core.Just ("ServerName" Core..= serverName)
          ]
      )

instance Core.ToPath UpdateServer where
  toPath = Core.const "/"

instance Core.ToQuery UpdateServer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateServerResponse' smart constructor.
data UpdateServerResponse = UpdateServerResponse'
  { -- | Contains the response to a @UpdateServer@ request.
    server :: Core.Maybe Server,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'server', 'updateServerResponse_server' - Contains the response to a @UpdateServer@ request.
--
-- 'httpStatus', 'updateServerResponse_httpStatus' - The response's http status code.
newUpdateServerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateServerResponse
newUpdateServerResponse pHttpStatus_ =
  UpdateServerResponse'
    { server = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the response to a @UpdateServer@ request.
updateServerResponse_server :: Lens.Lens' UpdateServerResponse (Core.Maybe Server)
updateServerResponse_server = Lens.lens (\UpdateServerResponse' {server} -> server) (\s@UpdateServerResponse' {} a -> s {server = a} :: UpdateServerResponse)

-- | The response's http status code.
updateServerResponse_httpStatus :: Lens.Lens' UpdateServerResponse Core.Int
updateServerResponse_httpStatus = Lens.lens (\UpdateServerResponse' {httpStatus} -> httpStatus) (\s@UpdateServerResponse' {} a -> s {httpStatus = a} :: UpdateServerResponse)

instance Core.NFData UpdateServerResponse
